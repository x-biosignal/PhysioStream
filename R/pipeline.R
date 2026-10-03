.pipeline_schema_version <- "1.0.0"
.pipeline_drop_audit_limit <- 1024L
.pipeline_latency_limit <- 100000L

.pipeline_abort <- function(message,
                            class = "PhysioStream_pipeline_error") {
  .stream_abort(message, class)
}

.pipeline_name <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .pipeline_abort(
      sprintf("`%s` must be one non-empty string", name),
      "PhysioStream_pipeline_validation_error"
    )
  }
  x
}

.pipeline_empty_latency <- function() {
  list(
    sequence_start = numeric(),
    sequence_end = numeric(),
    ingest_ns = numeric(),
    process_start_ns = numeric(),
    process_end_ns = numeric(),
    emit_ns = numeric(),
    queue_wait_ms = numeric(),
    processing_ms = numeric(),
    end_to_end_ms = numeric(),
    budget_exceeded = logical()
  )
}

.pipeline_empty_counters <- function() {
  list(
    accepted_chunks = 0,
    accepted_samples = 0,
    processed_chunks = 0,
    processed_samples = 0,
    emitted_chunks = 0,
    emitted_samples = 0,
    dropped_chunks = 0,
    dropped_samples = 0,
    sequence_last = 0,
    drop_audit_truncated = 0
  )
}

.pipeline_empty_state <- function(configuration, operations = list(),
                                  reset_count = 0) {
  list(
    configuration = configuration,
    operations = operations,
    queue = list(),
    counters = .pipeline_empty_counters(),
    latency = .pipeline_empty_latency(),
    drop_audit = list(),
    channel_names = NULL,
    timestamp_mode = NULL,
    last_timestamp = NULL,
    last_ingest_time_ns = NULL,
    reset_count = as.numeric(reset_count),
    pipeline_schema = .pipeline_schema_version
  )
}

.pipeline_assert <- function(pipeline) {
  if (!inherits(pipeline, "StreamPipeline") || !is.environment(pipeline)) {
    .pipeline_abort(
      "`pipeline` must be a StreamPipeline",
      "PhysioStream_pipeline_validation_error"
    )
  }
  .dsp_validate_state(pipeline$state)
  if (!is.list(pipeline$callbacks) ||
      length(pipeline$callbacks) != length(pipeline$state$operations) ||
      (length(pipeline$callbacks) &&
       any(!vapply(pipeline$callbacks, is.function, logical(1))))) {
    .pipeline_abort(
      "pipeline callback registry does not match its governed state",
      "PhysioStream_pipeline_state_error"
    )
  }
  if (!is.null(pipeline$source) &&
      !methods::is(pipeline$source, "StreamSource")) {
    .pipeline_abort(
      "pipeline source is not a StreamSource",
      "PhysioStream_pipeline_state_error"
    )
  }
  if (!is.logical(pipeline$busy) || length(pipeline$busy) != 1L ||
      is.na(pipeline$busy)) {
    .pipeline_abort(
      "pipeline runtime lock is invalid",
      "PhysioStream_pipeline_state_error"
    )
  }
  invisible(TRUE)
}

.pipeline_require_idle <- function(pipeline) {
  if (isTRUE(pipeline$busy)) {
    .pipeline_abort(
      "pipeline mutation is not reentrant",
      "PhysioStream_pipeline_state_error"
    )
  }
  invisible(TRUE)
}

.pipeline_publish <- function(pipeline, old_state, candidate) {
  if (!identical(pipeline$state$sha256, old_state$sha256)) {
    .pipeline_abort(
      "pipeline state changed during a transactional update",
      "PhysioStream_pipeline_state_error"
    )
  }
  candidate <- .dsp_seal_state(candidate)
  pipeline$state <- candidate
  invisible(pipeline)
}

.pipeline_state_bytes <- function(x, label, limit = .dsp_allocation_limit) {
  bad <- .dsp_runtime_path(x, label)
  if (!is.null(bad)) {
    .pipeline_abort(
      sprintf("unsupported runtime value at `%s`", bad),
      "PhysioStream_pipeline_state_error"
    )
  }
  object_path <- .pipeline_object_path(x, label)
  if (!is.null(object_path)) {
    .pipeline_abort(
      sprintf("non-plain object at `%s`", object_path),
      "PhysioStream_pipeline_state_error"
    )
  }
  payload <- tryCatch(
    serialize(x, NULL, version = 3L),
    error = function(e) NULL
  )
  if (is.null(payload) || length(payload) > limit) {
    .pipeline_abort(
      sprintf("`%s` exceeds the governed serialization ceiling", label),
      "PhysioStream_pipeline_resource_error"
    )
  }
  invisible(length(payload))
}

.pipeline_object_path <- function(x, path) {
  if (is.object(x)) {
    return(path)
  }
  if (is.list(x)) {
    names_x <- names(x)
    for (i in seq_along(x)) {
      label <- if (!is.null(names_x) && nzchar(names_x[[i]])) {
        names_x[[i]]
      } else {
        as.character(i)
      }
      bad <- .pipeline_object_path(x[[i]], paste0(path, "$", label))
      if (!is.null(bad)) {
        return(bad)
      }
    }
  }
  NULL
}

.pipeline_source_descriptor <- function(source) {
  if (is.null(source)) {
    return(NULL)
  }
  info <- streamInfo(source)
  list(
    class = class(source)[[1L]],
    name = info@name,
    type = info@type,
    channel_names = info@channel_names,
    nominal_srate = info@nominal_srate,
    dtype = info@dtype,
    source_id = info@source_id,
    clock_domain = info@clock_domain,
    schema = info@schema_version
  )
}

#' Construct a governed synchronous stream-processing pipeline
#'
#' A pipeline owns a bounded FIFO of whole chunks and an ordered callback
#' graph. Processing occurs synchronously on the caller's R thread. The runtime
#' environment is not portable; use [pipelineState()] for a hashed plain-state
#' snapshot.
#'
#' @param source Optional `StreamSource`. Caller-driven pipelines use `NULL`.
#' @param chunk_size Maximum samples per enqueued chunk.
#' @param queue_capacity Maximum number of whole chunks in the ingress FIFO.
#' @param backpressure Exact full-queue policy.
#' @param latency_budget_ms Finite positive empirical latency budget.
#' @return A `StreamPipeline` environment.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 8L)
#' pipelineState(pipeline)$configuration$chunk_size
#' @export
streamPipeline <- function(
    source = NULL,
    chunk_size = 32L,
    queue_capacity = 64L,
    backpressure = c("error", "drop_oldest", "drop_newest"),
    latency_budget_ms = 50) {
  chunk_size <- .dsp_scalar(
    chunk_size, "chunk_size", lower = 1, upper = 1048576, integer = TRUE
  )
  queue_capacity <- .dsp_scalar(
    queue_capacity, "queue_capacity", lower = 1, upper = 1048576,
    integer = TRUE
  )
  if (length(backpressure) > 1L) {
    backpressure <- backpressure[[1L]]
  }
  backpressure <- .dsp_enum(
    backpressure, c("error", "drop_oldest", "drop_newest"), "backpressure"
  )
  latency_budget_ms <- .dsp_scalar(
    latency_budget_ms, "latency_budget_ms", lower = 0, lower_open = TRUE
  )
  if (!is.null(source) && !methods::is(source, "StreamSource")) {
    .pipeline_abort(
      "`source` must be NULL or a StreamSource",
      "PhysioStream_pipeline_validation_error"
    )
  }

  configuration <- list(
    chunk_size = chunk_size,
    queue_capacity = queue_capacity,
    backpressure = backpressure,
    latency_budget_ms = latency_budget_ms,
    source = .pipeline_source_descriptor(source)
  )
  object <- new.env(parent = emptyenv())
  object$state <- .dsp_seal_state(.pipeline_empty_state(configuration))
  object$callbacks <- list()
  object$source <- source
  object$busy <- FALSE
  class(object) <- c("StreamPipeline", "PipelineRuntime")
  object
}

#' Register a causal per-chunk operation
#'
#' The callback is invoked as `callback(chunk, state, context)` and must return
#' a list containing `output`, `state`, `events`, and `diagnostics`. Operation
#' states are committed only after the full graph succeeds.
#'
#' @param pipeline A `StreamPipeline`.
#' @param callback A callback function or a `PipelineOperation` descriptor.
#' @param state Initial bounded plain-list state for a custom callback.
#' @param name Unique non-empty operation name.
#' @param kind Exact operation kind.
#' @return `pipeline`, invisibly.
#' @examples
#' sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
#'               dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
#' pipeline <- streamPipeline(chunk_size = 8L)
#' onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
#' length(pipelineState(pipeline)$operations)
#' @export
onChunk <- function(pipeline, callback, state = NULL, name = NULL,
                    kind = "filter") {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  old <- .dsp_deep_copy(pipeline$state)
  if (length(old$queue)) {
    .pipeline_abort(
      "operations cannot be registered while chunks are queued",
      "PhysioStream_pipeline_state_error"
    )
  }

  descriptor <- list(type = "callback", configuration = list())
  if (inherits(callback, "PipelineOperation")) {
    operation <- callback
    callback <- operation$callback
    if (is.null(state)) {
      state <- operation$state
    }
    if (is.null(name)) {
      name <- operation$name
    }
    if (identical(kind, "filter") && !is.null(operation$kind)) {
      kind <- operation$kind
    }
    descriptor <- operation$descriptor
  }
  if (!is.function(callback)) {
    .pipeline_abort(
      "`callback` must be a function or PipelineOperation",
      "PhysioStream_pipeline_validation_error"
    )
  }
  if (is.null(state)) {
    state <- list()
  }
  if (!is.list(state) || is.object(state)) {
    .pipeline_abort(
      "callback `state` must be a plain list",
      "PhysioStream_pipeline_validation_error"
    )
  }
  name <- .pipeline_name(name, "name")
  kind <- .dsp_enum(kind, c("filter", "feature", "detector"), "kind")
  if (name %in% vapply(old$operations, `[[`, character(1), "name")) {
    .pipeline_abort(
      sprintf("operation name `%s` is already registered", name),
      "PhysioStream_pipeline_validation_error"
    )
  }
  .pipeline_state_bytes(
    state, paste0("operation$", name, "$state"), .dsp_state_limit
  )
  entry <- list(
    name = name,
    kind = kind,
    type = descriptor$type,
    configuration = descriptor$configuration,
    state = .dsp_deep_copy(state),
    initial_state = .dsp_deep_copy(state)
  )
  candidate <- old
  candidate$operations[[length(candidate$operations) + 1L]] <- entry
  candidate <- .dsp_seal_state(candidate)
  if (!identical(pipeline$state$sha256, old$sha256)) {
    .pipeline_abort(
      "pipeline state changed during operation registration",
      "PhysioStream_pipeline_state_error"
    )
  }
  pipeline$state <- candidate
  pipeline$callbacks[[length(pipeline$callbacks) + 1L]] <- callback
  invisible(pipeline)
}

.pipeline_validate_ingest_time <- function(x, last = NULL) {
  if (is.null(x)) {
    x <- cpp_monotonic_ns()
  }
  x <- .dsp_scalar(x, "ingest_time_ns", lower = 0)
  if (!is.null(last) && x < last) {
    .pipeline_abort(
      "`ingest_time_ns` decreased within the monotonic clock domain",
      "PhysioStream_pipeline_timing_error"
    )
  }
  x
}

.pipeline_add_drop <- function(state, record) {
  if (length(state$drop_audit) >= .pipeline_drop_audit_limit) {
    state$drop_audit <- state$drop_audit[-1L]
    state$counters$drop_audit_truncated <-
      state$counters$drop_audit_truncated + 1
  }
  state$drop_audit[[length(state$drop_audit) + 1L]] <- record
  state
}

.pipeline_validate_enqueue <- function(state, samples, timestamps) {
  samples <- .dsp_normalize_matrix(samples)
  if (nrow(samples) == 0L) {
    if (!is.null(timestamps) &&
        (!is.numeric(timestamps) || length(timestamps) != 0L)) {
      .pipeline_abort(
        "`timestamps` must be NULL or empty for an empty chunk",
        "PhysioStream_pipeline_timestamp_error"
      )
    }
    return(list(empty = TRUE, samples = samples))
  }
  if (nrow(samples) > state$configuration$chunk_size) {
    .pipeline_abort(
      "chunk rows exceed the configured `chunk_size`",
      "PhysioStream_pipeline_validation_error"
    )
  }
  channel_names <- .dsp_channel_names(samples)
  if (!is.null(state$channel_names) &&
      !identical(channel_names, state$channel_names)) {
    .pipeline_abort(
      "input channel count, order, or names changed",
      "PhysioStream_pipeline_channel_error"
    )
  }
  mode <- if (is.null(timestamps)) "absent" else "provided"
  if (!is.null(state$timestamp_mode) &&
      !identical(mode, state$timestamp_mode)) {
    .pipeline_abort(
      "timestamp presence changed after the pipeline input contract bound",
      "PhysioStream_pipeline_timestamp_error"
    )
  }
  if (!is.null(timestamps)) {
    if (is.factor(timestamps) || !is.numeric(timestamps) ||
        !is.null(dim(timestamps)) || length(timestamps) != nrow(samples) ||
        any(!is.finite(timestamps)) ||
        (length(timestamps) > 1L && any(diff(timestamps) <= 0))) {
      .pipeline_abort(
        "`timestamps` must be finite, strictly increasing, and row-matched",
        "PhysioStream_pipeline_timestamp_error"
      )
    }
    timestamps <- as.numeric(timestamps)
    if (!is.null(state$last_timestamp) &&
        timestamps[[1L]] <= state$last_timestamp) {
      .pipeline_abort(
        "timestamps must increase across enqueued chunks",
        "PhysioStream_pipeline_timestamp_error"
      )
    }
  }
  list(
    empty = FALSE,
    samples = samples,
    timestamps = timestamps,
    channel_names = channel_names,
    timestamp_mode = mode
  )
}

#' Enqueue one bounded input chunk
#'
#' Full-queue behavior is governed by the pipeline's exact backpressure policy.
#' Empty chunks are no-ops and consume neither capacity nor sequence identity.
#'
#' @param pipeline A `StreamPipeline`.
#' @param samples Finite sample-by-channel values.
#' @param timestamps Optional strictly increasing row-matched timestamps.
#' @param ingest_time_ns Optional process-local monotonic nanosecond stamp.
#' @return `pipeline`, invisibly.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 4L)
#' x <- matrix(1:8, 4L, 2L, dimnames = list(NULL, c("a", "b")))
#' pipelineEnqueue(pipeline, x, c(1, 2, 3, 4))
#' pipelineState(pipeline)$channel_names
#' @export
pipelineEnqueue <- function(pipeline, samples, timestamps = NULL,
                            ingest_time_ns = NULL) {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  old <- .dsp_deep_copy(pipeline$state)
  chunk <- .pipeline_validate_enqueue(old, samples, timestamps)
  if (chunk$empty) {
    return(invisible(pipeline))
  }
  ingest_time_ns <- .pipeline_validate_ingest_time(
    ingest_time_ns, old$last_ingest_time_ns
  )
  candidate <- old
  full <- length(candidate$queue) >= candidate$configuration$queue_capacity
  policy <- candidate$configuration$backpressure
  if (full && identical(policy, "error")) {
    .pipeline_abort(
      "pipeline ingress queue is full",
      "PhysioStream_pipeline_backpressure"
    )
  }
  if (full && identical(policy, "drop_newest")) {
    candidate$counters$dropped_chunks <-
      candidate$counters$dropped_chunks + 1
    candidate$counters$dropped_samples <-
      candidate$counters$dropped_samples + nrow(chunk$samples)
    candidate$last_ingest_time_ns <- ingest_time_ns
    candidate <- .pipeline_add_drop(candidate, list(
      policy = "drop_newest",
      n_samples = as.numeric(nrow(chunk$samples)),
      sequence_start = numeric(),
      sequence_end = numeric(),
      ingest_time_ns = ingest_time_ns
    ))
    return(.pipeline_publish(pipeline, old, candidate))
  }
  if (full && identical(policy, "drop_oldest")) {
    dropped <- candidate$queue[[1L]]
    candidate$queue <- candidate$queue[-1L]
    candidate$counters$dropped_chunks <-
      candidate$counters$dropped_chunks + 1
    candidate$counters$dropped_samples <-
      candidate$counters$dropped_samples + nrow(dropped$samples)
    candidate <- .pipeline_add_drop(candidate, list(
      policy = "drop_oldest",
      n_samples = as.numeric(nrow(dropped$samples)),
      sequence_start = dropped$sequence[[1L]],
      sequence_end = dropped$sequence[[length(dropped$sequence)]],
      ingest_time_ns = dropped$ingest_time_ns,
      replacement_ingest_time_ns = ingest_time_ns
    ))
  }

  sequence_start <- candidate$counters$sequence_last + 1
  sequence_end <- candidate$counters$sequence_last + nrow(chunk$samples)
  if (!is.finite(sequence_end) || sequence_end > 2^53) {
    .pipeline_abort(
      "pipeline sample sequence would exceed exact double integers",
      "PhysioStream_pipeline_resource_error"
    )
  }
  sequence <- seq(from = sequence_start, to = sequence_end)
  queued <- list(
    samples = chunk$samples,
    timestamps = chunk$timestamps,
    sequence = sequence,
    ingest_time_ns = ingest_time_ns,
    schema = .pipeline_schema_version
  )
  .pipeline_state_bytes(queued, "queued chunk")
  candidate$queue[[length(candidate$queue) + 1L]] <- queued
  candidate$counters$accepted_chunks <-
    candidate$counters$accepted_chunks + 1
  candidate$counters$accepted_samples <-
    candidate$counters$accepted_samples + nrow(chunk$samples)
  candidate$counters$sequence_last <- sequence_end
  candidate$channel_names <- chunk$channel_names
  candidate$timestamp_mode <- chunk$timestamp_mode
  candidate$last_timestamp <- if (is.null(chunk$timestamps)) {
    NULL
  } else {
    tail(chunk$timestamps, 1L)
  }
  candidate$last_ingest_time_ns <- ingest_time_ns
  .pipeline_publish(pipeline, old, candidate)
}

.pipeline_validate_output <- function(output, input, operation) {
  if (!is.list(output) || is.object(output) ||
      !identical(output$schema, .pipeline_schema_version)) {
    .pipeline_abort(
      sprintf("operation `%s` returned an invalid output chunk", operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  samples <- output$samples
  if (!is.matrix(samples) || !is.numeric(samples) || is.object(samples) ||
      ncol(samples) < 1L || any(!is.finite(samples)) ||
      nrow(samples) > nrow(input$samples)) {
    .pipeline_abort(
      sprintf("operation `%s` returned invalid finite matrix output",
              operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  channel_names <- .dsp_channel_names(samples)
  sequence <- output$sequence
  if (is.factor(sequence) || !is.numeric(sequence) ||
      !is.null(dim(sequence)) || length(sequence) != nrow(samples) ||
      any(!is.finite(sequence)) ||
      (length(sequence) > 1L && any(diff(sequence) <= 0)) ||
      (length(sequence) &&
       any(!(sequence %in% input$sequence)))) {
    .pipeline_abort(
      sprintf("operation `%s` returned invalid sample sequence identity",
              operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  timestamps <- output$timestamps
  if (is.null(input$timestamps)) {
    if (!is.null(timestamps)) {
      .pipeline_abort(
        sprintf("operation `%s` changed timestamp presence", operation),
        "PhysioStream_pipeline_callback_error"
      )
    }
  } else if (!is.numeric(timestamps) || !is.null(dim(timestamps)) ||
             length(timestamps) != nrow(samples) ||
             any(!is.finite(timestamps)) ||
             (length(timestamps) > 1L && any(diff(timestamps) <= 0))) {
    .pipeline_abort(
      sprintf("operation `%s` returned invalid timestamps", operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  if (!identical(output$ingest_time_ns, input$ingest_time_ns)) {
    .pipeline_abort(
      sprintf("operation `%s` changed the monotonic ingest stamp", operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  positions <- match(sequence, input$sequence)
  if (!is.null(input$timestamps) &&
      !identical(as.numeric(timestamps),
                 as.numeric(input$timestamps[positions]))) {
    .pipeline_abort(
      sprintf("operation `%s` changed sample timestamp identity", operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  output$samples <- matrix(
    as.numeric(samples), nrow = nrow(samples), ncol = ncol(samples),
    dimnames = list(NULL, channel_names)
  )
  output$sequence <- as.numeric(sequence)
  output$timestamps <- if (is.null(timestamps)) NULL else as.numeric(timestamps)
  output
}

.pipeline_validate_events <- function(events, operation) {
  if (!is.list(events) || is.object(events)) {
    .pipeline_abort(
      sprintf("operation `%s` returned invalid events", operation),
      "PhysioStream_pipeline_callback_error"
    )
  }
  for (i in seq_along(events)) {
    event <- events[[i]]
    if (!is.list(event) || is.object(event) ||
        !is.numeric(event$timestamp) || length(event$timestamp) != 1L ||
        !is.finite(event$timestamp) ||
        !is.character(event$type) || length(event$type) != 1L ||
        is.na(event$type) || !nzchar(event$type) ||
        length(event$value) != 1L || !is.atomic(event$value) ||
        is.object(event$value) || anyNA(event$value)) {
      .pipeline_abort(
        sprintf("operation `%s` returned malformed event %d", operation, i),
        "PhysioStream_pipeline_callback_error"
      )
    }
  }
  events
}

.pipeline_invoke <- function(callback, chunk, operation, context) {
  result <- tryCatch(
    callback(
      .dsp_deep_copy(chunk),
      .dsp_deep_copy(operation$state),
      .dsp_deep_copy(context)
    ),
    error = function(e) {
      .pipeline_abort(
        sprintf("operation `%s` failed: %s",
                operation$name, conditionMessage(e)),
        "PhysioStream_pipeline_callback_error"
      )
    }
  )
  if (!is.list(result) || is.object(result) ||
      !all(c("output", "state", "events", "diagnostics") %in% names(result)) ||
      !is.list(result$state) || is.object(result$state) ||
      !is.list(result$diagnostics) || is.object(result$diagnostics)) {
    .pipeline_abort(
      sprintf("operation `%s` returned an invalid callback result",
              operation$name),
      "PhysioStream_pipeline_callback_error"
    )
  }
  if (is.null(result$output)) {
    if (!identical(operation$kind, "detector")) {
      .pipeline_abort(
        sprintf("operation `%s` returned NULL outside detector mode",
                operation$name),
        "PhysioStream_pipeline_callback_error"
      )
    }
    result$output <- chunk
  }
  result$output <- .pipeline_validate_output(
    result$output, chunk, operation$name
  )
  result$events <- .pipeline_validate_events(result$events, operation$name)
  if (!is.null(chunk$timestamps) && length(result$events) &&
      any(vapply(
        result$events, function(event) event$timestamp,
        numeric(1)
      ) < chunk$timestamps[[1L]])) {
    .pipeline_abort(
      sprintf("operation `%s` emitted an event before its input chunk",
              operation$name),
      "PhysioStream_pipeline_callback_error"
    )
  }
  .pipeline_state_bytes(
    result$state, paste0("operation$", operation$name, "$state"),
    .dsp_state_limit
  )
  .pipeline_state_bytes(result, paste0("operation$", operation$name, "$result"))
  result
}

.pipeline_append_latency <- function(state, record) {
  for (name in names(state$latency)) {
    state$latency[[name]] <- c(state$latency[[name]], record[[name]])
  }
  state
}

.pipeline_process_one <- function(pipeline) {
  .pipeline_assert(pipeline)
  old <- .dsp_deep_copy(pipeline$state)
  if (!length(old$queue)) {
    return(NULL)
  }
  if (length(old$latency$end_to_end_ms) >= .pipeline_latency_limit) {
    .pipeline_abort(
      "latency record ceiling of 100000 chunks reached",
      "PhysioStream_pipeline_resource_error"
    )
  }
  old_callbacks <- pipeline$callbacks
  old_source <- pipeline$source
  committed <- FALSE
  pipeline$busy <- TRUE
  on.exit({
    if (!committed) {
      pipeline$state <- old
      pipeline$callbacks <- old_callbacks
      pipeline$source <- old_source
    }
    pipeline$busy <- FALSE
  }, add = TRUE)
  candidate <- old
  input <- .dsp_deep_copy(candidate$queue[[1L]])
  chunk <- input
  all_events <- list()
  all_event_sources <- list()
  all_diagnostics <- list()
  process_start <- cpp_monotonic_ns()
  for (i in seq_along(candidate$operations)) {
    operation <- candidate$operations[[i]]
    context <- list(
      channel_names = colnames(chunk$samples),
      operation_name = operation$name,
      operation_kind = operation$kind,
      chunk_index = candidate$counters$processed_chunks + 1,
      latency_budget_ms = candidate$configuration$latency_budget_ms,
      schema = .pipeline_schema_version
    )
    result <- .pipeline_invoke(
      pipeline$callbacks[[i]], chunk, operation, context
    )
    candidate$operations[[i]]$state <- result$state
    chunk <- result$output
    if (length(result$events)) {
      all_events <- c(all_events, result$events)
      all_event_sources <- c(
        all_event_sources,
        lapply(seq_along(result$events), function(j) {
          list(
            operation_name = operation$name,
            operation_kind = operation$kind,
            operation_type = operation$type,
            operation_index = as.numeric(i)
          )
        })
      )
    }
    all_diagnostics[[operation$name]] <- result$diagnostics
  }
  process_end <- cpp_monotonic_ns()
  candidate$queue <- candidate$queue[-1L]
  candidate$counters$processed_chunks <-
    candidate$counters$processed_chunks + 1
  candidate$counters$processed_samples <-
    candidate$counters$processed_samples + nrow(input$samples)
  candidate$counters$emitted_chunks <-
    candidate$counters$emitted_chunks + 1
  candidate$counters$emitted_samples <-
    candidate$counters$emitted_samples + nrow(chunk$samples)
  candidate <- .dsp_seal_state(candidate)
  emit <- cpp_monotonic_ns()
  record <- list(
    sequence_start = input$sequence[[1L]],
    sequence_end = input$sequence[[length(input$sequence)]],
    ingest_ns = input$ingest_time_ns,
    process_start_ns = process_start,
    process_end_ns = process_end,
    emit_ns = emit,
    queue_wait_ms = (process_start - input$ingest_time_ns) / 1e6,
    processing_ms = (process_end - process_start) / 1e6,
    end_to_end_ms = (emit - input$ingest_time_ns) / 1e6,
    budget_exceeded =
      (emit - input$ingest_time_ns) / 1e6 >
      candidate$configuration$latency_budget_ms
  )
  if (record$queue_wait_ms < 0 || record$processing_ms < 0 ||
      record$end_to_end_ms < 0 ||
      process_start > process_end || process_end > emit) {
    .pipeline_abort(
      "monotonic latency phase ordering was violated",
      "PhysioStream_pipeline_timing_error"
    )
  }
  candidate <- .pipeline_append_latency(candidate, record)
  candidate <- .dsp_seal_state(candidate)
  runtime_unchanged <- tryCatch(
    identical(
      serialize(pipeline$state, NULL, version = 3L),
      serialize(old, NULL, version = 3L)
    ),
    error = function(e) FALSE
  )
  if (!runtime_unchanged) {
    .pipeline_abort(
      "pipeline state changed during chunk processing",
      "PhysioStream_pipeline_state_error"
    )
  }
  if (!identical(pipeline$callbacks, old_callbacks) ||
      !identical(pipeline$source, old_source)) {
    .pipeline_abort(
      "pipeline runtime registry changed during callback execution",
      "PhysioStream_pipeline_state_error"
    )
  }
  output <- list(
    output = .dsp_deep_copy(chunk),
    events = .dsp_deep_copy(all_events),
    event_sources = .dsp_deep_copy(all_event_sources),
    diagnostics = .dsp_deep_copy(all_diagnostics),
    latency = record,
    state_sha256 = candidate$sha256,
    schema = .pipeline_schema_version
  )
  .pipeline_state_bytes(output, "pipeline output")
  pipeline$state <- candidate
  committed <- TRUE
  output
}

#' Process queued chunks synchronously
#'
#' @param pipeline A `StreamPipeline`.
#' @param n Maximum whole chunks to process.
#' @return A plain list containing per-chunk results and counts. Each result
#'   includes `event_sources`, a plain operation identity parallel to `events`.
#' @examples
#' sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
#'               dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
#' pipeline <- streamPipeline(chunk_size = 8L)
#' onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
#' x <- matrix(sin(seq_len(16)), 8, 2, dimnames = list(NULL, c("C3", "C4")))
#' pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
#' pipelineStep(pipeline)$n_processed
#' @export
pipelineStep <- function(pipeline, n = 1L) {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  n <- .dsp_scalar(
    n, "n", lower = 0, upper = .Machine$integer.max, integer = TRUE
  )
  results <- vector("list", min(n, length(pipeline$state$queue)))
  if (length(results)) {
    for (i in seq_along(results)) {
      results[[i]] <- .pipeline_process_one(pipeline)
    }
  }
  list(
    results = results,
    n_processed = length(results),
    n_remaining = length(pipeline$state$queue),
    state_sha256 = pipeline$state$sha256,
    schema = .pipeline_schema_version
  )
}

#' Pull from a configured source and process synchronously
#'
#' The source must already be open. The function never starts a background
#' thread and stops on an empty pull rather than busy-spinning.
#'
#' @param pipeline A source-backed `StreamPipeline`.
#' @param max_chunks Maximum chunks to process; `Inf` is allowed.
#' @param timeout Finite non-negative overall run time in seconds. Zero means
#'   no wall-time limit for the current call.
#' @return A plain list of processed results and stop diagnostics.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = "C3", nominal_srate = 100)
#' src <- streamOpen(loopbackSource(info, capacity = 64L))
#' loopbackFeed(src, matrix(sin(seq_len(16)), 16, 1), seq_len(16) / 100)
#' sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
#'               dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
#' pipeline <- streamPipeline(source = src, chunk_size = 8L)
#' onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
#' run <- pipelineRun(pipeline)
#' run$n_processed
#' @export
pipelineRun <- function(pipeline, max_chunks = Inf, timeout = 0) {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  if (is.null(pipeline$source)) {
    .pipeline_abort(
      "`pipeline` has no configured StreamSource",
      "PhysioStream_pipeline_validation_error"
    )
  }
  if (!(is.numeric(max_chunks) && length(max_chunks) == 1L &&
        !is.na(max_chunks) &&
        (is.infinite(max_chunks) && max_chunks > 0 ||
         (is.finite(max_chunks) && max_chunks >= 0 &&
          max_chunks == floor(max_chunks) && max_chunks <= 2^53)))) {
    .pipeline_abort(
      "`max_chunks` must be a non-negative exact integer or Inf",
      "PhysioStream_pipeline_validation_error"
    )
  }
  timeout <- .dsp_scalar(timeout, "timeout", lower = 0)
  start_ns <- cpp_monotonic_ns()
  results <- list()
  reason <- "max_chunks"
  while (length(results) < max_chunks) {
    if (timeout > 0 &&
        (cpp_monotonic_ns() - start_ns) / 1e9 >= timeout) {
      reason <- "timeout"
      break
    }
    if (length(pipeline$state$queue)) {
      step <- pipelineStep(pipeline, 1L)
      results[[length(results) + 1L]] <- step$results[[1L]]
      next
    }
    pulled <- streamPull(
      pipeline$source, n = pipeline$state$configuration$chunk_size
    )
    if (!is.list(pulled) || !is.numeric(pulled$count) ||
        length(pulled$count) != 1L || !is.finite(pulled$count) ||
        pulled$count < 0 || pulled$count != floor(pulled$count) ||
        !is.matrix(pulled$samples) ||
        nrow(pulled$samples) != pulled$count) {
      .pipeline_abort(
        "StreamSource returned an invalid pull result",
        "PhysioStream_pipeline_source_error"
      )
    }
    if (pulled$count == 0L) {
      reason <- "empty_source"
      break
    }
    ingest <- cpp_monotonic_ns()
    pipelineEnqueue(
      pipeline, pulled$samples, pulled$timestamps,
      ingest_time_ns = ingest
    )
    step <- pipelineStep(pipeline, 1L)
    results[[length(results) + 1L]] <- step$results[[1L]]
  }
  list(
    results = results,
    n_processed = length(results),
    stop_reason = reason,
    n_remaining = length(pipeline$state$queue),
    state_sha256 = pipeline$state$sha256,
    schema = .pipeline_schema_version
  )
}

#' Inspect or reset governed pipeline state
#'
#' @param pipeline A `StreamPipeline`.
#' @param keep_operations Whether reset retains the registered graph and resets
#'   each operation to its initial state.
#' @return `pipelineState()` returns a deep plain list; `pipelineReset()`
#'   returns the pipeline invisibly.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 8L)
#' pipelineState(pipeline)$configuration$chunk_size
#' pipelineReset(pipeline)
#' @name pipeline-state
NULL

#' @rdname pipeline-state
#' @export
pipelineState <- function(pipeline) {
  .pipeline_assert(pipeline)
  .dsp_deep_copy(pipeline$state)
}

#' @rdname pipeline-state
#' @export
pipelineReset <- function(pipeline, keep_operations = TRUE) {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  keep_operations <- .dsp_logical(keep_operations, "keep_operations")
  old <- .dsp_deep_copy(pipeline$state)
  operations <- if (keep_operations) {
    lapply(old$operations, function(operation) {
      operation$state <- .dsp_deep_copy(operation$initial_state)
      operation
    })
  } else {
    list()
  }
  candidate <- .pipeline_empty_state(
    old$configuration, operations, old$reset_count + 1
  )
  candidate <- .dsp_seal_state(candidate)
  if (!identical(pipeline$state$sha256, old$sha256)) {
    .pipeline_abort(
      "pipeline state changed during reset",
      "PhysioStream_pipeline_state_error"
    )
  }
  pipeline$state <- candidate
  if (!keep_operations) {
    pipeline$callbacks <- list()
  }
  invisible(pipeline)
}

#' @export
print.StreamPipeline <- function(x, ...) {
  .pipeline_assert(x)
  cat(sprintf(
    "<StreamPipeline: operations=%d, queued=%d/%d, processed=%s, dropped=%s>\n",
    length(x$state$operations), length(x$state$queue),
    x$state$configuration$queue_capacity,
    x$state$counters$processed_chunks,
    x$state$counters$dropped_chunks
  ))
  invisible(x)
}
