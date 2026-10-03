.biofeedback_assert <- function(scope) {
  if (!inherits(scope, "BiofeedbackScope") || !is.environment(scope)) {
    .biofeedback_abort(
      "`scope` must be a BiofeedbackScope",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  .biofeedback_validate_sealed(
    scope$state, .biofeedback_state_limit, "biofeedback state"
  )
  if (!methods::is(scope$source, "StreamSource") ||
      !inherits(scope$pipeline, "StreamPipeline") ||
      !is.environment(scope$pipeline) ||
      !is.list(scope$pipeline_contract) ||
      is.object(scope$pipeline_contract) ||
      !is.list(scope$histories) || is.object(scope$histories) ||
      !is.list(scope$events) || is.object(scope$events) ||
      !is.logical(scope$busy) || length(scope$busy) != 1L ||
      is.na(scope$busy) ||
      !is.logical(scope$guard_clock) || length(scope$guard_clock) != 1L ||
      is.na(scope$guard_clock) ||
      !is.function(scope$clock)) {
    .biofeedback_abort(
      "biofeedback runtime registry is invalid",
      "PhysioStream_biofeedback_state_error"
    )
  }
  pipeline_state <- pipelineState(scope$pipeline)
  if (!identical(
        .biofeedback_source_descriptor(scope$source),
        scope$state$configuration$source
      ) ||
      !identical(
        pipeline_state$configuration$source,
        .pipeline_source_descriptor(scope$source)
      ) ||
      !identical(
        .biofeedback_pipeline_contract(pipeline_state),
        scope$pipeline_contract
      )) {
    .biofeedback_abort(
      "biofeedback source or pipeline identity changed",
      "PhysioStream_biofeedback_state_error"
    )
  }
  if (!identical(names(scope$histories),
                 names(scope$state$configuration$trace_meta))) {
    .biofeedback_abort(
      "biofeedback history registry does not match trace state",
      "PhysioStream_biofeedback_state_error"
    )
  }
  if (!is.null(scope$latest_frame)) {
    .biofeedback_validate_frame(scope$latest_frame)
  }
  invisible(TRUE)
}

.biofeedback_require_idle <- function(scope) {
  if (isTRUE(scope$busy)) {
    .biofeedback_abort(
      "biofeedback lifecycle operations are not reentrant",
      "PhysioStream_biofeedback_state_error"
    )
  }
  invisible(TRUE)
}

.biofeedback_now <- function(scope, previous = NULL) {
  before <- if (scope$guard_clock) {
    list(
      state = .dsp_deep_copy(scope$state),
      histories = .dsp_deep_copy(scope$histories),
      events = .dsp_deep_copy(scope$events),
      latest_frame = if (is.null(scope$latest_frame)) {
        NULL
      } else {
        .dsp_deep_copy(scope$latest_frame)
      },
      source = .biofeedback_source_identity(scope$source),
      pipeline = pipelineState(scope$pipeline),
      pipeline_callbacks = scope$pipeline$callbacks,
      pipeline_source = scope$pipeline$source,
      video = .dsp_deep_copy(scope$video),
      guard_clock = scope$guard_clock
    )
  } else {
    NULL
  }
  value <- tryCatch(
    scope$clock(),
    error = function(e) {
      .biofeedback_abort(
        "biofeedback monotonic clock failed",
        "PhysioStream_biofeedback_timing_error"
      )
    }
  )
  if (scope$guard_clock) {
    after <- list(
      state = scope$state,
      histories = scope$histories,
      events = scope$events,
      latest_frame = scope$latest_frame,
      source = .biofeedback_source_identity(scope$source),
      pipeline = pipelineState(scope$pipeline),
      pipeline_callbacks = scope$pipeline$callbacks,
      pipeline_source = scope$pipeline$source,
      video = scope$video,
      guard_clock = scope$guard_clock
    )
    if (!identical(before, after)) {
      .biofeedback_abort(
        "biofeedback runtime changed during monotonic clock evaluation",
        "PhysioStream_biofeedback_state_error"
      )
    }
  }
  value <- .biofeedback_scalar(
    value, "monotonic clock", lower = 0
  )
  if (!is.null(previous) && value < previous) {
    .biofeedback_abort(
      "biofeedback monotonic clock moved backwards",
      "PhysioStream_biofeedback_timing_error"
    )
  }
  value
}

.biofeedback_pipeline_identity <- function(pipeline, source) {
  state <- pipelineState(pipeline)
  expected <- .pipeline_source_descriptor(source)
  if (is.null(state$configuration$source) ||
      !identical(state$configuration$source, expected)) {
    .biofeedback_abort(
      "`pipeline` must bind the exact supplied source descriptor",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  if (length(state$queue)) {
    .biofeedback_abort(
      "`pipeline` must have an empty ingress queue at scope construction",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  state
}

.biofeedback_pipeline_contract <- function(state) {
  list(
    configuration = state$configuration,
    operations = lapply(state$operations, function(operation) {
      list(
        name = operation$name,
        kind = operation$kind,
        type = operation$type,
        configuration = operation$configuration
      )
    })
  )
}

.biofeedback_empty_counters <- function() {
  list(
    steps = 0,
    chunks = 0,
    source_samples = 0,
    committed_samples = 0,
    accepted_trace_points = 0,
    external_updates = 0,
    external_values = 0,
    frames = 0,
    empty_steps = 0,
    audit_truncated = 0,
    event_truncated = 0
  )
}

.biofeedback_default_clock <- function() {
  cpp_monotonic_ns()
}

#' Construct and optionally launch a governed live biofeedback scope
#'
#' Construction is side-effect-free. With `launch = FALSE`, the returned
#' runtime is advanced synchronously with [biofeedbackStart()] and
#' [biofeedbackStep()]. Shiny is required only for `launch = TRUE`.
#'
#' @param source A regular-rate numeric `StreamSource`.
#' @param pipeline Optional source-backed `StreamPipeline`.
#' @param channels Exact source channel names to display.
#' @param derived Strictly named derived/external trace descriptors.
#' @param window_seconds Positive visible signal-time window.
#' @param update_hz Requested display update rate.
#' @param max_points Maximum plotted representatives per trace.
#' @param gain Positive display-only gain.
#' @param threshold Optional finite display threshold.
#' @param target_range Optional increasing display target range.
#' @param source_lifecycle Whether the scope borrows an open source or owns its
#'   open/close lifecycle.
#' @param launch Whether to run the installed Shiny app immediately.
#' @param host Local Shiny bind host.
#' @param port Optional exact TCP port.
#' @param browser Whether Shiny should launch a browser.
#' @param clock Optional monotonic nanosecond clock for deterministic tests.
#' @return A `BiofeedbackScope` when `launch = FALSE`, otherwise the result of
#'   `shiny::runApp()`.
#' @examples
#' # Construction is device-free; Shiny is required only for launch = TRUE.
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("left", "right"), nominal_srate = 100,
#'                    channel_units = c("uV", "uV"))
#' source <- loopbackSource(info, capacity = 256L)
#' scope <- biofeedbackScope(source, source_lifecycle = "own", launch = FALSE)
#' biofeedbackState(scope)$lifecycle
#' @export
biofeedbackScope <- function(
    source,
    pipeline = NULL,
    channels = NULL,
    derived = list(),
    window_seconds = 10,
    update_hz = 20,
    max_points = 2000L,
    gain = 1,
    threshold = NULL,
    target_range = NULL,
    source_lifecycle = c("borrow", "own"),
    launch = interactive(),
    host = "127.0.0.1",
    port = NULL,
    browser = interactive(),
    clock = NULL) {
  if (!methods::is(source, "StreamSource")) {
    .biofeedback_abort(
      "`source` must be a StreamSource",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  info <- streamInfo(source)
  if (identical(info@dtype, "string") ||
      !is.numeric(info@nominal_srate) ||
      length(info@nominal_srate) != 1L ||
      !is.finite(info@nominal_srate) ||
      info@nominal_srate <= 0) {
    .biofeedback_abort(
      "biofeedback requires a regular-rate numeric source",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  source_channels <- .biofeedback_names(
    info@channel_names, "source channel names"
  )
  source_units <- info@channel_units
  if (!is.character(source_units) ||
      length(source_units) != length(source_channels) ||
      anyNA(source_units) || any(!nzchar(source_units)) ||
      any(nchar(source_units, type = "bytes") > 256L)) {
    .biofeedback_abort(
      "biofeedback requires one explicit non-empty unit per source channel",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  source_units <- as.character(source_units)
  if (is.null(channels)) {
    channels <- source_channels
  } else {
    channels <- .biofeedback_names(channels, "channels")
    if (any(!channels %in% source_channels)) {
      .biofeedback_abort(
        "`channels` must exactly match source channel names",
        "PhysioStream_biofeedback_channel_error"
      )
    }
  }
  selected_units <- source_units[match(channels, source_channels)]
  window_seconds <- .biofeedback_scalar(
    window_seconds, "window_seconds", lower = 0, lower_open = TRUE,
    upper = 3600
  )
  update_hz <- .biofeedback_scalar(
    update_hz, "update_hz", lower = 1, upper = 120
  )
  max_points <- .biofeedback_scalar(
    max_points, "max_points", lower = 32, upper = 100000,
    integer = TRUE
  )
  gain <- .biofeedback_scalar(
    gain, "gain", lower = 0, lower_open = TRUE
  )
  threshold <- .biofeedback_numeric(
    threshold, "threshold", length = 1L, allow_null = TRUE
  )
  target_range <- .biofeedback_numeric(
    target_range, "target_range", length = 2L, allow_null = TRUE
  )
  if (!is.null(target_range) && target_range[[1L]] >= target_range[[2L]]) {
    .biofeedback_abort(
      "`target_range` must be strictly increasing",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  if (length(source_lifecycle) > 1L) {
    source_lifecycle <- source_lifecycle[[1L]]
  }
  source_lifecycle <- .biofeedback_enum(
    source_lifecycle, c("borrow", "own"), "source_lifecycle"
  )
  launch <- .biofeedback_logical(launch, "launch")
  browser <- .biofeedback_logical(browser, "browser")
  host <- .biofeedback_name(host, "host")
  if (!host %in% c("127.0.0.1", "localhost", "::1")) {
    .biofeedback_abort(
      "`host` must be a loopback address",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  if (!is.null(port)) {
    port <- .biofeedback_scalar(
      port, "port", lower = 1, upper = 65535, integer = TRUE
    )
  }
  guard_clock <- !is.null(clock)
  if (is.null(clock)) {
    clock <- .biofeedback_default_clock
  }
  if (!is.function(clock) || length(formals(clock)) != 0L) {
    .biofeedback_abort(
      "`clock` must be NULL or a zero-argument function",
      "PhysioStream_biofeedback_validation_error"
    )
  }

  chunk_size <- max(
    1L, as.integer(ceiling(info@nominal_srate / update_hz))
  )
  if (is.null(pipeline)) {
    pipeline <- streamPipeline(
      source = source,
      chunk_size = chunk_size,
      queue_capacity = 16L,
      backpressure = "error",
      latency_budget_ms = 1000 / update_hz
    )
  } else if (!inherits(pipeline, "StreamPipeline") ||
             !is.environment(pipeline)) {
    .biofeedback_abort(
      "`pipeline` must be NULL or a StreamPipeline",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  pipeline_state <- .biofeedback_pipeline_identity(pipeline, source)
  trace_contract <- .biofeedback_build_trace_contract(
    channels, selected_units, gain, threshold, target_range,
    derived, pipeline_state
  )
  target_buckets <- max(1, floor(max_points / 4) - 2)
  bucket_width <- window_seconds / target_buckets
  configuration <- list(
    source = .biofeedback_source_descriptor(source),
    channels = channels,
    source_channel_positions = as.numeric(match(channels, source_channels)),
    trace_descriptors = trace_contract$descriptors,
    trace_meta = trace_contract$trace_meta,
    window_seconds = window_seconds,
    update_hz = update_hz,
    max_points = as.numeric(max_points),
    bucket_width = bucket_width,
    chunk_size = as.numeric(pipeline_state$configuration$chunk_size),
    max_chunks_default = 16,
    source_lifecycle = source_lifecycle,
    host = host,
    port = if (is.null(port)) NULL else as.numeric(port),
    browser = browser
  )
  scope_id <- digest::digest(
    serialize(configuration, NULL, version = 3L),
    algo = "sha256", serialize = FALSE
  )
  histories <- lapply(
    trace_contract$trace_meta, .biofeedback_new_history
  )
  state <- list(
    scope_id = scope_id,
    lifecycle = "created",
    configuration = configuration,
    latest_signal_time = NULL,
    latest_monotonic_ns = NULL,
    latest_external_sequence = NULL,
    trace_stats = lapply(
      histories,
      function(x) list(
        n_points = 0,
        first_timestamp = NULL,
        last_timestamp = NULL
      )
    ),
    initial_source_stats = NULL,
    initial_pipeline_stats = NULL,
    counters = .biofeedback_empty_counters(),
    audit = list(),
    last_error_code = NULL
  )
  scope <- new.env(parent = emptyenv())
  scope$state <- .biofeedback_seal(
    state, .biofeedback_state_limit, "biofeedback state"
  )
  scope$source <- source
  scope$pipeline <- pipeline
  scope$pipeline_contract <- .biofeedback_pipeline_contract(pipeline_state)
  scope$histories <- histories
  scope$events <- list()
  scope$latest_frame <- NULL
  scope$busy <- FALSE
  scope$clock <- clock
  scope$guard_clock <- guard_clock
  scope$video <- NULL
  class(scope) <- c("BiofeedbackScope", "BiofeedbackRuntime")
  if (launch) {
    return(.biofeedback_launch(scope))
  }
  scope
}

#' Start, step, and stop a biofeedback runtime
#'
#' @param scope A `BiofeedbackScope`.
#' @param max_chunks Exact per-step work bound.
#' @return Lifecycle functions return `scope` invisibly.
#'   `biofeedbackStep()` returns a plain update summary.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("left", "right"), nominal_srate = 100,
#'                    channel_units = c("uV", "uV"))
#' source <- streamOpen(loopbackSource(info, capacity = 4096L))
#' scope <- biofeedbackScope(source, update_hz = 20, window_seconds = 1,
#'                           max_points = 64L, launch = FALSE)
#' biofeedbackStart(scope)
#' loopbackFeed(source, matrix(as.double(1:40), 20, 2), seq_len(20) / 100)
#' invisible(biofeedbackStep(scope))
#' biofeedbackStop(scope)
#' biofeedbackState(scope)$lifecycle
#' @name biofeedback-lifecycle
NULL

#' @rdname biofeedback-lifecycle
#' @export
biofeedbackStart <- function(scope) {
  .biofeedback_assert(scope)
  .biofeedback_require_idle(scope)
  if (!identical(scope$state$lifecycle, "created")) {
    .biofeedback_abort(
      "biofeedbackStart requires a created scope",
      "PhysioStream_biofeedback_state_error"
    )
  }
  old <- .dsp_deep_copy(scope$state)
  old_histories <- .dsp_deep_copy(scope$histories)
  old_pipeline <- pipelineState(scope$pipeline)
  old_clock <- scope$clock
  scope$busy <- TRUE
  committed <- FALSE
  opened_by_scope <- FALSE
  on.exit({
    scope$busy <- FALSE
    if (!committed) {
      if (opened_by_scope &&
          identical(streamState(scope$source), "open")) {
        try(scope$source <- streamClose(scope$source), silent = TRUE)
      }
      scope$state <- old
      scope$histories <- old_histories
    }
  }, add = TRUE)

  lifecycle <- old$configuration$source_lifecycle
  current <- streamState(scope$source)
  if (identical(lifecycle, "borrow")) {
    if (!identical(current, "open")) {
      .biofeedback_abort(
        "a borrowed source must already be open",
        "PhysioStream_biofeedback_state_error"
      )
    }
  } else {
    if (!identical(current, "created")) {
      .biofeedback_abort(
        "an owned source must be in the created state",
        "PhysioStream_biofeedback_state_error"
      )
    }
    scope$source <- streamOpen(scope$source)
    opened_by_scope <- TRUE
  }
  if (!identical(serialize(scope$state, NULL, version = 3L),
                 serialize(old, NULL, version = 3L)) ||
      !identical(scope$histories, old_histories) ||
      !identical(pipelineState(scope$pipeline), old_pipeline) ||
      !identical(scope$clock, old_clock)) {
    .biofeedback_abort(
      "biofeedback runtime changed during source start",
      "PhysioStream_biofeedback_state_error"
    )
  }
  now <- .biofeedback_now(scope)
  candidate <- old
  candidate$lifecycle <- "running"
  candidate$latest_monotonic_ns <- now
  candidate$initial_source_stats <- .biofeedback_source_stats(scope$source)
  candidate$initial_pipeline_stats <- .biofeedback_pipeline_stats(
    scope$pipeline
  )
  candidate <- .biofeedback_append_audit(candidate, list(
    operation = "start",
    monotonic_ns = now,
    source_lifecycle = lifecycle
  ))
  scope$state <- .biofeedback_seal(
    candidate, .biofeedback_state_limit, "biofeedback state"
  )
  committed <- TRUE
  invisible(scope)
}

.biofeedback_validate_pull <- function(pulled, channels) {
  if (is.list(pulled) && is.matrix(pulled$samples) &&
      is.null(colnames(pulled$samples)) &&
      ncol(pulled$samples) == length(channels)) {
    colnames(pulled$samples) <- channels
  }
  if (!is.list(pulled) || is.object(pulled) ||
      !is.numeric(pulled$count) || length(pulled$count) != 1L ||
      !is.finite(pulled$count) || pulled$count < 0 ||
      pulled$count != floor(pulled$count) ||
      !is.matrix(pulled$samples) || !is.numeric(pulled$samples) ||
      is.object(pulled$samples) ||
      nrow(pulled$samples) != pulled$count ||
      ncol(pulled$samples) != length(channels) ||
      !identical(colnames(pulled$samples), channels) ||
      any(!is.finite(pulled$samples)) ||
      !is.numeric(pulled$timestamps) ||
      length(pulled$timestamps) != pulled$count ||
      any(!is.finite(pulled$timestamps)) ||
      (pulled$count > 1L && any(diff(pulled$timestamps) <= 0))) {
    .biofeedback_abort(
      "StreamSource returned an invalid biofeedback pull",
      "PhysioStream_biofeedback_source_error"
    )
  }
  pulled
}

.biofeedback_add_trace <- function(histories, state, trace_id,
                                   timestamps, values) {
  if (!length(timestamps)) {
    return(list(histories = histories, state = state))
  }
  start <- state$counters$accepted_trace_points + 1
  end <- state$counters$accepted_trace_points + length(values)
  if (!is.finite(end) || end > 2^53) {
    .biofeedback_abort(
      "biofeedback trace sequence exceeded exact numeric identity",
      "PhysioStream_biofeedback_resource_error"
    )
  }
  latest <- max(timestamps)
  cutoff <- latest - state$configuration$window_seconds
  histories[[trace_id]] <- .biofeedback_history_add(
    histories[[trace_id]], timestamps, values, seq(start, end),
    state$configuration$bucket_width, cutoff
  )
  state$counters$accepted_trace_points <- end
  list(histories = histories, state = state)
}

.biofeedback_append_chunk <- function(histories, state, pulled, result) {
  channels <- state$configuration$channels
  positions <- state$configuration$source_channel_positions
  for (i in seq_along(channels)) {
    id <- .biofeedback_trace_id("raw", channels[[i]])
    added <- .biofeedback_add_trace(
      histories, state, id, pulled$timestamps,
      pulled$samples[, positions[[i]]]
    )
    histories <- added$histories
    state <- added$state
  }
  output <- result$output
  for (descriptor in state$configuration$trace_descriptors) {
    if (identical(descriptor$type, "external")) {
      next
    }
    if (is.null(output$timestamps) ||
        any(!descriptor$output_channels %in% colnames(output$samples))) {
      .biofeedback_abort(
        sprintf("committed operation `%s` did not emit declared trace channels",
                descriptor$operation_name),
        "PhysioStream_biofeedback_pipeline_error"
      )
    }
    for (i in seq_along(descriptor$output_channels)) {
      values <- output$samples[, descriptor$output_channels[[i]]]
      if (identical(descriptor$type, "band_power")) {
        values <- values^2
        if (any(!is.finite(values))) {
          .biofeedback_abort(
            "band-power display overflowed finite numeric range",
            "PhysioStream_biofeedback_resource_error"
          )
        }
      }
      added <- .biofeedback_add_trace(
        histories, state, descriptor$trace_ids[[i]],
        output$timestamps, values
      )
      histories <- added$histories
      state <- added$state
    }
  }
  state$latest_signal_time <- max(
    c(state$latest_signal_time, pulled$timestamps)
  )
  state$counters$chunks <- state$counters$chunks + 1
  state$counters$source_samples <-
    state$counters$source_samples + pulled$count
  state$counters$committed_samples <-
    state$counters$committed_samples + nrow(output$samples)
  list(histories = histories, state = state)
}

.biofeedback_append_events <- function(events, result, state) {
  if (length(result$events)) {
    for (i in seq_along(result$events)) {
      events[[length(events) + 1L]] <- list(
        timestamp = result$events[[i]]$timestamp,
        type = result$events[[i]]$type,
        value = result$events[[i]]$value,
        source = result$event_sources[[i]]
      )
    }
  }
  if (length(events) > .biofeedback_event_limit) {
    excess <- length(events) - .biofeedback_event_limit
    events <- tail(events, .biofeedback_event_limit)
    state$counters$event_truncated <-
      state$counters$event_truncated + excess
  }
  list(events = events, state = state)
}

.biofeedback_error_code <- function(error) {
  classes <- class(error)
  known <- classes[grepl("^PhysioStream_", classes)]
  if (length(known)) known[[1L]] else "PhysioStream_biofeedback_step_error"
}

.biofeedback_fail <- function(scope, old, error) {
  candidate <- old
  candidate$lifecycle <- "error_stopped"
  candidate$last_error_code <- .biofeedback_error_code(error)
  now <- tryCatch(
    .biofeedback_now(scope, old$latest_monotonic_ns),
    error = function(e) old$latest_monotonic_ns
  )
  candidate$latest_monotonic_ns <- now
  candidate <- .biofeedback_append_audit(candidate, list(
    operation = "error_stop",
    monotonic_ns = now,
    error_code = candidate$last_error_code
  ))
  if (identical(old$configuration$source_lifecycle, "own") &&
      identical(streamState(scope$source), "open")) {
    try(scope$source <- streamClose(scope$source), silent = TRUE)
  }
  scope$state <- .biofeedback_seal(
    candidate, .biofeedback_state_limit, "biofeedback state"
  )
  invisible(scope)
}

#' @rdname biofeedback-lifecycle
#' @export
biofeedbackStep <- function(scope, max_chunks = 16L) {
  .biofeedback_assert(scope)
  .biofeedback_require_idle(scope)
  if (!identical(scope$state$lifecycle, "running")) {
    .biofeedback_abort(
      "biofeedbackStep requires a running scope",
      "PhysioStream_biofeedback_state_error"
    )
  }
  max_chunks <- .biofeedback_scalar(
    max_chunks, "max_chunks", lower = 1, upper = 100000, integer = TRUE
  )
  old <- .dsp_deep_copy(scope$state)
  old_histories <- scope$histories
  old_events <- scope$events
  old_frame <- scope$latest_frame
  old_clock <- scope$clock
  old_source_identity <- .biofeedback_source_identity(scope$source)
  old_pipeline_callbacks <- scope$pipeline$callbacks
  old_pipeline_source <- scope$pipeline$source
  scope$busy <- TRUE
  committed <- FALSE
  on.exit({
    scope$busy <- FALSE
    if (!committed) {
      scope$histories <- old_histories
      scope$events <- old_events
      scope$latest_frame <- old_frame
    }
  }, add = TRUE)

  result <- tryCatch({
    candidate <- old
    histories <- .dsp_deep_copy(old_histories)
    events <- .dsp_deep_copy(old_events)
    processed <- 0L
    for (i in seq_len(max_chunks)) {
      pulled <- streamPull(
        scope$source, n = candidate$configuration$chunk_size
      )
      pulled <- .biofeedback_validate_pull(
        pulled, candidate$configuration$source$channel_names
      )
      if (pulled$count == 0L) {
        break
      }
      pipelineEnqueue(
        scope$pipeline, pulled$samples, pulled$timestamps,
        ingest_time_ns = .biofeedback_now(
          scope, candidate$latest_monotonic_ns
        )
      )
      stepped <- pipelineStep(scope$pipeline, 1L)
      if (stepped$n_processed != 1L) {
        .biofeedback_abort(
          "biofeedback pipeline did not commit one enqueued chunk",
          "PhysioStream_biofeedback_pipeline_error"
        )
      }
      chunk_result <- stepped$results[[1L]]
      appended <- .biofeedback_append_chunk(
        histories, candidate, pulled, chunk_result
      )
      histories <- appended$histories
      candidate <- appended$state
      appended_events <- .biofeedback_append_events(
        events, chunk_result, candidate
      )
      events <- appended_events$events
      candidate <- appended_events$state
      processed <- processed + 1L
    }
    if (!identical(scope$state, old) ||
        !identical(scope$histories, old_histories) ||
        !identical(scope$events, old_events) ||
        !identical(scope$latest_frame, old_frame) ||
        !identical(scope$clock, old_clock) ||
        !identical(.biofeedback_source_identity(scope$source),
                   old_source_identity) ||
        !identical(scope$pipeline$callbacks, old_pipeline_callbacks) ||
        !identical(scope$pipeline$source, old_pipeline_source)) {
      .biofeedback_abort(
        "biofeedback runtime changed during stream processing",
        "PhysioStream_biofeedback_state_error"
      )
    }
    candidate$counters$steps <- candidate$counters$steps + 1
    if (processed == 0L) {
      candidate$counters$empty_steps <- candidate$counters$empty_steps + 1
      scope$state <- .biofeedback_seal(
        candidate, .biofeedback_state_limit, "biofeedback state"
      )
      committed <- TRUE
      return(list(
        updated = FALSE,
        n_processed = 0,
        frame_id = old$counters$frames,
        state_sha256 = scope$state$sha256,
        schema = .biofeedback_schema_version
      ))
    }
    now <- .biofeedback_now(scope, old$latest_monotonic_ns)
    candidate$latest_monotonic_ns <- now
    candidate$counters$frames <- candidate$counters$frames + 1
    candidate$trace_stats <- .biofeedback_trace_stats(
      histories, candidate$latest_signal_time,
      candidate$configuration$window_seconds,
      candidate$configuration$max_points
    )
    candidate <- .biofeedback_append_audit(candidate, list(
      operation = "step",
      monotonic_ns = now,
      n_chunks = as.numeric(processed),
      frame_id = candidate$counters$frames
    ))
    candidate <- .biofeedback_seal(
      candidate, .biofeedback_state_limit, "biofeedback state"
    )
    frame <- .biofeedback_frame(
      scope, candidate, histories, events, now
    )
    if (length(serialize(histories, NULL, version = 3L)) >
        .biofeedback_materialization_limit) {
      .biofeedback_abort(
        "biofeedback history exceeded the materialization ceiling",
        "PhysioStream_biofeedback_resource_error"
      )
    }
    scope$histories <- histories
    scope$events <- events
    scope$latest_frame <- frame
    scope$state <- candidate
    committed <- TRUE
    list(
      updated = TRUE,
      n_processed = as.numeric(processed),
      frame_id = candidate$counters$frames,
      state_sha256 = candidate$sha256,
      schema = .biofeedback_schema_version
    )
  }, error = function(e) {
    .biofeedback_fail(scope, old, e)
    stop(e)
  })
  result
}

#' Publish a bounded external feedback update
#'
#' This is the sink used by downstream live metric adapters. It updates only
#' declared external display traces and has no pipeline or stimulation effect.
#'
#' @param scope A running `BiofeedbackScope`.
#' @param values Finite named values.
#' @param timestamp One signal-domain timestamp.
#' @param names Exact names corresponding to `values`.
#' @param units Optional exact units corresponding to `values`.
#' @param sequence Optional exact contiguous update sequence.
#' @return An immutable plain receipt.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("left", "right"), nominal_srate = 100,
#'                    channel_units = c("uV", "uV"))
#' source <- streamOpen(loopbackSource(info, capacity = 4096L))
#' scope <- biofeedbackScope(source, derived = list(
#'   score = list(type = "external", unit = "ratio", gain = 1)
#' ), launch = FALSE)
#' biofeedbackStart(scope)
#' receipt <- biofeedbackUpdate(scope, c(score = 0.8), timestamp = 1,
#'                              units = "ratio", sequence = 0)
#' receipt$names
#' biofeedbackStop(scope)
#' @export
biofeedbackUpdate <- function(
    scope,
    values,
    timestamp,
    names = base::names(values),
    units = NULL,
    sequence = NULL) {
  .biofeedback_assert(scope)
  .biofeedback_require_idle(scope)
  if (!identical(scope$state$lifecycle, "running")) {
    .biofeedback_abort(
      "biofeedbackUpdate requires a running scope",
      "PhysioStream_biofeedback_state_error"
    )
  }
  supplied_names <- if (missing(names)) base::names(values) else names
  values <- .biofeedback_numeric(values, "values")
  names <- .biofeedback_names(supplied_names, "names")
  base::names(values) <- names
  if (!length(values) || length(values) != length(names) ||
      !identical(base::names(values), names)) {
    .biofeedback_abort(
      "`values` must be a non-empty exactly named vector",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  timestamp <- .biofeedback_scalar(timestamp, "timestamp")
  if (!is.null(units)) {
    units <- .biofeedback_strings(units, "units")
    if (length(units) != length(values)) {
      .biofeedback_abort(
        "`units` must align exactly with `values`",
        "PhysioStream_biofeedback_validation_error"
      )
    }
  }
  if (!is.null(sequence)) {
    sequence <- .biofeedback_scalar(
      sequence, "sequence", lower = 0, upper = 2^53, integer = TRUE
    )
  }
  descriptors <- scope$state$configuration$trace_descriptors
  external <- descriptors[
    vapply(descriptors, function(x) identical(x$type, "external"), logical(1))
  ]
  external_names <- unname(vapply(
    external, function(x) x$name, character(1)
  ))
  if (any(!names %in% external_names)) {
    .biofeedback_abort(
      "update names must exactly identify declared external traces",
      "PhysioStream_biofeedback_channel_error"
    )
  }
  external <- external[match(names, external_names)]
  declared_units <- unname(vapply(
    external, function(x) x$unit, character(1)
  ))
  if (!is.null(units) && !identical(units, declared_units)) {
    .biofeedback_abort(
      "`units` do not match the external trace contract",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  old <- .dsp_deep_copy(scope$state)
  if (!is.null(old$latest_signal_time) &&
      timestamp < old$latest_signal_time) {
    .biofeedback_abort(
      "external update timestamp is stale",
      "PhysioStream_biofeedback_timing_error"
    )
  }
  for (descriptor in external) {
    stat <- old$trace_stats[[descriptor$trace_ids]]
    if (!is.null(stat$last_timestamp) &&
        timestamp <= stat$last_timestamp) {
      .biofeedback_abort(
        "external trace timestamps must increase strictly",
        "PhysioStream_biofeedback_timing_error"
      )
    }
  }
  if (!is.null(sequence)) {
    expected <- if (is.null(old$latest_external_sequence)) {
      0
    } else {
      old$latest_external_sequence + 1
    }
    if (!identical(as.numeric(sequence), as.numeric(expected))) {
      .biofeedback_abort(
        "`sequence` must be contiguous from zero",
        "PhysioStream_biofeedback_timing_error"
      )
    }
  }
  old_histories <- scope$histories
  old_frame <- scope$latest_frame
  old_events <- scope$events
  old_clock <- scope$clock
  old_source <- .biofeedback_source_identity(scope$source)
  old_pipeline <- pipelineState(scope$pipeline)
  scope$busy <- TRUE
  committed <- FALSE
  on.exit({
    scope$busy <- FALSE
    if (!committed) {
      scope$histories <- old_histories
      scope$latest_frame <- old_frame
    }
  }, add = TRUE)

  histories <- .dsp_deep_copy(old_histories)
  candidate <- old
  for (i in seq_along(values)) {
    added <- .biofeedback_add_trace(
      histories, candidate, external[[i]]$trace_ids,
      timestamp, values[[i]]
    )
    histories <- added$histories
    candidate <- added$state
  }
  now <- .biofeedback_now(scope, old$latest_monotonic_ns)
  if (!identical(scope$state, old) ||
      !identical(scope$histories, old_histories) ||
      !identical(scope$latest_frame, old_frame) ||
      !identical(scope$events, old_events) ||
      !identical(scope$clock, old_clock) ||
      !identical(.biofeedback_source_identity(scope$source), old_source) ||
      !identical(pipelineState(scope$pipeline), old_pipeline)) {
    .biofeedback_abort(
      "biofeedback runtime changed during external update",
      "PhysioStream_biofeedback_state_error"
    )
  }
  candidate$latest_signal_time <- max(
    c(candidate$latest_signal_time, timestamp)
  )
  candidate$latest_external_sequence <- if (is.null(sequence)) {
    candidate$latest_external_sequence
  } else {
    as.numeric(sequence)
  }
  candidate$counters$external_updates <-
    candidate$counters$external_updates + 1
  candidate$counters$external_values <-
    candidate$counters$external_values + length(values)
  candidate$counters$frames <- candidate$counters$frames + 1
  candidate$latest_monotonic_ns <- now
  candidate$trace_stats <- .biofeedback_trace_stats(
    histories, candidate$latest_signal_time,
    candidate$configuration$window_seconds,
    candidate$configuration$max_points
  )
  candidate <- .biofeedback_append_audit(candidate, list(
    operation = "external_update",
    monotonic_ns = now,
    timestamp = timestamp,
    sequence = if (is.null(sequence)) NULL else as.numeric(sequence),
    names = names,
    frame_id = candidate$counters$frames
  ))
  candidate <- .biofeedback_seal(
    candidate, .biofeedback_state_limit, "biofeedback state"
  )
  frame <- .biofeedback_frame(
    scope, candidate, histories, scope$events, now
  )
  scope$histories <- histories
  scope$latest_frame <- frame
  scope$state <- candidate
  committed <- TRUE
  receipt <- list(
    scope_id = candidate$scope_id,
    sequence = if (is.null(sequence)) NULL else as.numeric(sequence),
    timestamp = timestamp,
    names = names,
    frame_id = candidate$counters$frames,
    state_sha256 = candidate$sha256
  )
  .biofeedback_seal(
    receipt, .biofeedback_state_limit, "biofeedback update receipt"
  )
}

#' Inspect a biofeedback frame or portable state
#'
#' @param scope A `BiofeedbackScope`.
#' @return A deep plain-list copy, or `NULL` before the first frame.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("left", "right"), nominal_srate = 100,
#'                    channel_units = c("uV", "uV"))
#' source <- loopbackSource(info, capacity = 256L)
#' scope <- biofeedbackScope(source, source_lifecycle = "own", launch = FALSE)
#' biofeedbackState(scope)$lifecycle
#' biofeedbackFrame(scope)
#' @name biofeedback-state
NULL

#' @rdname biofeedback-state
#' @export
biofeedbackFrame <- function(scope) {
  .biofeedback_assert(scope)
  if (is.null(scope$latest_frame)) {
    return(NULL)
  }
  .dsp_deep_copy(scope$latest_frame)
}

#' @rdname biofeedback-state
#' @export
biofeedbackState <- function(scope) {
  .biofeedback_assert(scope)
  .dsp_deep_copy(scope$state)
}

#' @rdname biofeedback-lifecycle
#' @export
biofeedbackStop <- function(scope) {
  .biofeedback_assert(scope)
  .biofeedback_require_idle(scope)
  if (scope$state$lifecycle %in% c("stopped", "error_stopped")) {
    return(invisible(scope))
  }
  if (!identical(scope$state$lifecycle, "running")) {
    .biofeedback_abort(
      "biofeedbackStop requires a running scope",
      "PhysioStream_biofeedback_state_error"
    )
  }
  old <- .dsp_deep_copy(scope$state)
  scope$busy <- TRUE
  on.exit({
    scope$busy <- FALSE
  }, add = TRUE)
  close_failed <- FALSE
  if (identical(old$configuration$source_lifecycle, "own") &&
      identical(streamState(scope$source), "open")) {
    closed <- tryCatch(
      {
        scope$source <- streamClose(scope$source)
        TRUE
      },
      error = function(e) FALSE
    )
    close_failed <- !closed
  }
  now <- .biofeedback_now(scope, old$latest_monotonic_ns)
  candidate <- old
  candidate$lifecycle <- if (close_failed) {
    "error_stopped"
  } else {
    "stopped"
  }
  candidate$last_error_code <- if (close_failed) {
    "PhysioStream_biofeedback_source_close_error"
  } else {
    NULL
  }
  candidate$latest_monotonic_ns <- now
  candidate <- .biofeedback_append_audit(candidate, list(
    operation = "stop",
    monotonic_ns = now,
    close_failed = close_failed
  ))
  scope$state <- .biofeedback_seal(
    candidate, .biofeedback_state_limit, "biofeedback state"
  )
  if (close_failed) {
    .biofeedback_abort(
      "owned source close failed",
      "PhysioStream_biofeedback_source_error"
    )
  }
  invisible(scope)
}

.biofeedback_shiny_paths <- function() {
  root <- system.file(
    "shiny", "biofeedback", package = "PhysioStream",
    mustWork = FALSE
  )
  paths <- list(
    app = file.path(root, "app.R"),
    css = file.path(root, "www", "biofeedback.css"),
    js = file.path(root, "www", "biofeedback.js")
  )
  if (!nzchar(root) || any(!file.exists(unlist(paths)))) {
    .biofeedback_abort(
      "installed biofeedback Shiny assets are unavailable",
      "PhysioStream_biofeedback_capability_error"
    )
  }
  paths
}

.biofeedback_launch <- function(scope) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .biofeedback_abort(
      "launching the biofeedback app requires the suggested package `shiny`",
      "PhysioStream_biofeedback_capability_error"
    )
  }
  .biofeedback_assert(scope)
  if (identical(scope$state$lifecycle, "created")) {
    biofeedbackStart(scope)
  }
  paths <- .biofeedback_shiny_paths()
  app_env <- new.env(parent = asNamespace("PhysioStream"))
  sys.source(paths$app, envir = app_env)
  builder <- app_env$.physiostream_biofeedback_app
  if (!is.function(builder)) {
    .biofeedback_abort(
      "installed biofeedback app entry point is invalid",
      "PhysioStream_biofeedback_capability_error"
    )
  }
  css <- paste(readLines(paths$css, warn = FALSE), collapse = "\n")
  js <- paste(readLines(paths$js, warn = FALSE), collapse = "\n")
  video <- .video_prepare_for_shiny(scope$video)
  on.exit(.video_cleanup_shiny(video), add = TRUE)
  app <- builder(scope, css, js, video)
  on.exit({
    if (identical(scope$state$lifecycle, "running")) {
      try(biofeedbackStop(scope), silent = TRUE)
    }
  }, add = TRUE)
  shiny::runApp(
    app,
    host = scope$state$configuration$host,
    port = scope$state$configuration$port,
    launch.browser = scope$state$configuration$browser
  )
}

#' @export
print.BiofeedbackScope <- function(x, ...) {
  .biofeedback_assert(x)
  cat(sprintf(
    "<BiofeedbackScope: state=%s, traces=%d, frames=%s, time=%s>\n",
    x$state$lifecycle,
    length(x$state$configuration$trace_meta),
    x$state$counters$frames,
    if (is.null(x$state$latest_signal_time)) {
      "none"
    } else {
      format(x$state$latest_signal_time, digits = 8)
    }
  ))
  invisible(x)
}
