.biofeedback_schema_version <- "1.0.0"
.biofeedback_state_limit <- 16 * 1024^2
.biofeedback_frame_limit <- 16 * 1024^2
.biofeedback_materialization_limit <- 512 * 1024^2
.biofeedback_audit_limit <- 1024L
.biofeedback_event_limit <- 1024L

.biofeedback_abort <- function(
    message,
    class = "PhysioStream_biofeedback_error") {
  .stream_abort(message, class)
}

.biofeedback_name <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      nchar(x, type = "bytes") > 256L) {
    .biofeedback_abort(
      sprintf("`%s` must be one bounded non-empty string", name),
      "PhysioStream_biofeedback_validation_error"
    )
  }
  x
}

.biofeedback_names <- function(x, name, allow_empty = FALSE) {
  if (!is.character(x) || anyNA(x) ||
      (!allow_empty && !length(x)) ||
      any(!nzchar(x)) ||
      any(nchar(x, type = "bytes") > 256L) ||
      anyDuplicated(x)) {
    .biofeedback_abort(
      sprintf("`%s` must contain unique bounded non-empty strings", name),
      "PhysioStream_biofeedback_validation_error"
    )
  }
  as.character(x)
}

.biofeedback_strings <- function(x, name, allow_empty = FALSE) {
  if (!is.character(x) || anyNA(x) ||
      (!allow_empty && !length(x)) ||
      any(!nzchar(x)) ||
      any(nchar(x, type = "bytes") > 256L)) {
    .biofeedback_abort(
      sprintf("`%s` must contain bounded non-empty strings", name),
      "PhysioStream_biofeedback_validation_error"
    )
  }
  as.character(x)
}

.biofeedback_scalar <- function(x, name, lower = -Inf, upper = Inf,
                                lower_open = FALSE, upper_open = FALSE,
                                integer = FALSE) {
  .dsp_scalar(
    x, name, lower = lower, upper = upper, lower_open = lower_open,
    upper_open = upper_open, integer = integer
  )
}

.biofeedback_enum <- function(x, choices, name) {
  .dsp_enum(x, choices, name)
}

.biofeedback_logical <- function(x, name) {
  .dsp_logical(x, name)
}

.biofeedback_numeric <- function(x, name, length = NULL,
                                 allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (is.factor(x) || !is.numeric(x) || is.object(x) ||
      !is.null(dim(x)) || any(!is.finite(x)) ||
      (!is.null(length) && base::length(x) != length)) {
    suffix <- if (is.null(length)) "" else sprintf(" of length %d", length)
    .biofeedback_abort(
      sprintf("`%s` must be a plain finite numeric vector%s", name, suffix),
      "PhysioStream_biofeedback_validation_error"
    )
  }
  as.numeric(x)
}

.biofeedback_hash <- function(x) {
  payload <- x
  payload$sha256 <- NULL
  digest::digest(
    serialize(payload, NULL, version = 3L),
    algo = "sha256",
    serialize = FALSE
  )
}

.biofeedback_seal <- function(x, limit, label) {
  if (!is.list(x) || is.object(x)) {
    .biofeedback_abort(
      sprintf("`%s` must be a plain list", label),
      "PhysioStream_biofeedback_state_error"
    )
  }
  x$schema_version <- .biofeedback_schema_version
  x$sha256 <- NULL
  bad <- .dsp_runtime_path(x, label)
  if (!is.null(bad)) {
    .biofeedback_abort(
      sprintf("unsupported runtime value at `%s`", bad),
      "PhysioStream_biofeedback_state_error"
    )
  }
  bytes <- length(serialize(x, NULL, version = 3L))
  if (bytes > limit) {
    .biofeedback_abort(
      sprintf("`%s` exceeds its governed serialization ceiling", label),
      "PhysioStream_biofeedback_resource_error"
    )
  }
  x$sha256 <- .biofeedback_hash(x)
  if (length(serialize(x, NULL, version = 3L)) > limit) {
    .biofeedback_abort(
      sprintf("`%s` exceeds its governed serialization ceiling", label),
      "PhysioStream_biofeedback_resource_error"
    )
  }
  x
}

.biofeedback_validate_sealed <- function(x, limit, label) {
  if (!is.list(x) || is.object(x) ||
      !identical(x$schema_version, .biofeedback_schema_version) ||
      !is.character(x$sha256) || length(x$sha256) != 1L ||
      is.na(x$sha256) || !nzchar(x$sha256)) {
    .biofeedback_abort(
      sprintf("`%s` has an invalid schema", label),
      "PhysioStream_biofeedback_state_error"
    )
  }
  bad <- .dsp_runtime_path(x, label)
  if (!is.null(bad) ||
      length(serialize(x, NULL, version = 3L)) > limit ||
      !identical(x$sha256, .biofeedback_hash(x))) {
    .biofeedback_abort(
      sprintf("`%s` failed integrity validation", label),
      "PhysioStream_biofeedback_state_error"
    )
  }
  invisible(TRUE)
}

.biofeedback_source_descriptor <- function(source) {
  info <- streamInfo(source)
  list(
    class = class(source)[[1L]],
    name = info@name,
    type = info@type,
    source_id = info@source_id,
    clock_domain = info@clock_domain,
    channel_names = info@channel_names,
    channel_units = info@channel_units,
    nominal_srate = info@nominal_srate,
    dtype = info@dtype,
    schema = info@schema_version
  )
}

.biofeedback_trace_id <- function(kind, name, channel = NULL) {
  if (is.null(channel)) {
    paste(kind, name, sep = ":")
  } else {
    paste(kind, name, channel, sep = ":")
  }
}

.biofeedback_trace_meta <- function(id, name, kind, channel, unit,
                                    gain, threshold, target_range) {
  list(
    id = id,
    name = name,
    kind = kind,
    channel = channel,
    unit = unit,
    gain = gain,
    threshold = threshold,
    target_range = target_range
  )
}

.biofeedback_normalize_derived <- function(derived, source_channels,
                                           source_units, pipeline_state) {
  if (!is.list(derived) || is.object(derived)) {
    .biofeedback_abort(
      "`derived` must be a plain list",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  if (!length(derived)) {
    return(list(descriptors = list(), trace_meta = list()))
  }
  derived_names <- names(derived)
  .biofeedback_names(derived_names, "names(derived)")
  operation_names <- vapply(
    pipeline_state$operations, function(x) x$name, character(1)
  )
  descriptors <- vector("list", length(derived))
  names(descriptors) <- derived_names
  trace_meta <- list()
  for (i in seq_along(derived)) {
    value <- derived[[i]]
    label <- paste0("derived$", derived_names[[i]])
    if (!is.list(value) || is.object(value)) {
      .biofeedback_abort(
        sprintf("`%s` must be a plain list", label),
        "PhysioStream_biofeedback_validation_error"
      )
    }
    type <- .biofeedback_enum(
      value$type,
      c("emg_rms", "band_power", "external"),
      paste0(label, "$type")
    )
    gain <- if (is.null(value$gain)) 1 else .biofeedback_scalar(
      value$gain, paste0(label, "$gain"), lower = 0, lower_open = TRUE
    )
    threshold <- .biofeedback_numeric(
      value$threshold, paste0(label, "$threshold"),
      length = 1L, allow_null = TRUE
    )
    target_range <- .biofeedback_numeric(
      value$target_range, paste0(label, "$target_range"),
      length = 2L, allow_null = TRUE
    )
    if (!is.null(target_range) && target_range[[1L]] >= target_range[[2L]]) {
      .biofeedback_abort(
        sprintf("`%s$target_range` must be strictly increasing", label),
        "PhysioStream_biofeedback_validation_error"
      )
    }
    unit <- .biofeedback_name(value$unit, paste0(label, "$unit"))
    if (identical(type, "external")) {
      if (!is.null(value$channels) || !is.null(value$operation_name)) {
        .biofeedback_abort(
          sprintf("`%s` external traces cannot bind channels or operations",
                  label),
          "PhysioStream_biofeedback_validation_error"
        )
      }
      id <- .biofeedback_trace_id("external", derived_names[[i]])
      descriptor <- list(
        name = derived_names[[i]],
        type = type,
        channels = character(),
        output_channels = character(),
        operation_name = NULL,
        unit = unit,
        gain = gain,
        threshold = threshold,
        target_range = target_range,
        trace_ids = id
      )
      trace_meta[[id]] <- .biofeedback_trace_meta(
        id, derived_names[[i]], type, "", unit, gain, threshold, target_range
      )
    } else {
      channels <- .biofeedback_names(
        value$channels, paste0(label, "$channels")
      )
      if (any(!channels %in% source_channels)) {
        .biofeedback_abort(
          sprintf("`%s$channels` must exactly match source channels", label),
          "PhysioStream_biofeedback_channel_error"
        )
      }
      operation_name <- .biofeedback_name(
        value$operation_name, paste0(label, "$operation_name")
      )
      if (!length(operation_names) ||
          !identical(operation_name, tail(operation_names, 1L))) {
        .biofeedback_abort(
          sprintf("`%s$operation_name` must identify the final operation",
                  label),
          "PhysioStream_biofeedback_validation_error"
        )
      }
      operation <- tail(pipeline_state$operations, 1L)[[1L]]
      if (!identical(operation$type, "bandpass_rms")) {
        .biofeedback_abort(
          sprintf("`%s$operation_name` must bind a bandpassRmsOp", label),
          "PhysioStream_biofeedback_validation_error"
        )
      }
      channel_units <- source_units[match(channels, source_channels)]
      if (anyDuplicated(channel_units)) {
        channel_units <- unique(channel_units)
      }
      expected_unit <- if (identical(type, "band_power")) {
        paste0(channel_units, "^2")
      } else {
        channel_units
      }
      if (length(expected_unit) != 1L || !identical(unit, expected_unit)) {
        .biofeedback_abort(
          sprintf("`%s$unit` does not match the selected source unit", label),
          "PhysioStream_biofeedback_validation_error"
        )
      }
      output_channels <- paste0(channels, "_rms")
      trace_ids <- vapply(
        channels,
        function(channel) {
          .biofeedback_trace_id(type, derived_names[[i]], channel)
        },
        character(1)
      )
      descriptor <- list(
        name = derived_names[[i]],
        type = type,
        channels = channels,
        output_channels = output_channels,
        operation_name = operation_name,
        unit = unit,
        gain = gain,
        threshold = threshold,
        target_range = target_range,
        trace_ids = trace_ids
      )
      for (j in seq_along(channels)) {
        trace_meta[[trace_ids[[j]]]] <- .biofeedback_trace_meta(
          trace_ids[[j]], derived_names[[i]], type, channels[[j]], unit,
          gain, threshold, target_range
        )
      }
    }
    descriptors[[i]] <- descriptor
  }
  list(descriptors = descriptors, trace_meta = trace_meta)
}

.biofeedback_build_trace_contract <- function(channels, units, gain,
                                               threshold, target_range,
                                               derived, pipeline_state) {
  raw_meta <- list()
  for (i in seq_along(channels)) {
    id <- .biofeedback_trace_id("raw", channels[[i]])
    raw_meta[[id]] <- .biofeedback_trace_meta(
      id, channels[[i]], "raw", channels[[i]], units[[i]],
      gain, threshold, target_range
    )
  }
  normalized <- .biofeedback_normalize_derived(
    derived, channels, units, pipeline_state
  )
  trace_meta <- c(raw_meta, normalized$trace_meta)
  if (anyDuplicated(names(trace_meta))) {
    .biofeedback_abort(
      "trace identifiers must be unique",
      "PhysioStream_biofeedback_validation_error"
    )
  }
  list(
    descriptors = normalized$descriptors,
    trace_meta = trace_meta
  )
}

.biofeedback_new_history <- function(meta) {
  columns <- c(
    "bucket",
    "first_timestamp", "first_value", "first_order",
    "minimum_timestamp", "minimum_value", "minimum_order",
    "maximum_timestamp", "maximum_value", "maximum_order",
    "last_timestamp", "last_value", "last_order"
  )
  list(
    meta = meta,
    buckets = matrix(
      numeric(), nrow = 0L, ncol = length(columns),
      dimnames = list(NULL, columns)
    )
  )
}

.biofeedback_bucket_key <- function(timestamp, width) {
  index <- floor(timestamp / width)
  if (!is.finite(index) || abs(index) > 2^53) {
    .biofeedback_abort(
      "signal time cannot be represented in the display bucket domain",
      "PhysioStream_biofeedback_timing_error"
    )
  }
  sprintf("%.0f", index)
}

.biofeedback_history_add <- function(history, timestamps, values, orders,
                                     width, cutoff) {
  if (length(timestamps) != length(values) ||
      length(values) != length(orders)) {
    .biofeedback_abort(
      "trace append vectors have inconsistent lengths",
      "PhysioStream_biofeedback_state_error"
    )
  }
  buckets <- history$buckets
  if (length(timestamps)) {
    for (i in seq_along(timestamps)) {
      index <- floor(timestamps[[i]] / width)
      if (!is.finite(index) || abs(index) > 2^53) {
        .biofeedback_abort(
          "signal time cannot be represented in the display bucket domain",
          "PhysioStream_biofeedback_timing_error"
        )
      }
      n_bucket <- nrow(buckets)
      if (!n_bucket || index > buckets[n_bucket, "bucket"]) {
        row <- c(
          index,
          timestamps[[i]], values[[i]], orders[[i]],
          timestamps[[i]], values[[i]], orders[[i]],
          timestamps[[i]], values[[i]], orders[[i]],
          timestamps[[i]], values[[i]], orders[[i]]
        )
        buckets <- rbind(buckets, row)
      } else if (index == buckets[n_bucket, "bucket"]) {
        if (values[[i]] < buckets[n_bucket, "minimum_value"]) {
          buckets[n_bucket, c(
            "minimum_timestamp", "minimum_value", "minimum_order"
          )] <- c(timestamps[[i]], values[[i]], orders[[i]])
        }
        if (values[[i]] > buckets[n_bucket, "maximum_value"]) {
          buckets[n_bucket, c(
            "maximum_timestamp", "maximum_value", "maximum_order"
          )] <- c(timestamps[[i]], values[[i]], orders[[i]])
        }
        buckets[n_bucket, c(
          "last_timestamp", "last_value", "last_order"
        )] <- c(timestamps[[i]], values[[i]], orders[[i]])
      } else {
        .biofeedback_abort(
          "trace bucket order moved backwards",
          "PhysioStream_biofeedback_timing_error"
        )
      }
    }
  }
  if (nrow(buckets)) {
    buckets <- buckets[
      buckets[, "last_timestamp"] >= cutoff, , drop = FALSE
    ]
  }
  history$buckets <- buckets
  history
}

.biofeedback_history_points <- function(history, cutoff, max_points) {
  buckets <- history$buckets
  if (!nrow(buckets)) {
    return(list(timestamps = numeric(), values = numeric()))
  }
  timestamps <- c(
    buckets[, "first_timestamp"],
    buckets[, "minimum_timestamp"],
    buckets[, "maximum_timestamp"],
    buckets[, "last_timestamp"]
  )
  values <- c(
    buckets[, "first_value"],
    buckets[, "minimum_value"],
    buckets[, "maximum_value"],
    buckets[, "last_value"]
  )
  point_order <- c(
    buckets[, "first_order"],
    buckets[, "minimum_order"],
    buckets[, "maximum_order"],
    buckets[, "last_order"]
  )
  keep <- timestamps >= cutoff & !duplicated(point_order)
  timestamps <- timestamps[keep]
  values <- values[keep]
  point_order <- point_order[keep]
  if (!length(timestamps)) {
    return(list(timestamps = numeric(), values = numeric()))
  }
  ordered <- order(point_order)
  timestamps <- timestamps[ordered]
  values <- values[ordered]
  if (length(timestamps) > max_points) {
    keep <- tail(seq_along(timestamps), max_points)
    timestamps <- timestamps[keep]
    values <- values[keep]
  }
  list(
    timestamps = unname(timestamps),
    values = unname(values)
  )
}

.biofeedback_trace_stats <- function(histories, latest_time, window_seconds,
                                     max_points) {
  cutoff <- latest_time - window_seconds
  lapply(histories, function(history) {
    points <- .biofeedback_history_points(history, cutoff, max_points)
    list(
      n_points = as.numeric(length(points$values)),
      first_timestamp = if (length(points$timestamps)) {
        points$timestamps[[1L]]
      } else {
        NULL
      },
      last_timestamp = if (length(points$timestamps)) {
        tail(points$timestamps, 1L)
      } else {
        NULL
      }
    )
  })
}

.biofeedback_append_audit <- function(state, record) {
  state$audit[[length(state$audit) + 1L]] <- record
  if (length(state$audit) > .biofeedback_audit_limit) {
    excess <- length(state$audit) - .biofeedback_audit_limit
    state$audit <- tail(state$audit, .biofeedback_audit_limit)
    state$counters$audit_truncated <-
      state$counters$audit_truncated + excess
  }
  state
}

.biofeedback_source_stats <- function(source) {
  if (methods::is(source, "LoopbackSource")) {
    stats <- ringStats(source@buffer)
    return(list(
      fill = as.numeric(stats$fill),
      capacity = as.numeric(stats$capacity),
      pushed = as.numeric(stats$total_pushed),
      pulled = as.numeric(stats$total_pulled),
      overwritten = as.numeric(stats$total_dropped)
    ))
  }
  list(
    fill = NULL,
    capacity = NULL,
    pushed = NULL,
    pulled = NULL,
    overwritten = NULL
  )
}

.biofeedback_source_identity <- function(source) {
  audit <- if ("audit" %in% methods::slotNames(source)) {
    .dsp_deep_copy(methods::slot(source, "audit"))
  } else {
    NULL
  }
  list(
    descriptor = .biofeedback_source_descriptor(source),
    state = streamState(source),
    audit = audit
  )
}

.biofeedback_pipeline_stats <- function(pipeline) {
  state <- pipelineState(pipeline)
  list(
    queued = as.numeric(length(state$queue)),
    accepted_chunks = state$counters$accepted_chunks,
    processed_chunks = state$counters$processed_chunks,
    dropped_chunks = state$counters$dropped_chunks,
    dropped_samples = state$counters$dropped_samples,
    latest_end_to_end_ms = if (length(state$latency$end_to_end_ms)) {
      tail(state$latency$end_to_end_ms, 1L)
    } else {
      NULL
    },
    latency_budget_exceeded = if (length(state$latency$budget_exceeded)) {
      isTRUE(tail(state$latency$budget_exceeded, 1L))
    } else {
      FALSE
    },
    sha256 = state$sha256
  )
}

.biofeedback_frame <- function(scope, state, histories, events,
                               monotonic_ns) {
  latest_time <- state$latest_signal_time
  cutoff <- if (is.null(latest_time)) {
    -Inf
  } else {
    latest_time - state$configuration$window_seconds
  }
  traces <- lapply(histories, function(history) {
    points <- .biofeedback_history_points(
      history, cutoff, state$configuration$max_points
    )
    c(history$meta, points)
  })
  source_stats <- .biofeedback_source_stats(scope$source)
  pipeline_stats <- .biofeedback_pipeline_stats(scope$pipeline)
  initial_source <- state$initial_source_stats
  initial_pipeline <- state$initial_pipeline_stats
  source_stats$session_overwritten <- if (
      is.null(source_stats$overwritten) ||
      is.null(initial_source$overwritten)
    ) {
    NULL
  } else {
    source_stats$overwritten - initial_source$overwritten
  }
  pipeline_stats$session_dropped_chunks <-
    pipeline_stats$dropped_chunks - initial_pipeline$dropped_chunks
  pipeline_stats$session_dropped_samples <-
    pipeline_stats$dropped_samples - initial_pipeline$dropped_samples
  retained_points <- sum(vapply(
    traces, function(trace) length(trace$values), numeric(1)
  ))
  frame <- list(
    frame_id = state$counters$frames,
    scope_id = state$scope_id,
    clock_domain = state$configuration$source$clock_domain,
    signal_time = latest_time,
    created_monotonic_ns = monotonic_ns,
    traces = traces,
    targets = Filter(
      function(trace) {
        !is.null(trace$threshold) || !is.null(trace$target_range)
      },
      traces
    ),
    events = events,
    diagnostics = list(
      source = source_stats,
      pipeline = pipeline_stats,
      display = list(
        retained_points = as.numeric(retained_points),
        accepted_points = state$counters$accepted_trace_points,
        reduced_or_expired_points = max(
          0, state$counters$accepted_trace_points - retained_points
        ),
        external_update_drops = 0
      ),
      counters = state$counters
    ),
    state_sha256 = state$sha256
  )
  .biofeedback_seal(frame, .biofeedback_frame_limit, "biofeedback frame")
}

.biofeedback_validate_frame <- function(frame) {
  .biofeedback_validate_sealed(
    frame, .biofeedback_frame_limit, "biofeedback frame"
  )
  invisible(TRUE)
}
