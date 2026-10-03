.lsl_marker_schema <- "1.0.0"

.lsl_marker_queue <- function(capacity, n_channels) {
  queue <- new.env(parent = emptyenv())
  queue$schema <- .lsl_marker_schema
  queue$capacity <- as.integer(capacity)
  queue$n_channels <- as.integer(n_channels)
  queue$timestamps <- numeric()
  queue$sequence <- numeric()
  queue$values <- matrix(character(), nrow = 0L, ncol = n_channels)
  queue$next_sequence <- 0
  queue$total_pushed <- 0
  queue$total_pulled <- 0
  queue$total_dropped <- 0
  queue$last_timestamp <- NULL
  queue
}

.lsl_marker_assert <- function(queue) {
  if (!is.environment(queue) ||
      !identical(queue$schema, .lsl_marker_schema) ||
      !is.integer(queue$capacity) || queue$capacity < 1L ||
      !is.integer(queue$n_channels) || queue$n_channels < 1L) {
    .stream_abort(
      "invalid LSL marker queue",
      "PhysioStream_lsl_lifetime_error"
    )
  }
  invisible(TRUE)
}

.lsl_marker_stats <- function(queue) {
  .lsl_marker_assert(queue)
  list(
    capacity = queue$capacity,
    fill = as.integer(length(queue$timestamps)),
    total_pushed = queue$total_pushed,
    total_pulled = queue$total_pulled,
    total_dropped = queue$total_dropped,
    next_sequence = queue$next_sequence,
    oldest_sequence = if (length(queue$sequence)) {
      queue$sequence[[1L]]
    } else {
      queue$next_sequence
    },
    newest_sequence = if (length(queue$sequence)) {
      queue$sequence[[length(queue$sequence)]]
    } else {
      NULL
    },
    loss_observed = queue$total_dropped > 0
  )
}

.lsl_marker_push <- function(queue, values, timestamps) {
  .lsl_marker_assert(queue)
  if (!is.matrix(values) || !is.character(values) || nrow(values) < 1L ||
      ncol(values) != queue$n_channels || anyNA(values)) {
    .stream_abort(
      "invalid marker payload",
      "PhysioStream_lsl_payload_error"
    )
  }
  if (typeof(timestamps) != "double" || !is.vector(timestamps) ||
      length(timestamps) != nrow(values) || any(!is.finite(timestamps)) ||
      (length(timestamps) > 1L && any(diff(timestamps) <= 0)) ||
      (!is.null(queue$last_timestamp) &&
       timestamps[[1L]] <= queue$last_timestamp)) {
    .stream_abort(
      "marker timestamps must increase strictly across pushes",
      "PhysioStream_lsl_payload_error"
    )
  }
  n <- nrow(values)
  if (queue$next_sequence + n - 1 > 2^53 - 1) {
    .stream_abort(
      "marker sequence exceeds exact R numeric identity",
      "PhysioStream_lsl_payload_error"
    )
  }
  sequence <- queue$next_sequence + seq.int(0, n - 1L)
  combined_values <- rbind(queue$values, values)
  combined_timestamps <- c(queue$timestamps, timestamps)
  combined_sequence <- c(queue$sequence, sequence)
  drop <- max(length(combined_timestamps) - queue$capacity, 0L)
  if (drop) {
    keep <- seq.int(drop + 1L, length(combined_timestamps))
    combined_values <- combined_values[keep, , drop = FALSE]
    combined_timestamps <- combined_timestamps[keep]
    combined_sequence <- combined_sequence[keep]
  }
  queue$values <- combined_values
  queue$timestamps <- combined_timestamps
  queue$sequence <- combined_sequence
  queue$next_sequence <- queue$next_sequence + n
  queue$total_pushed <- queue$total_pushed + n
  queue$total_dropped <- queue$total_dropped + drop
  queue$last_timestamp <- timestamps[[n]]
  invisible(.lsl_marker_stats(queue))
}

.lsl_marker_select <- function(queue, n, from = "oldest", consume = FALSE) {
  .lsl_marker_assert(queue)
  fill <- length(queue$timestamps)
  n <- min(.ring_exact_integer(n, "n", 0), fill)
  if (!n) {
    indices <- integer()
  } else if (identical(from, "oldest")) {
    indices <- seq_len(n)
  } else {
    indices <- seq.int(fill - n + 1L, fill)
  }
  if (consume && !identical(indices, seq_len(fill))) {
    .stream_abort(
      "`consume = TRUE` requires the complete oldest marker selection",
      "PhysioStream_state_error"
    )
  }
  selected <- list(
    values = queue$values[indices, , drop = FALSE],
    timestamps = queue$timestamps[indices],
    sequence = queue$sequence[indices],
    count = as.integer(n)
  )
  if (consume && fill) {
    queue$values <- matrix(
      character(), nrow = 0L, ncol = queue$n_channels
    )
    queue$timestamps <- numeric()
    queue$sequence <- numeric()
    queue$total_pulled <- queue$total_pulled + fill
  }
  selected$stats_after <- .lsl_marker_stats(queue)
  selected
}

.lsl_marker_json <- function(values, channel_names) {
  payload <- stats::setNames(as.list(values), channel_names)
  as.character(jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    force = TRUE,
    pretty = FALSE
  ))
}

#' Convert buffered LSL markers to PhysioEvents
#'
#' Marker timestamps remain in the LSL clock domain unless an explicit
#' `time_origin` is subtracted. Cross-stream alignment is handled separately.
#'
#' @param x An open string/irregular-rate `LSLInlet`.
#' @param n Optional newest marker count.
#' @param consume Whether to consume the complete oldest selection.
#' @param time_origin Finite scalar subtracted from LSL timestamps.
#' @param type Optional event type; defaults to the stream type.
#' @return A valid `PhysioExperiment::PhysioEvents`.
#' @examples
#' \donttest{
#' # Requires an open marker inlet bound to a live LSL marker stream.
#' if (lslAvailable()) {
#'   found <- lslResolveStreams(property = "type", value = "Markers",
#'                              timeout = 0.2)
#'   if (length(found)) {
#'     inlet <- streamOpen(lslInlet(found[[1]]))
#'     events <- lslMarkerEvents(inlet)
#'     streamClose(inlet)
#'   }
#' }
#' }
#' @export
lslMarkerEvents <- function(x, n = NULL, consume = FALSE, time_origin = 0,
                            type = NULL) {
  if (!methods::is(x, "LSLInlet") ||
      !identical(x@info@dtype, "string") ||
      x@info@nominal_srate != 0) {
    .stream_abort(
      "`x` must be a string irregular-rate LSLInlet",
      "PhysioStream_validation_error"
    )
  }
  .lsl_require_open(x, "lslMarkerEvents")
  consume <- .lsl_scalar_logical(consume, "consume")
  if (!is.numeric(time_origin) || length(time_origin) != 1L ||
      !is.finite(time_origin)) {
    .stream_abort(
      "`time_origin` must be one finite number",
      "PhysioStream_validation_error"
    )
  }
  event_type <- if (is.null(type)) x@info@type else {
    .stream_scalar_string(type, "type")
  }
  fill <- .lsl_marker_stats(x@buffer)$fill
  if (is.null(n)) {
    selected <- .lsl_marker_select(
      x@buffer, fill, from = "oldest", consume = consume
    )
  } else {
    requested <- .ring_exact_integer(n, "n", 0)
    selected <- .lsl_marker_select(
      x@buffer, min(requested, fill), from = "latest", consume = consume
    )
  }
  values <- if (!selected$count) {
    character()
  } else if (x@info@n_channels == 1L) {
    selected$values[, 1L]
  } else {
    vapply(seq_len(selected$count), function(i) {
      .lsl_marker_json(
        selected$values[i, , drop = TRUE],
        x@info@channel_names
      )
    }, character(1))
  }
  events <- PhysioExperiment::PhysioEvents(
    onset = selected$timestamps - time_origin,
    duration = rep(0, selected$count),
    type = rep(event_type, selected$count),
    value = values
  )
  attr(events, "lsl") <- list(
    original_timestamps = selected$timestamps,
    marker_sequence = selected$sequence,
    descriptor_sha256 = .lsl_info_identity(x@info),
    queue_stats = selected$stats_after,
    time_origin = as.numeric(time_origin),
    mapping_schema = .lsl_mapping_schema
  )
  methods::validObject(events)
  events
}
