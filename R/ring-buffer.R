.ring_memory_limit <- 2 * 1024^3

.ring_exact_integer <- function(x, name, minimum = 0) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x == floor(x) && x >= minimum && x <= .Machine$integer.max
  if (!ok) {
    .stream_abort(sprintf("`%s` must be one exact integer in [%s, %s]",
                          name, minimum, .Machine$integer.max),
                  "PhysioStream_validation_error")
  }
  as.integer(x)
}

.ring_call <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (inherits(e, "PhysioStream_error")) {
        stop(e)
      }
      .stream_abort(conditionMessage(e), "PhysioStream_buffer_error")
    }
  )
}

.ring_buffer_validity <- function(object) {
  if (!methods::is(object@ptr, "externalptr")) {
    return("`ptr` must be a native external pointer")
  }
  if (!methods::is(object@info, "StreamInfo")) {
    return("`info` must be a StreamInfo")
  }
  if (!is.integer(object@capacity) || length(object@capacity) != 1L ||
      is.na(object@capacity) || object@capacity < 1L) {
    return("`capacity` must be one positive integer")
  }
  if (!identical(object@schema_version, .stream_schema_version)) {
    return("ring-buffer schema mismatch")
  }
  TRUE
}

#' Live bounded ring buffer
#'
#' Copying a `RingBuffer` wrapper aliases the same native buffer. The native
#' queue is not serialized; a restored wrapper fails on its first operation.
#'
#' @slot ptr Protected native pointer.
#' @slot info Immutable stream metadata.
#' @slot capacity Fixed sample capacity.
#' @slot schema_version Wrapper schema.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 8L)
#' ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
#' ringFill(buffer)
#' @exportClass RingBuffer
methods::setClass(
  "RingBuffer",
  slots = c(
    ptr = "externalptr",
    info = "StreamInfo",
    capacity = "integer",
    schema_version = "character"
  ),
  validity = .ring_buffer_validity
)

#' Construct a native ring buffer
#'
#' @param info Numeric `StreamInfo`.
#' @param capacity Exact positive sample capacity.
#' @return A live `RingBuffer`.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 16L)
#' ringCapacity(buffer)
#' @export
ringBuffer <- function(info, capacity) {
  if (!methods::is(info, "StreamInfo")) {
    .stream_abort("`info` must be a StreamInfo",
                  "PhysioStream_validation_error")
  }
  methods::validObject(info)
  if (identical(info@dtype, "string")) {
    .stream_abort("string streams require a marker queue transport",
                  "PhysioStream_validation_error")
  }
  capacity <- .ring_exact_integer(capacity, "capacity", 1)
  bytes_per_sample <- 8 * info@n_channels + 8 + 8
  bytes <- as.double(capacity) * as.double(bytes_per_sample)
  if (!is.finite(bytes) || bytes > .ring_memory_limit) {
    .stream_abort("ring allocation exceeds the 2 GiB memory ceiling",
                  "PhysioStream_validation_error")
  }
  ptr <- .ring_call(cpp_ring_create(
    info@n_channels, capacity, info@dtype, .ring_memory_limit
  ))
  methods::new(
    "RingBuffer",
    ptr = ptr,
    info = info,
    capacity = capacity,
    schema_version = .stream_schema_version
  )
}

#' @rdname streamInfo
#' @export
methods::setMethod("streamInfo", "RingBuffer", function(name, ...) name@info)

#' Ring-buffer capacity and occupancy
#'
#' @param x A live `RingBuffer`.
#' @return `ringCapacity()` and `ringFill()` return integer scalars.
#'   `ringStats()` returns a serializable named list.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 8L)
#' ringPush(buffer, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
#' ringCapacity(buffer)
#' ringFill(buffer)
#' ringStats(buffer)$fill
#' @name ring-state
NULL

#' @rdname ring-state
#' @export
ringCapacity <- function(x) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  x@capacity
}

#' @rdname ring-state
#' @export
ringFill <- function(x) as.integer(ringStats(x)$fill)

#' @rdname ring-state
#' @export
ringStats <- function(x) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  .ring_call(cpp_ring_stats(x@ptr))
}

.ring_validate_samples <- function(x, samples, timestamps) {
  if (!is.matrix(samples) || typeof(samples) != "double") {
    .stream_abort("`samples` must be a real numeric matrix",
                  "PhysioStream_validation_error")
  }
  if (nrow(samples) < 1L || ncol(samples) != x@info@n_channels) {
    .stream_abort("`samples` must have rows and the exact stream channel count",
                  "PhysioStream_validation_error")
  }
  if (any(!is.finite(samples))) {
    .stream_abort("`samples` must be finite",
                  "PhysioStream_validation_error")
  }
  if (typeof(timestamps) != "double" || !is.vector(timestamps) ||
      length(timestamps) != nrow(samples) || any(!is.finite(timestamps))) {
    .stream_abort("`timestamps` must be a finite real vector matching rows",
                  "PhysioStream_validation_error")
  }
  if (length(timestamps) > 1L && any(diff(timestamps) <= 0)) {
    .stream_abort("`timestamps` must be strictly increasing",
                  "PhysioStream_validation_error")
  }
  invisible(TRUE)
}

#' Push samples into a ring buffer
#'
#' The push is transactional. When unread rows are overwritten, the oldest
#' rows are dropped and counted. Matrices are always sample by channel.
#'
#' @param x A live `RingBuffer`.
#' @param samples A finite real matrix, sample by channel.
#' @param timestamps Strictly increasing finite timestamps.
#' @return Updated serializable ring statistics, invisibly.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 8L)
#' ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
#' ringFill(buffer)
#' @export
ringPush <- function(x, samples, timestamps) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  .ring_validate_samples(x, samples, timestamps)
  invisible(.ring_call(cpp_ring_push(x@ptr, samples, timestamps)))
}

.ring_n <- function(n, fill) {
  n <- .ring_exact_integer(n, "n", 0)
  min(n, fill)
}

#' Pull or inspect buffered samples
#'
#' @param x A live `RingBuffer`.
#' @param n Non-negative exact number of rows. Requests larger than the current
#'   fill return all available rows.
#' @param from Exact peek side, either `"oldest"` or `"latest"`.
#' @return A named list with `samples`, `timestamps`, `sequence`, `count`, and
#'   `stats_after`.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 8L)
#' ringPush(buffer, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
#' ringPeek(buffer, 2L)$samples
#' ringPull(buffer)$sequence
#' @name ring-read
NULL

#' @rdname ring-read
#' @export
ringPull <- function(x, n = ringFill(x)) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  n <- .ring_n(n, ringFill(x))
  .ring_call(cpp_ring_pull(x@ptr, n))
}

#' @rdname ring-read
#' @export
ringPeek <- function(x, n = ringFill(x), from = "oldest") {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  if (!is.character(from) || length(from) != 1L || is.na(from) ||
      !(from %in% c("oldest", "latest"))) {
    .stream_abort("`from` must be exactly 'oldest' or 'latest'",
                  "PhysioStream_validation_error")
  }
  n <- .ring_n(n, ringFill(x))
  .ring_call(cpp_ring_peek(x@ptr, n, from))
}

#' Reset unread ring-buffer state
#'
#' Reset clears unread rows and the last-timestamp constraint while preserving
#' lifetime counters and monotonic sequence identity.
#'
#' @param x A live `RingBuffer`.
#' @return Updated serializable ring statistics, invisibly.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' buffer <- ringBuffer(info, capacity = 8L)
#' ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
#' ringReset(buffer)
#' ringFill(buffer)
#' @export
ringReset <- function(x) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  invisible(.ring_call(cpp_ring_reset(x@ptr)))
}

.ring_finalize <- function(x) {
  if (!methods::is(x, "RingBuffer")) {
    .stream_abort("`x` must be a RingBuffer", "PhysioStream_validation_error")
  }
  invisible(cpp_ring_finalize(x@ptr))
}

.ring_native_stress <- function(iterations = 100000L, capacity = 1024L) {
  iterations <- .ring_exact_integer(iterations, "iterations", 1)
  capacity <- .ring_exact_integer(capacity, "capacity", 1)
  .ring_call(cpp_ring_stress(iterations, capacity))
}
