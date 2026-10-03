.loopback_validity <- function(object) {
  if (!methods::is(object@buffer, "RingBuffer")) {
    return("`buffer` must be a RingBuffer")
  }
  if (!identical(object@info, streamInfo(object@buffer))) {
    return("endpoint and buffer StreamInfo must be identical")
  }
  TRUE
}

#' Deterministic in-process stream source
#'
#' @slot buffer Live native ring buffer.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' src <- loopbackSource(info, capacity = 16L)
#' is(src, "LoopbackSource")
#' @exportClass LoopbackSource
methods::setClass(
  "LoopbackSource",
  contains = "StreamSource",
  slots = c(buffer = "RingBuffer"),
  validity = .loopback_validity
)

#' Construct a loopback source
#'
#' @param info Numeric `StreamInfo`.
#' @param capacity Exact positive ring capacity.
#' @return A closed-over loopback source in the `"created"` state.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' src <- streamOpen(loopbackSource(info, capacity = 32L))
#' streamState(src)
#' @export
loopbackSource <- function(info, capacity = 1024L) {
  if (!methods::is(info, "StreamInfo") || identical(info@dtype, "string")) {
    .stream_abort("loopback sources require numeric StreamInfo",
                  "PhysioStream_validation_error")
  }
  methods::new(
    "LoopbackSource",
    info = info,
    state = "created",
    audit = list(),
    buffer = ringBuffer(info, capacity)
  )
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamOpen", "LoopbackSource", function(x, ...) {
  invisible(.stream_transition(x, "open", "open"))
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamClose", "LoopbackSource", function(x, ...) {
  invisible(.stream_transition(x, "closed", "close"))
})

.stream_require_open <- function(x, operation) {
  if (!identical(streamState(x), "open")) {
    .stream_abort(
      sprintf("%s requires an open %s (state is `%s`)",
              operation, class(x)[[1L]], streamState(x)),
      "PhysioStream_state_error"
    )
  }
  invisible(TRUE)
}

#' Feed a loopback source
#'
#' @param x An open `LoopbackSource`.
#' @param samples A finite sample-by-channel real matrix.
#' @param timestamps Strictly increasing finite timestamps.
#' @return `x`, invisibly. The native buffer is modified by reference.
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' src <- streamOpen(loopbackSource(info, capacity = 32L))
#' loopbackFeed(src, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
#' streamPull(src)$count
#' @export
loopbackFeed <- function(x, samples, timestamps) {
  if (!methods::is(x, "LoopbackSource")) {
    .stream_abort("`x` must be a LoopbackSource",
                  "PhysioStream_validation_error")
  }
  .stream_require_open(x, "loopbackFeed")
  ringPush(x@buffer, samples, timestamps)
  invisible(x)
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamPull", "LoopbackSource", function(x, ...) {
  .stream_require_open(x, "streamPull")
  ringPull(x@buffer, ...)
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamPush", "LoopbackSource", function(x, ...) {
  .stream_require_open(x, "streamPush")
  loopbackFeed(x, ...)
})
