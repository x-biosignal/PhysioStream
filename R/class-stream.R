.stream_states <- c("created", "open", "closed", "error")

.stream_endpoint_validity <- function(object) {
  if (!methods::is(object@info, "StreamInfo")) {
    return("`info` must be a valid StreamInfo")
  }
  if (!is.character(object@state) || length(object@state) != 1L ||
      is.na(object@state) || !(object@state %in% .stream_states)) {
    return("`state` must be one exact endpoint state")
  }
  if (!is.list(object@audit)) {
    return("`audit` must be a list")
  }
  TRUE
}

#' Virtual stream endpoint
#'
#' @slot info Immutable `StreamInfo`.
#' @slot state Exact lifecycle state.
#' @slot audit Append-only state transition records.
#' @examples
#' # StreamEndpoint is virtual; a loopback source is a concrete endpoint.
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = "C3", nominal_srate = 100)
#' src <- loopbackSource(info)
#' is(src, "StreamEndpoint")
#' streamState(src)
#' @exportClass StreamEndpoint
methods::setClass(
  "StreamEndpoint",
  slots = c(info = "StreamInfo", state = "character", audit = "list"),
  contains = "VIRTUAL",
  validity = .stream_endpoint_validity
)

#' Virtual stream source
#'
#' Transport backends extend this class and the stream-operation generics.
#'
#' @examples
#' # StreamSource is virtual; loopbackSource() returns a concrete instance.
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = "C3", nominal_srate = 100)
#' is(loopbackSource(info), "StreamSource")
#' @exportClass StreamSource
methods::setClass("StreamSource", contains = c("StreamEndpoint", "VIRTUAL"))

#' Virtual stream sink
#'
#' Transport backends extend this class and the stream-operation generics.
#'
#' @examples
#' # StreamSink is virtual; transport outlets such as lslOutlet() extend it.
#' isVirtualClass("StreamSink")
#' @exportClass StreamSink
methods::setClass("StreamSink", contains = c("StreamEndpoint", "VIRTUAL"))

#' @rdname streamInfo
#' @export
methods::setMethod("streamInfo", "StreamEndpoint", function(name, ...) name@info)

#' Stream endpoint lifecycle and transport operations
#'
#' Transport packages extend these generics. Base virtual endpoints provide no
#' implicit transport.
#'
#' @param x A stream endpoint.
#' @param timeout Optional finite non-negative LSL inlet open timeout.
#' @param ... Backend-specific arguments.
#' @return Backend-specific output.
#' @examples
#' # Drive the lifecycle on a device-free loopback source.
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' src <- streamOpen(loopbackSource(info, capacity = 16L))
#' streamState(src)
#' loopbackFeed(src, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
#' streamPull(src)$samples
#' src <- streamClose(src)
#' streamState(src)
#' @name stream-operations
NULL

#' @rdname stream-operations
#' @export
methods::setGeneric("streamOpen", function(x, ...) {
  standardGeneric("streamOpen")
})

#' @rdname stream-operations
#' @export
methods::setGeneric("streamClose", function(x, ...) {
  standardGeneric("streamClose")
})

#' @rdname stream-operations
#' @export
methods::setGeneric("streamPull", function(x, ...) {
  standardGeneric("streamPull")
})

#' @rdname stream-operations
#' @export
methods::setGeneric("streamPush", function(x, ...) {
  standardGeneric("streamPush")
})

#' @rdname stream-operations
#' @export
methods::setGeneric("streamState", function(x) {
  standardGeneric("streamState")
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamState", "StreamEndpoint", function(x) x@state)

.unsupported_stream_operation <- function(x, operation) {
  .stream_abort(
    sprintf("%s is not implemented for endpoint class `%s`",
            operation, class(x)[[1L]]),
    "PhysioStream_unsupported_operation"
  )
}

#' @rdname stream-operations
methods::setMethod("streamOpen", "StreamEndpoint", function(x, ...) {
  .unsupported_stream_operation(x, "streamOpen")
})

#' @rdname stream-operations
methods::setMethod("streamClose", "StreamEndpoint", function(x, ...) {
  .unsupported_stream_operation(x, "streamClose")
})

#' @rdname stream-operations
methods::setMethod("streamPull", "StreamEndpoint", function(x, ...) {
  .unsupported_stream_operation(x, "streamPull")
})

#' @rdname stream-operations
methods::setMethod("streamPush", "StreamEndpoint", function(x, ...) {
  .unsupported_stream_operation(x, "streamPush")
})

.stream_transition <- function(x, to, operation) {
  from <- streamState(x)
  allowed <- switch(
    operation,
    open = identical(from, "created"),
    close = from %in% c("created", "open", "closed"),
    FALSE
  )
  if (!allowed) {
    .stream_abort(
      sprintf("invalid %s transition for `%s`: %s -> %s",
              operation, class(x)[[1L]], from, to),
      "PhysioStream_state_error"
    )
  }
  if (!identical(from, to)) {
    x@state <- to
    x@audit[[length(x@audit) + 1L]] <- list(
      operation = operation,
      from = from,
      to = to
    )
  }
  methods::validObject(x)
  x
}
