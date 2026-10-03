.lsl_runtime_schema <- "1.0.0"
.lsl_processing <- c("none", "clocksync")

.lsl_runtime_token_info <- streamInfo(
  name = "lsl-runtime-token",
  type = "internal",
  channel_names = "token",
  nominal_srate = 1,
  dtype = "float64",
  source_id = "",
  clock_domain = "local"
)

.lsl_new_runtime <- function(backend) {
  runtime <- new.env(parent = emptyenv())
  runtime$schema <- .lsl_runtime_schema
  runtime$backend <- .lsl_backend_arg(backend)
  runtime$adapter <- NULL
  runtime$handle <- NULL
  runtime$state <- "created"
  runtime$total_received <- 0
  runtime$total_pushed <- 0
  runtime$last_timestamp <- NULL
  runtime$token <- ringBuffer(.lsl_runtime_token_info, 1L)
  runtime
}

.lsl_runtime_assert <- function(runtime) {
  if (!is.environment(runtime) ||
      !identical(runtime$schema, .lsl_runtime_schema)) {
    .stream_abort(
      "invalid LSL endpoint runtime",
      "PhysioStream_lsl_lifetime_error"
    )
  }
  tryCatch(
    ringStats(runtime$token),
    error = function(e) {
      .stream_abort(
        "serialized or finalized LSL endpoint cannot be used",
        "PhysioStream_lsl_lifetime_error"
      )
    }
  )
  invisible(TRUE)
}

.lsl_runtime_adapter <- function(runtime) {
  .lsl_runtime_assert(runtime)
  if (is.null(runtime$adapter)) {
    runtime$adapter <- .lsl_import_adapter(runtime$backend)
  }
  runtime$adapter
}

.lsl_endpoint_validity <- function(object) {
  if (!is.environment(object@runtime)) {
    return("`runtime` must be an environment")
  }
  if (!is.integer(object@max_chunk) || length(object@max_chunk) != 1L ||
      is.na(object@max_chunk) || object@max_chunk < 1L) {
    return("`max_chunk` must be one positive integer")
  }
  if (!is.logical(object@recover) || length(object@recover) != 1L ||
      is.na(object@recover)) {
    return("`recover` must be one non-missing logical")
  }
  if (!is.character(object@processing) ||
      length(object@processing) != 1L ||
      is.na(object@processing) ||
      !(object@processing %in% .lsl_processing)) {
    return("`processing` must be exactly 'none' or 'clocksync'")
  }
  if (!is.integer(object@marker_capacity) ||
      length(object@marker_capacity) != 1L ||
      is.na(object@marker_capacity) ||
      object@marker_capacity < 1L) {
    return("`marker_capacity` must be one positive integer")
  }
  if (identical(object@info@dtype, "string")) {
    if (!is.environment(object@buffer)) {
      return("marker inlets require an environment queue")
    }
  } else if (!methods::is(object@buffer, "RingBuffer")) {
    return("numeric inlets require a RingBuffer")
  }
  TRUE
}

#' Lab Streaming Layer inlet
#'
#' Live backend and buffer state are intentionally reference-like and do not
#' survive serialization.
#'
#' @slot runtime Private live backend state.
#' @slot buffer Numeric ring buffer or bounded marker queue.
#' @slot max_chunk Maximum rows requested per pull.
#' @slot recover Whether liblsl source recovery is enabled.
#' @slot processing Exact timestamp processing mode.
#' @slot marker_capacity Marker queue capacity.
#' @examples
#' # Concrete inlet objects come from lslInlet(); see ?lslInlet.
#' isVirtualClass("LSLInlet")
#' @exportClass LSLInlet
methods::setClass(
  "LSLInlet",
  contains = "StreamSource",
  slots = c(
    runtime = "environment",
    buffer = "ANY",
    max_chunk = "integer",
    recover = "logical",
    processing = "character",
    marker_capacity = "integer"
  ),
  validity = .lsl_endpoint_validity
)

.lsl_outlet_validity <- function(object) {
  if (!is.environment(object@runtime)) {
    return("`runtime` must be an environment")
  }
  if (!is.integer(object@chunk_size) || length(object@chunk_size) != 1L ||
      is.na(object@chunk_size) || object@chunk_size < 0L) {
    return("`chunk_size` must be one non-negative integer")
  }
  if (!is.integer(object@max_buffered) ||
      length(object@max_buffered) != 1L ||
      is.na(object@max_buffered) ||
      object@max_buffered < 1L) {
    return("`max_buffered` must be one positive integer")
  }
  TRUE
}

#' Lab Streaming Layer outlet
#'
#' @slot runtime Private live backend state.
#' @slot chunk_size Preferred LSL chunk size.
#' @slot max_buffered LSL sender buffer bound.
#' @examples
#' # Concrete outlet objects come from lslOutlet(); see ?lslOutlet.
#' isVirtualClass("LSLOutlet")
#' @exportClass LSLOutlet
methods::setClass(
  "LSLOutlet",
  contains = "StreamSink",
  slots = c(
    runtime = "environment",
    chunk_size = "integer",
    max_buffered = "integer"
  ),
  validity = .lsl_outlet_validity
)

.lsl_info_identity <- function(info) {
  if (!methods::is(info, "StreamInfo")) {
    return(NULL)
  }
  lsl <- info@metadata$lsl
  if (!is.list(lsl) || !identical(lsl$mapping_schema, .lsl_mapping_schema) ||
      !is.character(lsl$descriptor_sha256) ||
      length(lsl$descriptor_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", lsl$descriptor_sha256)) {
    return(NULL)
  }
  lsl$descriptor_sha256
}

#' Construct an LSL inlet
#'
#' Construction validates R state only. Assign the result of `streamOpen()` to
#' open the already configured backend.
#'
#' @param info A resolved `StreamInfo`.
#' @param capacity Numeric ring capacity.
#' @param max_chunk Maximum rows per backend pull.
#' @param recover Whether liblsl may recover by non-empty source id.
#' @param processing Exact timestamp processing mode.
#' @param marker_capacity String marker queue capacity.
#' @inheritParams lslAvailable
#' @return An `LSLInlet` in state `"created"`.
#' @examples
#' \donttest{
#' # Requires a running LSL stream on the local network.
#' if (lslAvailable()) {
#'   found <- lslResolveStreams(timeout = 0.2)
#'   if (length(found)) {
#'     inlet <- streamOpen(lslInlet(found[[1]]))
#'     streamClose(inlet)
#'   }
#' }
#' }
#' @export
lslInlet <- function(info, capacity = 4096L, max_chunk = 1024L,
                     recover = TRUE,
                     processing = c("none", "clocksync"),
                     marker_capacity = 4096L,
                     backend = getOption(
                       "PhysioStream.lsl_backend", "auto"
                     )) {
  if (!methods::is(info, "StreamInfo")) {
    .stream_abort("`info` must be a StreamInfo",
                  "PhysioStream_validation_error")
  }
  methods::validObject(info)
  if (is.null(.lsl_info_identity(info))) {
    .stream_abort(
      "`info` must retain governed metadata from lslResolveStreams()",
      "PhysioStream_validation_error"
    )
  }
  backend <- .lsl_backend_arg(backend)
  capacity <- .ring_exact_integer(capacity, "capacity", 1)
  max_chunk <- .ring_exact_integer(max_chunk, "max_chunk", 1)
  marker_capacity <- .ring_exact_integer(
    marker_capacity, "marker_capacity", 1
  )
  recover <- .lsl_scalar_logical(recover, "recover")
  if (recover && !nzchar(info@source_id)) {
    .stream_abort(
      "`recover = TRUE` requires a non-empty source_id",
      "PhysioStream_validation_error"
    )
  }
  if (missing(processing)) {
    processing <- "none"
  }
  if (!is.character(processing) || length(processing) != 1L ||
      is.na(processing) || !(processing %in% .lsl_processing)) {
    .stream_abort(
      "`processing` must be exactly 'none' or 'clocksync'",
      "PhysioStream_validation_error"
    )
  }
  limit <- if (identical(info@dtype, "string")) {
    marker_capacity
  } else {
    capacity
  }
  if (max_chunk > limit) {
    .stream_abort(
      "`max_chunk` must not exceed the active inlet capacity",
      "PhysioStream_validation_error"
    )
  }
  buffer <- if (identical(info@dtype, "string")) {
    .lsl_marker_queue(marker_capacity, info@n_channels)
  } else {
    ringBuffer(info, capacity)
  }
  methods::new(
    "LSLInlet",
    info = info,
    state = "created",
    audit = list(),
    runtime = .lsl_new_runtime(backend),
    buffer = buffer,
    max_chunk = max_chunk,
    recover = recover,
    processing = processing,
    marker_capacity = marker_capacity
  )
}

#' Construct an LSL outlet
#'
#' @param info A valid numeric regular-rate or string irregular-rate
#'   `StreamInfo`.
#' @param chunk_size Exact non-negative LSL chunk preference.
#' @param max_buffered Exact positive LSL sender buffer bound.
#' @inheritParams lslAvailable
#' @return An `LSLOutlet` in state `"created"`.
#' @examples
#' \donttest{
#' # Publishing requires the LSL runtime (pylsl/liblsl).
#' if (lslAvailable()) {
#'   info <- streamInfo("demo", type = "EEG",
#'                      channel_names = c("C3", "C4"), nominal_srate = 100)
#'   outlet <- streamOpen(lslOutlet(info))
#'   lslPush(outlet, matrix(as.double(1:4), 2, 2))
#'   streamClose(outlet)
#' }
#' }
#' @export
lslOutlet <- function(info, chunk_size = 0L, max_buffered = 360L,
                      backend = getOption(
                        "PhysioStream.lsl_backend", "auto"
                      )) {
  if (!methods::is(info, "StreamInfo")) {
    .stream_abort("`info` must be a StreamInfo",
                  "PhysioStream_validation_error")
  }
  methods::validObject(info)
  if (identical(info@dtype, "string") && info@nominal_srate != 0) {
    .stream_abort(
      "string LSL outlets require nominal_srate = 0",
      "PhysioStream_validation_error"
    )
  }
  if (!identical(info@dtype, "string") && info@nominal_srate <= 0) {
    .stream_abort(
      "numeric LSL outlets require a positive nominal rate",
      "PhysioStream_validation_error"
    )
  }
  chunk_size <- .ring_exact_integer(chunk_size, "chunk_size", 0)
  max_buffered <- .ring_exact_integer(max_buffered, "max_buffered", 1)
  methods::new(
    "LSLOutlet",
    info = info,
    state = "created",
    audit = list(),
    runtime = .lsl_new_runtime(backend),
    chunk_size = chunk_size,
    max_buffered = max_buffered
  )
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamState", "LSLInlet", function(x) {
  .lsl_runtime_assert(x@runtime)
  x@runtime$state
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamState", "LSLOutlet", function(x) {
  .lsl_runtime_assert(x@runtime)
  x@runtime$state
})

.lsl_resolve_inlet_info <- function(x, adapter, timeout) {
  property <- if (nzchar(x@info@source_id)) "source_id" else "uid"
  value <- if (nzchar(x@info@source_id)) {
    x@info@source_id
  } else {
    x@info@metadata$lsl$uid
  }
  found <- adapter$resolve_byprop(property, value, 1L, timeout)
  if (!length(found)) {
    .stream_abort(
      sprintf("LSL inlet source `%s` was not resolved", value),
      "PhysioStream_lsl_resolution_error"
    )
  }
  wanted <- .lsl_info_identity(x@info)
  full <- lapply(found, adapter$full_info, timeout = timeout)
  hashes <- vapply(full, function(item) {
    .lsl_info_identity(.lsl_info_to_stream_info(item, adapter))
  }, character(1))
  match <- which(hashes == wanted)
  if (length(match) != 1L) {
    .stream_abort(
      "LSL inlet descriptor identity is absent or ambiguous",
      "PhysioStream_lsl_resolution_error"
    )
  }
  found[[match]]
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamOpen", "LSLInlet", function(x, timeout = 2, ...) {
  .lsl_runtime_assert(x@runtime)
  if (!identical(streamState(x), "created")) {
    .stream_abort(
      sprintf("streamOpen requires a created LSLInlet (state is `%s`)",
              streamState(x)),
      "PhysioStream_state_error"
    )
  }
  timeout <- .lsl_scalar_timeout(timeout)
  adapter <- .lsl_runtime_adapter(x@runtime)
  handle <- NULL
  tryCatch({
    resolved <- .lsl_resolve_inlet_info(x, adapter, timeout)
    max_buflen <- if (identical(x@info@dtype, "string")) {
      max(1, ceiling(x@marker_capacity / 100))
    } else {
      max(1, ceiling(ringCapacity(x@buffer) / x@info@nominal_srate))
    }
    handle <- adapter$make_inlet(
      resolved,
      max_buflen = max_buflen,
      max_chunklen = x@max_chunk,
      recover = x@recover,
      processing_flags = adapter$processing_flag(x@processing)
    )
    adapter$open_inlet(handle, timeout)
    verified <- .lsl_info_to_stream_info(
      adapter$inlet_info(handle, timeout), adapter
    )
    if (!.lsl_info_public_equal(verified, x@info) ||
        !identical(.lsl_info_identity(verified), .lsl_info_identity(x@info))) {
      .stream_abort(
        "opened LSL inlet metadata differs from the resolved descriptor",
        "PhysioStream_lsl_metadata_error"
      )
    }
    x@runtime$handle <- handle
    x@runtime$state <- "open"
  }, error = function(e) {
    if (!is.null(handle)) {
      try(adapter$close_inlet(handle), silent = TRUE)
    }
    x@runtime$handle <- NULL
    x@runtime$state <- "created"
    stop(e)
  })
  x@state <- "open"
  x@audit[[length(x@audit) + 1L]] <- list(
    operation = "open", from = "created", to = "open"
  )
  methods::validObject(x)
  invisible(x)
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamOpen", "LSLOutlet", function(x, ...) {
  .lsl_runtime_assert(x@runtime)
  if (!identical(streamState(x), "created")) {
    .stream_abort(
      sprintf("streamOpen requires a created LSLOutlet (state is `%s`)",
              streamState(x)),
      "PhysioStream_state_error"
    )
  }
  adapter <- .lsl_runtime_adapter(x@runtime)
  handle <- NULL
  tryCatch({
    py_info <- .stream_info_to_lsl_info(x@info, adapter)
    observed <- .lsl_info_to_stream_info(py_info, adapter)
    if (!.lsl_info_public_equal(observed, x@info)) {
      .stream_abort(
        "constructed LSL outlet metadata failed semantic round-trip",
        "PhysioStream_lsl_metadata_error"
      )
    }
    handle <- adapter$make_outlet(
      py_info, x@chunk_size, x@max_buffered
    )
    x@runtime$handle <- handle
    x@runtime$state <- "open"
  }, error = function(e) {
    if (!is.null(handle)) {
      try(adapter$close_outlet(handle), silent = TRUE)
    }
    x@runtime$handle <- NULL
    x@runtime$state <- "created"
    stop(e)
  })
  x@state <- "open"
  x@audit[[length(x@audit) + 1L]] <- list(
    operation = "open", from = "created", to = "open"
  )
  methods::validObject(x)
  invisible(x)
})

.lsl_close_endpoint <- function(x, inlet) {
  .lsl_runtime_assert(x@runtime)
  state <- streamState(x)
  if (!(state %in% c("created", "open", "closed", "error"))) {
    .stream_abort("invalid LSL endpoint state", "PhysioStream_state_error")
  }
  if (!is.null(x@runtime$handle)) {
    adapter <- .lsl_runtime_adapter(x@runtime)
    closer <- if (inlet) adapter$close_inlet else adapter$close_outlet
    try(closer(x@runtime$handle), silent = TRUE)
    x@runtime$handle <- NULL
  }
  x@runtime$state <- "closed"
  if (!identical(state, "closed")) {
    x@audit[[length(x@audit) + 1L]] <- list(
      operation = "close", from = state, to = "closed"
    )
  }
  x@state <- "closed"
  methods::validObject(x)
  invisible(x)
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamClose", "LSLInlet", function(x, ...) {
  .lsl_close_endpoint(x, inlet = TRUE)
})

#' @rdname stream-operations
#' @export
methods::setMethod("streamClose", "LSLOutlet", function(x, ...) {
  .lsl_close_endpoint(x, inlet = FALSE)
})

.lsl_require_open <- function(x, operation) {
  .lsl_runtime_assert(x@runtime)
  if (!identical(streamState(x), "open") || is.null(x@runtime$handle)) {
    .stream_abort(
      sprintf("%s requires an open %s (state is `%s`)",
              operation, class(x)[[1L]], streamState(x)),
      "PhysioStream_state_error"
    )
  }
  invisible(TRUE)
}

.lsl_chunk_matrix <- function(samples, timestamps, n_channels, string) {
  timestamps <- unlist(timestamps, recursive = TRUE, use.names = FALSE)
  if (!length(timestamps)) {
    return(matrix(
      if (string) character() else numeric(),
      nrow = 0L, ncol = n_channels
    ))
  }
  rows <- if (is.matrix(samples)) {
    lapply(seq_len(nrow(samples)), function(i) samples[i, , drop = TRUE])
  } else if (is.list(samples)) {
    samples
  } else {
    list(samples)
  }
  if (length(rows) != length(timestamps) ||
      any(vapply(rows, length, integer(1)) != n_channels)) {
    .stream_abort(
      "LSL backend returned a ragged or wrong-channel chunk",
      "PhysioStream_lsl_payload_error"
    )
  }
  values <- unlist(rows, recursive = TRUE, use.names = FALSE)
  if (string) {
    if (!is.character(values) || anyNA(values)) {
      .stream_abort(
        "LSL marker chunk must contain non-missing strings",
        "PhysioStream_lsl_payload_error"
      )
    }
    matrix(values, nrow = length(rows), ncol = n_channels, byrow = TRUE)
  } else {
    if (!is.numeric(values) || any(!is.finite(values))) {
      .stream_abort(
        "LSL numeric chunk must contain finite numeric values",
        "PhysioStream_lsl_payload_error"
      )
    }
    matrix(as.numeric(values), nrow = length(rows), ncol = n_channels,
           byrow = TRUE)
  }
}

#' Pull one LSL chunk into an inlet buffer
#'
#' @param x An open `LSLInlet`.
#' @param max_samples Exact number of samples requested, at most the inlet
#'   chunk bound.
#' @param timeout Finite non-negative backend timeout.
#' @return A serializable pull summary. Payload remains in the bounded inlet
#'   buffer or marker queue.
#' @examples
#' \donttest{
#' # Requires an open inlet bound to a live LSL stream.
#' if (lslAvailable()) {
#'   found <- lslResolveStreams(timeout = 0.2)
#'   if (length(found)) {
#'     inlet <- streamOpen(lslInlet(found[[1]]))
#'     summary <- lslPull(inlet, max_samples = 32)
#'     streamClose(inlet)
#'   }
#' }
#' }
#' @export
lslPull <- function(x, max_samples = x@max_chunk, timeout = 0) {
  if (!methods::is(x, "LSLInlet")) {
    .stream_abort("`x` must be an LSLInlet",
                  "PhysioStream_validation_error")
  }
  .lsl_require_open(x, "lslPull")
  max_samples <- .ring_exact_integer(max_samples, "max_samples", 0)
  if (max_samples > x@max_chunk) {
    .stream_abort(
      "`max_samples` must not exceed the inlet max_chunk",
      "PhysioStream_validation_error"
    )
  }
  timeout <- .lsl_scalar_timeout(timeout)
  before <- if (identical(x@info@dtype, "string")) {
    .lsl_marker_stats(x@buffer)
  } else {
    ringStats(x@buffer)
  }
  if (max_samples == 0L) {
    return(list(
      received = 0L,
      buffered = as.integer(before$fill),
      dropped_increment = 0,
      total_received = x@runtime$total_received,
      first_timestamp = NULL,
      last_timestamp = NULL,
      timeout = timeout,
      processing = x@processing,
      backend = x@runtime$backend
    ))
  }
  adapter <- .lsl_runtime_adapter(x@runtime)
  raw <- adapter$pull_chunk(
    x@runtime$handle, timeout, max_samples, x@info@dtype
  )
  if (length(raw) != 2L) {
    .stream_abort(
      "LSL backend pull must return samples and timestamps",
      "PhysioStream_lsl_payload_error"
    )
  }
  timestamps_raw <- unlist(
    raw[[2L]], recursive = TRUE, use.names = FALSE
  )
  if (!is.numeric(timestamps_raw)) {
    .stream_abort(
      "LSL backend timestamps must be numeric",
      "PhysioStream_lsl_payload_error"
    )
  }
  timestamps <- as.numeric(timestamps_raw)
  samples <- .lsl_chunk_matrix(
    raw[[1L]], timestamps, x@info@n_channels,
    string = identical(x@info@dtype, "string")
  )
  if (nrow(samples) > max_samples) {
    .stream_abort(
      "LSL backend returned more samples than requested",
      "PhysioStream_lsl_payload_error"
    )
  }
  if (nrow(samples) != length(timestamps) ||
      any(!is.finite(timestamps)) ||
      (length(timestamps) > 1L && any(diff(timestamps) <= 0))) {
    .stream_abort(
      "LSL backend timestamps must be finite and strictly increasing",
      "PhysioStream_lsl_payload_error"
    )
  }
  if (!length(timestamps)) {
    return(list(
      received = 0L,
      buffered = as.integer(before$fill),
      dropped_increment = 0,
      total_received = x@runtime$total_received,
      first_timestamp = NULL,
      last_timestamp = NULL,
      timeout = timeout,
      processing = x@processing,
      backend = x@runtime$backend
    ))
  }
  if (identical(x@info@dtype, "string")) {
    .lsl_marker_push(x@buffer, samples, timestamps)
    after <- .lsl_marker_stats(x@buffer)
  } else {
    ringPush(x@buffer, samples, timestamps)
    after <- ringStats(x@buffer)
  }
  x@runtime$total_received <- x@runtime$total_received + length(timestamps)
  list(
    received = as.integer(length(timestamps)),
    buffered = as.integer(after$fill),
    dropped_increment = after$total_dropped - before$total_dropped,
    total_received = x@runtime$total_received,
    first_timestamp = timestamps[[1L]],
    last_timestamp = timestamps[[length(timestamps)]],
    timeout = timeout,
    processing = x@processing,
    backend = x@runtime$backend
  )
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamPull", "LSLInlet", function(x, ...) {
  lslPull(x, ...)
})

.lsl_float32 <- function(x) {
  values <- readBin(
    writeBin(as.numeric(x), raw(), size = 4L, endian = .Platform$endian),
    what = "numeric", n = length(x), size = 4L,
    endian = .Platform$endian
  )
  if (any(!is.finite(values))) {
    .stream_abort(
      "sample overflows finite float32 transport",
      "PhysioStream_validation_error"
    )
  }
  values
}

.lsl_validate_numeric_payload <- function(info, samples) {
  if (!is.matrix(samples) || typeof(samples) != "double" ||
      nrow(samples) < 1L || ncol(samples) != info@n_channels ||
      any(!is.finite(samples))) {
    .stream_abort(
      "`samples` must be a finite real sample-by-channel matrix",
      "PhysioStream_validation_error"
    )
  }
  values <- samples
  if (identical(info@dtype, "float32")) {
    values[] <- .lsl_float32(values)
  } else if (startsWith(info@dtype, "int")) {
    bounds <- switch(
      info@dtype,
      int32 = c(-2147483648, 2147483647),
      int16 = c(-32768, 32767),
      int8 = c(-128, 127)
    )
    if (any(values != trunc(values)) ||
        any(values < bounds[[1L]] | values > bounds[[2L]])) {
      .stream_abort(
        "samples are not exactly representable by the declared integer dtype",
        "PhysioStream_validation_error"
      )
    }
  }
  values
}

.lsl_rows <- function(samples) {
  lapply(seq_len(nrow(samples)), function(i) {
    unname(as.list(samples[i, , drop = TRUE]))
  })
}

#' Push samples or markers through an LSL outlet
#'
#' @param x An open `LSLOutlet`.
#' @param samples A strict sample-by-channel numeric or character matrix.
#' @param timestamps Optional explicit timestamp per row.
#' @param pushthrough Whether the final backend operation flushes the chunk.
#' @return A serializable push summary.
#' @examples
#' \donttest{
#' # Requires the LSL runtime (pylsl/liblsl).
#' if (lslAvailable()) {
#'   info <- streamInfo("demo", type = "EEG",
#'                      channel_names = c("C3", "C4"), nominal_srate = 100)
#'   outlet <- streamOpen(lslOutlet(info))
#'   lslPush(outlet, matrix(as.double(1:4), 2, 2))
#'   streamClose(outlet)
#' }
#' }
#' @export
lslPush <- function(x, samples, timestamps = NULL, pushthrough = TRUE) {
  if (!methods::is(x, "LSLOutlet")) {
    .stream_abort("`x` must be an LSLOutlet",
                  "PhysioStream_validation_error")
  }
  .lsl_require_open(x, "lslPush")
  pushthrough <- .lsl_scalar_logical(pushthrough, "pushthrough")
  marker <- identical(x@info@dtype, "string")
  if (marker) {
    if (!is.matrix(samples) || !is.character(samples) ||
        nrow(samples) < 1L || ncol(samples) != x@info@n_channels ||
        anyNA(samples)) {
      .stream_abort(
        "marker `samples` must be a non-missing character sample-by-channel matrix",
        "PhysioStream_validation_error"
      )
    }
    payload <- samples
  } else {
    payload <- .lsl_validate_numeric_payload(x@info, samples)
  }
  if (is.null(timestamps)) {
    if (marker && nrow(payload) > 1L) {
      .stream_abort(
        "multi-row marker pushes require explicit timestamps",
        "PhysioStream_validation_error"
      )
    }
  } else {
    if (typeof(timestamps) != "double" || !is.vector(timestamps) ||
        length(timestamps) != nrow(payload) ||
        any(!is.finite(timestamps)) ||
        (length(timestamps) > 1L && any(diff(timestamps) <= 0))) {
      .stream_abort(
        "`timestamps` must be a finite strictly increasing real vector matching rows",
        "PhysioStream_validation_error"
      )
    }
    if (!is.null(x@runtime$last_timestamp) &&
        timestamps[[1L]] <= x@runtime$last_timestamp) {
      .stream_abort(
        "explicit timestamps must increase across outlet pushes",
        "PhysioStream_validation_error"
      )
    }
  }
  adapter <- .lsl_runtime_adapter(x@runtime)
  rows <- if (is.function(adapter$prepare_rows)) {
    adapter$prepare_rows(payload, x@info@dtype)
  } else {
    .lsl_rows(payload)
  }
  confirmed <- 0L
  tryCatch({
    if (is.null(timestamps) && !marker) {
      adapter$push_chunk(x@runtime$handle, rows, pushthrough)
      confirmed <- nrow(payload)
    } else {
      for (i in seq_len(nrow(payload))) {
        timestamp <- if (is.null(timestamps)) 0 else timestamps[[i]]
        adapter$push_sample(
          x@runtime$handle, rows[[i]], timestamp,
          pushthrough && i == nrow(payload)
        )
        confirmed <- i
      }
    }
  }, error = function(e) {
    x@runtime$total_pushed <- x@runtime$total_pushed + confirmed
    if (!is.null(timestamps) && confirmed > 0L) {
      x@runtime$last_timestamp <- timestamps[[confirmed]]
    }
    x@runtime$state <- "error"
    .stream_abort(
      sprintf(
        "LSL push failed after %d of %d confirmed sample(s): %s",
        confirmed, nrow(payload), conditionMessage(e)
      ),
      "PhysioStream_lsl_transport_error"
    )
  })
  x@runtime$total_pushed <- x@runtime$total_pushed + confirmed
  if (!is.null(timestamps)) {
    x@runtime$last_timestamp <- timestamps[[length(timestamps)]]
  }
  list(
    pushed = as.integer(confirmed),
    first_timestamp = if (is.null(timestamps)) NULL else timestamps[[1L]],
    last_timestamp = if (is.null(timestamps)) NULL else
      timestamps[[length(timestamps)]],
    total_pushed = x@runtime$total_pushed,
    backend = x@runtime$backend
  )
}

#' @rdname stream-operations
#' @export
methods::setMethod("streamPush", "LSLOutlet", function(x, ...) {
  lslPush(x, ...)
})

.lsl_snapshot_metadata <- function(x) {
  list(
    backend = x@runtime$backend,
    processing = x@processing,
    descriptor_sha256 = .lsl_info_identity(x@info),
    total_received = x@runtime$total_received
  )
}
