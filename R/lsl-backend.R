.lsl_capability_schema <- "1.0.0"
.lsl_backends <- c("auto", "pylsl")

.lsl_backend_arg <- function(backend) {
  if (!is.character(backend) || length(backend) != 1L || is.na(backend) ||
      !(backend %in% .lsl_backends)) {
    .stream_abort(
      "`backend` must be exactly 'auto' or 'pylsl'",
      "PhysioStream_validation_error"
    )
  }
  if (identical(backend, "auto")) "pylsl" else backend
}

.lsl_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .stream_abort(
      sprintf("`%s` must be one non-missing logical", name),
      "PhysioStream_validation_error"
    )
  }
  x
}

# TRUE only when reticulate can use an already-configured Python interpreter
# that physically exists. This deliberately excludes reticulate's automatic
# ephemeral-environment provisioning: reticulate (>= 1.41) downloads uv, a
# CPython build and NumPy on first initialization when no interpreter is
# configured. A capability probe must never trigger that network work, which on
# an offline build machine (e.g. r-universe) fails or hangs instead of yielding
# a clean FALSE. py_discover_config() only reports what would be used and does
# not provision, so gating on a real on-disk interpreter keeps the probe fast,
# side-effect-free, and honest when no backend is available.
.python_interpreter_configured <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }
  if (isTRUE(tryCatch(reticulate::py_available(initialize = FALSE),
                      error = function(e) FALSE))) {
    return(TRUE)
  }
  config <- tryCatch(
    reticulate::py_discover_config(),
    error = function(e) NULL,
    interrupt = function(e) NULL
  )
  is.list(config) &&
    !is.null(config$python) &&
    length(config$python) == 1L &&
    nzchar(config$python) &&
    file.exists(config$python)
}

.lsl_scalar_timeout <- function(x, name = "timeout") {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    .stream_abort(
      sprintf("`%s` must be one finite non-negative number", name),
      "PhysioStream_validation_error"
    )
  }
  as.numeric(x)
}

.lsl_py_list <- function(x) {
  converted <- tryCatch(reticulate::py_to_r(x), error = function(e) x)
  if (is.null(converted)) {
    return(list())
  }
  if (is.list(converted)) {
    return(converted)
  }
  list(converted)
}

.lsl_make_pylsl_adapter <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .stream_abort(
      "LSL backend 'pylsl' requires the suggested package `reticulate`",
      "PhysioStream_lsl_unavailable"
    )
  }
  module <- tryCatch(
    reticulate::import("pylsl", convert = TRUE),
    error = function(e) {
      .stream_abort(
        paste0(
          "LSL backend 'pylsl' is unavailable: ",
          conditionMessage(e),
          ". Configure an existing Python environment containing pylsl/liblsl."
        ),
        "PhysioStream_lsl_unavailable"
      )
    }
  )
  reticulate::py_run_string(
    paste(
      "def _physiostream_pull_chunk_v1(inlet, timeout, max_samples, numeric):",
      "    samples, timestamps = inlet.pull_chunk(",
      "        timeout=timeout, max_samples=max_samples)",
      "    if numeric:",
      "        samples = [[float(value) for value in row] for row in samples]",
      "    return samples, [float(value) for value in timestamps]",
      sep = "\n"
    )
  )
  pull_chunk_helper <- reticulate::py[["_physiostream_pull_chunk_v1"]]

  adapter <- new.env(parent = emptyenv())
  adapter$backend <- "pylsl"
  adapter$module <- module
  adapter$backend_info <- function() {
    config <- reticulate::py_config()
    python_version <- strsplit(
      as.character(config$version_string), " ", fixed = TRUE
    )[[1L]][[1L]]
    list(
      backend = "pylsl",
      python = as.character(config$python),
      python_version = python_version,
      reticulate_version = as.character(utils::packageVersion("reticulate")),
      pylsl_version = as.character(module[["__version__"]]),
      liblsl_version = as.character(module$library_version()),
      liblsl_info = as.character(module$library_info()),
      capability_schema = .lsl_capability_schema
    )
  }
  adapter$resolve_all <- function(timeout) {
    .lsl_py_list(module$resolve_streams(wait_time = timeout))
  }
  adapter$resolve_byprop <- function(property, value, minimum, timeout) {
    .lsl_py_list(module$resolve_byprop(
      property, value, minimum = minimum, timeout = timeout
    ))
  }
  adapter$info_xml <- function(info) as.character(info$as_xml())
  adapter$make_info <- function(info) {
    format_name <- switch(
      info@dtype,
      float32 = "cf_float32",
      float64 = "cf_double64",
      int32 = "cf_int32",
      int16 = "cf_int16",
      int8 = "cf_int8",
      string = "cf_string",
      .stream_abort(
        sprintf("unsupported LSL dtype `%s`", info@dtype),
        "PhysioStream_lsl_metadata_error"
      )
    )
    module$StreamInfo(
      name = info@name,
      type = info@type,
      channel_count = as.integer(info@n_channels),
      nominal_srate = as.numeric(info@nominal_srate),
      channel_format = module[[format_name]],
      source_id = if (nzchar(info@source_id)) info@source_id else NULL
    )
  }
  adapter$append_child <- function(node, name) node$append_child(name)
  adapter$append_child_value <- function(node, name, value) {
    invisible(node$append_child_value(name, value))
  }
  adapter$make_inlet <- function(info, max_buflen, max_chunklen, recover,
                                processing_flags) {
    module$StreamInlet(
      info,
      max_buflen = as.integer(max_buflen),
      max_chunklen = as.integer(max_chunklen),
      recover = recover,
      processing_flags = as.integer(processing_flags)
    )
  }
  adapter$full_info <- function(info, timeout) {
    inlet <- module$StreamInlet(
      info,
      max_buflen = 1L,
      max_chunklen = 1L,
      recover = FALSE,
      processing_flags = as.integer(module$proc_none)
    )
    on.exit(try(inlet$close_stream(), silent = TRUE), add = TRUE)
    inlet$info(timeout = timeout)
  }
  adapter$open_inlet <- function(inlet, timeout) {
    invisible(inlet$open_stream(timeout = timeout))
  }
  adapter$close_inlet <- function(inlet) invisible(inlet$close_stream())
  adapter$inlet_info <- function(inlet, timeout) inlet$info(timeout = timeout)
  adapter$pull_chunk <- function(inlet, timeout, max_samples, dtype) {
    .lsl_py_list(pull_chunk_helper(
      inlet, timeout, as.integer(max_samples),
      !identical(dtype, "string")
    ))
  }
  adapter$make_outlet <- function(info, chunk_size, max_buffered) {
    module$StreamOutlet(
      info,
      chunk_size = as.integer(chunk_size),
      max_buffered = as.integer(max_buffered)
    )
  }
  adapter$close_outlet <- function(outlet) invisible(NULL)
  adapter$prepare_rows <- function(samples, dtype) {
    rows <- lapply(seq_len(nrow(samples)), function(i) {
      unname(as.list(samples[i, , drop = TRUE]))
    })
    if (!startsWith(dtype, "int")) {
      return(rows)
    }
    builtins <- reticulate::import_builtins(convert = FALSE)
    lapply(rows, function(row) {
      lapply(row, function(value) {
        builtins$int(format(value, scientific = FALSE, trim = TRUE))
      })
    })
  }
  adapter$push_chunk <- function(outlet, samples, pushthrough) {
    invisible(outlet$push_chunk(
      samples, timestamp = 0, pushthrough = pushthrough
    ))
  }
  adapter$push_sample <- function(outlet, sample, timestamp, pushthrough) {
    invisible(outlet$push_sample(
      sample, timestamp = timestamp, pushthrough = pushthrough
    ))
  }
  adapter$processing_flag <- function(processing) {
    if (identical(processing, "none")) {
      as.integer(module$proc_none)
    } else {
      as.integer(module$proc_clocksync)
    }
  }
  adapter
}

.lsl_import_adapter <- function(backend) {
  backend <- .lsl_backend_arg(backend)
  if (identical(backend, "pylsl")) {
    return(.lsl_make_pylsl_adapter())
  }
  .stream_abort(
    sprintf("unsupported LSL backend `%s`", backend),
    "PhysioStream_lsl_unavailable"
  )
}

#' Test Lab Streaming Layer backend availability
#'
#' The default probe is conservative and does not initialize Python. Explicit
#' transport operations initialize only the already configured reticulate
#' interpreter; no function installs or selects an environment.
#'
#' @param backend Exact backend, currently `"auto"` or `"pylsl"`.
#' @param initialize Whether the configured Python interpreter may be
#'   initialized to verify that pylsl and liblsl load.
#' @return One logical value.
#' @examples
#' # Reports FALSE unless pylsl and liblsl are installed and loadable.
#' lslAvailable()
#' @export
lslAvailable <- function(backend = c("auto", "pylsl"), initialize = FALSE) {
  if (missing(backend)) {
    backend <- "auto"
  }
  backend <- .lsl_backend_arg(backend)
  initialize <- .lsl_scalar_logical(initialize, "initialize")
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }
  if (!initialize) {
    if (!isTRUE(reticulate::py_available(initialize = FALSE))) {
      return(FALSE)
    }
    return(isTRUE(tryCatch(
      !is.null(
        reticulate::import("importlib.util", convert = TRUE)$find_spec("pylsl")
      ),
      error = function(e) FALSE
    )))
  }
  if (!.python_interpreter_configured()) {
    return(FALSE)
  }
  isTRUE(tryCatch({
    .lsl_import_adapter(backend)
    TRUE
  }, error = function(e) FALSE, interrupt = function(e) FALSE))
}

#' Report the configured LSL backend
#'
#' @inheritParams lslAvailable
#' @return A serializable named list of backend and runtime versions.
#' @examples
#' \donttest{
#' # Resolving the backend version requires pylsl/liblsl to be installed.
#' if (lslAvailable()) str(lslBackendInfo())
#' }
#' @export
lslBackendInfo <- function(backend = c("auto", "pylsl")) {
  if (missing(backend)) {
    backend <- "auto"
  }
  .lsl_import_adapter(backend)$backend_info()
}
