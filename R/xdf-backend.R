.xdf_capability_schema <- "1.0.0"
.xdf_backends <- c("auto", "pyxdf")

.xdf_backend_arg <- function(backend) {
  if (!is.character(backend) || length(backend) != 1L || is.na(backend) ||
      !(backend %in% .xdf_backends)) {
    .stream_abort(
      "`backend` must be exactly 'auto' or 'pyxdf'",
      "PhysioStream_validation_error"
    )
  }
  if (identical(backend, "auto")) "pyxdf" else backend
}

.xdf_make_pyxdf_adapter <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .stream_abort(
      "XDF backend 'pyxdf' requires the suggested package `reticulate`",
      "PhysioStream_xdf_unavailable"
    )
  }
  module <- tryCatch(
    reticulate::import("pyxdf", convert = FALSE),
    error = function(e) {
      .stream_abort(
        paste0(
          "XDF backend 'pyxdf' is unavailable: ",
          conditionMessage(e),
          ". Configure an existing Python environment containing pyxdf."
        ),
        "PhysioStream_xdf_unavailable"
      )
    }
  )
  numpy <- tryCatch(
    reticulate::import("numpy", convert = FALSE),
    error = function(e) {
      .stream_abort(
        paste0("XDF backend 'pyxdf' cannot import NumPy: ",
               conditionMessage(e)),
        "PhysioStream_xdf_unavailable"
      )
    }
  )

  reticulate::py_run_string(
    paste(
      "def _physiostream_load_xdf_v1(module, path, synchronize, dejitter):",
      "    import logging",
      "    previous_disable = logging.root.manager.disable",
      "    logging.disable(logging.CRITICAL)",
      "    try:",
      "        streams, _ = module.load_xdf(",
      "            path, select_streams=None,",
      "            on_chunk=None, synchronize_clocks=bool(synchronize),",
      "            handle_clock_resets=True,",
      "            dejitter_timestamps=bool(dejitter), verbose=False)",
      "    finally:",
      "        logging.disable(previous_disable)",
      "    out = []",
      "    for stream in streams:",
      "        info = stream['info']",
      "        def first(name, default=''):",
      "            value = info.get(name, [default])",
      "            if isinstance(value, (list, tuple)):",
      "                value = value[0] if value else default",
      "            return str(value)",
      "        fmt = first('channel_format')",
      "        values = stream['time_series']",
      "        values = values.tolist() if hasattr(values, 'tolist') else values",
      "        if len(values) == 0:",
      "            rows = []",
      "        elif fmt == 'string':",
      "            rows = [[str(value) for value in row]",
      "                    for row in values]",
      "        else:",
      "            rows = [[float(value) for value in row]",
      "                    for row in values]",
      "        segments = [[float(pair[0]), float(pair[1])]",
      "                    for pair in info.get('segments', [])]",
      "        effective = info.get('effective_srate', 0.0)",
      "        out.append({",
      "            'stream_id': str(info['stream_id']),",
      "            'name': first('name'),",
      "            'type': first('type'),",
      "            'source_id': first('source_id'),",
      "            'uid': first('uid'),",
      "            'channel_count': first('channel_count'),",
      "            'nominal_srate': first('nominal_srate'),",
      "            'channel_format': fmt,",
      "            'time_series': rows,",
      "            'time_stamps': [float(value)",
      "                            for value in stream['time_stamps']],",
      "            'clock_times': [float(value)",
      "                            for value in stream.get('clock_times', [])],",
      "            'clock_values': [float(value)",
      "                             for value in stream.get('clock_values', [])],",
      "            'segments': segments,",
      "            'effective_srate': float(effective)",
      "        })",
      "    return out",
      sep = "\n"
    )
  )
  load_helper <- reticulate::py[["_physiostream_load_xdf_v1"]]

  adapter <- new.env(parent = emptyenv())
  adapter$backend <- "pyxdf"
  adapter$backend_info <- function() {
    config <- reticulate::py_config()
    python_version <- strsplit(
      as.character(config$version_string), " ", fixed = TRUE
    )[[1L]][[1L]]
    list(
      backend = "pyxdf",
      python = as.character(config$python),
      python_version = python_version,
      reticulate_version = as.character(utils::packageVersion("reticulate")),
      pyxdf_version = as.character(module[["__version__"]]),
      numpy_version = as.character(numpy[["__version__"]]),
      capability_schema = .xdf_capability_schema
    )
  }
  adapter$load <- function(path, synchronize, dejitter) {
    tryCatch(
      reticulate::py_to_r(load_helper(
        module, path, synchronize, dejitter
      )),
      error = function(e) {
        .stream_abort(
          paste0("pyxdf failed to read `", path, "`: ", conditionMessage(e)),
          "PhysioStream_xdf_parse_error"
        )
      }
    )
  }
  adapter
}

.xdf_import_adapter <- function(backend) {
  backend <- .xdf_backend_arg(backend)
  if (identical(backend, "pyxdf")) {
    return(.xdf_make_pyxdf_adapter())
  }
  .stream_abort(
    sprintf("unsupported XDF backend `%s`", backend),
    "PhysioStream_xdf_unavailable"
  )
}

#' Test XDF backend availability
#'
#' The default probe is conservative and does not initialize Python. Explicit
#' read operations initialize only the already configured reticulate
#' interpreter. PhysioStream never installs or selects a Python environment.
#'
#' @param backend Exact backend, currently `"auto"` or `"pyxdf"`.
#' @param initialize Whether the configured Python interpreter may be
#'   initialized to verify pyxdf and NumPy.
#' @return One non-missing logical value.
#' @examples
#' # Reports FALSE unless the pyxdf backend is installed.
#' xdfAvailable()
#' @export
xdfAvailable <- function(backend = c("auto", "pyxdf"), initialize = FALSE) {
  if (missing(backend)) {
    backend <- "auto"
  }
  backend <- .xdf_backend_arg(backend)
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
        reticulate::import("importlib.util", convert = TRUE)$find_spec("pyxdf")
      ),
      error = function(e) FALSE
    )))
  }
  if (!.python_interpreter_configured()) {
    return(FALSE)
  }
  isTRUE(tryCatch({
    .xdf_import_adapter(backend)
    TRUE
  }, error = function(e) FALSE, interrupt = function(e) FALSE))
}

#' Report the configured XDF backend
#'
#' @inheritParams xdfAvailable
#' @return A serializable named list of backend and runtime versions.
#' @examples
#' \donttest{
#' # Resolving the backend version requires the pyxdf backend.
#' if (xdfAvailable()) str(xdfBackendInfo())
#' }
#' @export
xdfBackendInfo <- function(backend = c("auto", "pyxdf")) {
  if (missing(backend)) {
    backend <- "auto"
  }
  .xdf_import_adapter(backend)$backend_info()
}
