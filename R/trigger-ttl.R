.ttl_make_serial_module <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .trigger_abort(
      "serial TTL requires the suggested package `reticulate`",
      "PhysioStream_trigger_unavailable"
    )
  }
  module <- tryCatch(
    reticulate::import("serial", convert = TRUE),
    error = function(e) {
      .trigger_abort(
        "configured Python cannot import pinned pySerial",
        "PhysioStream_trigger_unavailable"
      )
    }
  )
  reticulate::py_run_string(
    paste(
      "def _physiostream_serial_open_v1(serial_module, port, baud, timeout_s):",
      "    return serial_module.Serial(port=str(port), baudrate=int(baud),",
      "                                timeout=0, write_timeout=float(timeout_s))",
      "",
      "def _physiostream_serial_write_v1(handle, payload):",
      "    raw = bytes([int(value) for value in payload])",
      "    count = int(handle.write(raw))",
      "    handle.flush()",
      "    if count != len(raw):",
      "        raise IOError('partial governed serial write')",
      "    return count",
      "",
      "def _physiostream_serial_close_v1(handle):",
      "    handle.close()",
      "    return None",
      sep = "\n"
    )
  )
  module
}

.ttl_open_serial_adapter <- function(config) {
  module <- .ttl_make_serial_module()
  handle <- tryCatch(
    reticulate::py[["_physiostream_serial_open_v1"]](
      module, config$port, as.integer(config$baud),
      config$write_timeout_ms / 1000
    ),
    error = function(e) {
      .trigger_abort(
        "serial TTL transport failed to open",
        "PhysioStream_trigger_transport_error"
      )
    }
  )
  write_frame <- function(frame) {
    bytes <- as.integer(charToRaw(enc2utf8(frame)))
    count <- tryCatch(
      reticulate::py[["_physiostream_serial_write_v1"]](handle, bytes),
      error = function(e) {
        .trigger_abort(
          "serial TTL write acknowledgement is unknown",
          "PhysioStream_trigger_transport_error"
        )
      }
    )
    list(
      ok = TRUE,
      ack_code = "serial_write_complete",
      bytes_written = as.numeric(count)
    )
  }
  adapter <- new.env(parent = emptyenv())
  adapter$send <- write_frame
  adapter$stop <- function() {
    write_frame(.trigger_stop_frame(config$line))
  }
  adapter$close <- function() {
    tryCatch(
      reticulate::py[["_physiostream_serial_close_v1"]](handle),
      error = function(e) {
        .trigger_abort(
          "serial TTL transport failed to close",
          "PhysioStream_trigger_transport_error"
        )
      }
    )
    invisible(TRUE)
  }
  stop_result <- tryCatch(adapter$stop(), error = function(e) e)
  if (inherits(stop_result, "condition") || !isTRUE(stop_result$ok)) {
    try(adapter$close(), silent = TRUE)
    .trigger_abort(
      "serial TTL could not establish fail-safe low on open",
      "PhysioStream_trigger_transport_error"
    )
  }
  adapter
}

#' Construct a governed TTL stimulation-command trigger
#'
#' TTL transport is an adapter boundary, not a vendor stimulator protocol.
#' Serial mode writes a versioned ASCII frame to a cooperating independently
#' fail-safe adapter. Callback mode requires one atomic timed pulse operation;
#' PhysioStream does not synthesize pulse timing with an R sleep or busy loop.
#' Loopback mode records the exact frame without hardware.
#'
#' Opening some serial ports can momentarily affect RTS/DTR control lines.
#' Hardware must remain fail-safe despite port open, process failure, malformed
#' frames, and communication loss.
#'
#' @inheritParams loopbackTrigger
#' @param transport Exact `"serial"`, `"callback"`, or `"loopback"`.
#' @param port Serial port path for serial mode.
#' @param baud Positive integer serial baud rate.
#' @param line Positive integer adapter line identifier.
#' @param pulse_width_ms Positive pulse width requested from the adapter.
#' @param write_timeout_ms Positive bounded serial write timeout.
#' @param writer Callback implementing atomic `"pulse"` and fail-safe `"stop"`
#'   actions in callback mode.
#' @return A closed, disarmed `TtlTrigger`.
#' @examples
#' \dontrun{
#' # Serial transport requires a device exposing a TTL line; see
#' # loopbackTrigger() for an offline equivalent with the same interlocks.
#' trigger <- ttlTrigger(transport = "serial", port = "/dev/ttyUSB0",
#'                       allowed_channels = "left", max_intensity = 20,
#'                       intensity_unit = "mA", max_duration_ms = 500,
#'                       refractory_ms = 0, deadman_ms = 1000)
#' }
#' @export
ttlTrigger <- function(
    transport = c("serial", "callback", "loopback"),
    allowed_channels,
    max_intensity,
    intensity_unit,
    max_duration_ms,
    refractory_ms,
    deadman_ms,
    port = NULL,
    baud = 115200L,
    line = 1L,
    pulse_width_ms = 5,
    write_timeout_ms = 100,
    writer = NULL,
    audit_capacity = 4096L,
    clock = NULL) {
  if (missing(transport)) {
    transport <- "serial"
  }
  transport <- .trigger_enum(
    transport, c("serial", "callback", "loopback"), "transport"
  )
  if (!is.null(clock) && !identical(transport, "loopback")) {
    .trigger_abort(
      "clock injection is available only for loopback triggers",
      "PhysioStream_trigger_validation_error"
    )
  }
  line <- .trigger_scalar(
    line, "line", lower = 1, upper = 65535, integer = TRUE
  )
  pulse_width_ns <- .trigger_ms_to_ns(
    pulse_width_ms, "pulse_width_ms"
  )
  pulse_width_ms <- pulse_width_ns / 1e6
  write_timeout_ms <- .trigger_scalar(
    write_timeout_ms, "write_timeout_ms", lower = 0,
    lower_open = TRUE, upper = 60000
  )
  baud <- .trigger_scalar(
    baud, "baud", lower = 1, upper = 4000000, integer = TRUE
  )
  allowed_channels <- .trigger_channels(allowed_channels)
  invisible(vapply(
    allowed_channels,
    .trigger_serial_token,
    character(1),
    name = "allowed_channels"
  ))
  intensity_unit <- .trigger_serial_token(
    intensity_unit, "intensity_unit"
  )

  if (identical(transport, "serial")) {
    port <- .trigger_string(port, "port", max_bytes = 4096L)
    if (!is.null(writer)) {
      .trigger_abort(
        "`writer` is valid only for callback transport",
        "PhysioStream_trigger_validation_error"
      )
    }
  } else if (!is.null(port)) {
    .trigger_abort(
      "`port` is valid only for serial transport",
      "PhysioStream_trigger_validation_error"
    )
  }
  if (identical(transport, "callback")) {
    if (!is.function(writer)) {
      .trigger_abort(
        "callback transport requires a writer function",
        "PhysioStream_trigger_validation_error"
      )
    }
  } else if (!is.null(writer)) {
    .trigger_abort(
      "`writer` is valid only for callback transport",
      "PhysioStream_trigger_validation_error"
    )
  }

  runtime_transport <- switch(
    transport,
    serial = "serial",
    callback = "callback",
    loopback = "ttl_loopback"
  )
  endpoint <- list(
    transport = transport,
    port = if (identical(transport, "serial")) port else NULL,
    baud = if (identical(transport, "serial")) baud else NULL,
    line = line,
    pulse_width_ms = pulse_width_ms,
    write_timeout_ms = if (identical(transport, "serial")) {
      write_timeout_ms
    } else {
      NULL
    },
    frame_schema = "physiostream.ttl-frame/1.0.0"
  )
  configuration <- .trigger_configuration(
    backend = "ttl",
    transport = runtime_transport,
    allowed_channels = allowed_channels,
    max_intensity = max_intensity,
    intensity_unit = intensity_unit,
    max_duration_ms = max_duration_ms,
    refractory_ms = refractory_ms,
    deadman_ms = deadman_ms,
    audit_capacity = audit_capacity,
    endpoint = endpoint
  )
  runtime_config <- list(
    port = port,
    baud = baud,
    line = line,
    pulse_width_ms = pulse_width_ms,
    write_timeout_ms = write_timeout_ms
  )
  .trigger_new(
    "TtlTrigger", configuration,
    runtime_config = runtime_config,
    writer = writer,
    clock = clock
  )
}
