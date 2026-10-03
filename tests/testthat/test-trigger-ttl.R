test_that("callback TTL establishes low, executes atomic pulse, and stops", {
  callback <- test_callback_writer()
  trigger <- ttlTrigger(
    "callback", c("left", "right"), 10, "mA", 100, 100, 1000,
    line = 2L, pulse_width_ms = 5, writer = callback$writer
  )
  triggerOpen(trigger)
  expect_identical(callback$log$requests[[1L]]$action, "stop")
  armTrigger(trigger, "session", 0)
  receipt <- sendStim(trigger, 2, "left", 10, "command-1", 0)
  request <- callback$log$requests[[2L]]
  expect_identical(request$action, "pulse")
  expect_identical(request$line, 2L)
  expect_identical(request$pulse_width_ms, 5)
  expect_identical(rawToChar(request$payload_raw),
                   receipt$transport_details$frame)
  expect_identical(receipt$status, "acknowledged")
  expect_identical(receipt$acknowledged_at_ns, 5e6)
  expect_identical(
    receipt$transport_details$deassert_time_ns -
      receipt$transport_details$assert_time_ns,
    5e6
  )
  triggerClose(trigger)
  expect_identical(tail(callback$log$requests, 1L)[[1L]]$action, "stop")
})

test_that("TTL loopback returns independently verifiable frame", {
  trigger <- ttlTrigger(
    "loopback", "left", 10, "mA", 100, 0, 1000,
    line = 3L, pulse_width_ms = 4
  )
  open_and_arm(trigger)
  receipt <- sendStim(trigger, 2, "left", 10, "command-1", 0)
  frame <- receipt$transport_details$frame
  parts <- strsplit(sub("\n$", "", frame), ",", fixed = TRUE)[[1L]]
  body <- paste(head(parts, -1L), collapse = ",")
  expect_identical(
    tail(parts, 1L),
    digest::digest(charToRaw(body), algo = "crc32", serialize = FALSE)
  )
  expect_identical(parts[[5L]], "3")
})

test_that("callback errors become conservative unknown attempts", {
  calls <- 0L
  writer <- function(request) {
    calls <<- calls + 1L
    if (request$action == "stop") {
      return(list(ok = TRUE, ack_code = "stop"))
    }
    stop("transport failure")
  }
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  open_and_arm(trigger)
  receipt <- sendStim(trigger, 1, "left", 10, "command-1", 0)
  expect_identical(receipt$status, "unknown")
  expect_identical(
    receipt$error_class,
    "PhysioStream_trigger_transport_error"
  )
  expect_identical(receipt$error_code, "transport_error")
  expect_equal(triggerState(trigger)$last_attempt_by_channel$left, 0)
  expect_equal(calls, 2L)
})

test_that("caller interrupts finalize conservative unknown state", {
  interrupt <- structure(
    list(message = "test transport interrupt", call = NULL),
    class = c("interrupt", "condition")
  )
  writer <- function(request) {
    if (identical(request$action, "stop")) {
      return(list(ok = TRUE, ack_code = "stop"))
    }
    signalCondition(interrupt)
  }
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  open_and_arm(trigger)
  observed <- tryCatch(
    sendStim(trigger, 1, "left", 10, "interrupted", 0),
    interrupt = function(e) e
  )
  expect_s3_class(observed, "interrupt")
  state <- triggerState(trigger)
  expect_false(trigger$busy)
  expect_identical(state$counters$attempted, 1)
  expect_identical(state$counters$unknown, 1)
  expect_identical(state$recent_command_ids, "interrupted")
  expect_identical(state$audit[[1L]]$status, "unknown")
  expect_identical(
    state$audit[[1L]]$error_class,
    "PhysioStream_trigger_transport_error"
  )
  expect_identical(state$audit[[1L]]$error_code, "transport_error")
})

test_that("malformed callback receipts become bounded unknown attempts", {
  malformed <- list(
    matrix_ok = function(now) list(
      ok = matrix(TRUE), ack_code = "pulse",
      assert_time_ns = now, deassert_time_ns = now + 1,
      ack_time_ns = now + 1
    ),
    matrix_ack = function(now) list(
      ok = TRUE, ack_code = matrix("pulse"),
      assert_time_ns = now, deassert_time_ns = now + 1,
      ack_time_ns = now + 1
    ),
    unordered_times = function(now) list(
      ok = TRUE, ack_code = "pulse",
      assert_time_ns = now + 2, deassert_time_ns = now + 1,
      ack_time_ns = now + 3
    ),
    zero_interval = function(now) list(
      ok = TRUE, ack_code = "pulse",
      assert_time_ns = now, deassert_time_ns = now,
      ack_time_ns = now
    ),
    runtime_detail = function(now) list(
      ok = TRUE, ack_code = "pulse",
      assert_time_ns = now, deassert_time_ns = now + 1,
      ack_time_ns = now + 1, detail = new.env()
    ),
    oversized_detail = function(now) list(
      ok = TRUE, ack_code = "pulse",
      assert_time_ns = now, deassert_time_ns = now + 1,
      ack_time_ns = now + 1,
      detail = paste(rep("x", 1024^2 + 1L), collapse = "")
    )
  )

  for (name in names(malformed)) {
    make_result <- malformed[[name]]
    writer <- function(request) {
      if (identical(request$action, "stop")) {
        return(list(ok = TRUE, ack_code = "stop"))
      }
      make_result(request$command$issued_monotonic_ns)
    }
    trigger <- ttlTrigger(
      "callback", "left", 10, "mA", 100, 100, 1000,
      writer = writer
    )
    open_and_arm(trigger)
    receipt <- sendStim(
      trigger, 1, "left", 10, paste0("malformed-", name), 0
    )
    expect_identical(receipt$status, "unknown", info = name)
    expect_identical(
      receipt$error_class,
      "PhysioStream_trigger_transport_error",
      info = name
    )
    expect_identical(receipt$error_code, "transport_error", info = name)
    expect_identical(
      triggerState(trigger)$counters$attempted, 1, info = name
    )
    expect_identical(
      triggerState(trigger)$counters$unknown, 1, info = name
    )
  }
})

test_that("callback reentry and direct mutation are contained", {
  trigger <- NULL
  writer <- function(request) {
    if (request$action == "stop") {
      return(list(ok = TRUE, ack_code = "stop"))
    }
    sendStim(trigger, 1, "left", 10, "nested", 1)
  }
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  open_and_arm(trigger)
  receipt <- sendStim(trigger, 1, "left", 10, "outer", 0)
  expect_identical(receipt$status, "unknown")
  expect_false(trigger$busy)
  expect_equal(triggerState(trigger)$counters$attempted, 1)

  direct <- NULL
  writer <- function(request) {
    if (request$action == "stop") {
      return(list(ok = TRUE, ack_code = "stop"))
    }
    direct$state$counters$attempted <- 999
    list(
      ok = TRUE, ack_code = "bad",
      assert_time_ns = 0, deassert_time_ns = 1, ack_time_ns = 1
    )
  }
  direct <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  open_and_arm(direct)
  receipt <- sendStim(direct, 1, "left", 10, "outer", 0)
  expect_identical(receipt$status, "unknown")
  expect_identical(receipt$error_code, "runtime_mutation")
  expect_equal(triggerState(direct)$counters$attempted, 1)
})

test_that("failed callback stop leaves local emergency latch", {
  calls <- 0L
  writer <- function(request) {
    calls <<- calls + 1L
    list(ok = calls == 1L, ack_code = "stop")
  }
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  triggerOpen(trigger)
  armTrigger(trigger, "session", 0)
  expect_error(emergencyStop(trigger, "operator", 1), "latched locally")
  state <- triggerState(trigger)
  expect_true(state$lifecycle$stopped)
  expect_identical(state$arm$status, "disarmed")
  expect_identical(
    state$lifecycle$last_error_code,
    "emergency_stop_transport_failed"
  )
})

test_that("failed close deassert closes and latches local stop", {
  calls <- 0L
  writer <- function(request) {
    calls <<- calls + 1L
    list(ok = calls == 1L, ack_code = "stop")
  }
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = writer
  )
  triggerOpen(trigger)
  expect_error(triggerClose(trigger), "stop/deassert failure")
  state <- triggerState(trigger)
  expect_identical(state$lifecycle$status, "closed")
  expect_true(state$lifecycle$stopped)
  expect_identical(state$arm$status, "disarmed")
  expect_identical(
    state$lifecycle$last_error_code,
    "transport_close_failed"
  )
})

test_that("TTL constructor validates transport-specific fields", {
  common <- list(
    allowed_channels = "left", max_intensity = 10,
    intensity_unit = "mA", max_duration_ms = 100,
    refractory_ms = 100, deadman_ms = 1000
  )
  expect_error(
    do.call(ttlTrigger, c(list(transport = "call"), common)),
    "exactly one"
  )
  expect_error(
    do.call(ttlTrigger, c(list(
      transport = structure("serial", class = "transport")
    ), common)),
    "exactly one"
  )
  expect_error(
    do.call(ttlTrigger, c(list(transport = "serial"), common)),
    "`port`"
  )
  expect_error(
    do.call(ttlTrigger, c(list(
      transport = "callback", writer = NULL
    ), common)),
    "writer"
  )
  expect_error(
    do.call(ttlTrigger, c(list(
      transport = "callback", writer = identity, clock = function() 0
    ), common)),
    "clock injection"
  )
})

test_that("pySerial helper rejects partial writes and timeouts", {
  skip_if_not(
    requireNamespace("reticulate", quietly = TRUE),
    "reticulate unavailable"
  )
  skip_if_not(
    isTRUE(tryCatch({
      PhysioStream:::.ttl_make_serial_module()
      TRUE
    }, error = function(e) FALSE)),
    "pySerial unavailable"
  )
  reticulate::py_run_string(paste(
    "class _PhysioStreamShortWriteV1:",
    "    def write(self, payload):",
    "        return max(0, len(payload) - 1)",
    "    def flush(self):",
    "        return None",
    "",
    "class _PhysioStreamTimeoutWriteV1:",
    "    def write(self, payload):",
    "        raise TimeoutError('test write timeout')",
    "    def flush(self):",
    "        return None",
    "",
    "_physiostream_short_write_v1 = _PhysioStreamShortWriteV1()",
    "_physiostream_timeout_write_v1 = _PhysioStreamTimeoutWriteV1()",
    sep = "\n"
  ))
  payload <- as.integer(charToRaw("STIM\n"))
  expect_error(
    reticulate::py[["_physiostream_serial_write_v1"]](
      reticulate::py[["_physiostream_short_write_v1"]], payload
    ),
    "partial governed serial write"
  )
  expect_error(
    reticulate::py[["_physiostream_serial_write_v1"]](
      reticulate::py[["_physiostream_timeout_write_v1"]], payload
    ),
    "test write timeout"
  )
})

test_that("pySerial writes fail-safe stop and exact STIM frames on a PTY", {
  skip_if(.Platform$OS.type != "unix", "PTY integration requires Unix")
  skip_if_not(
    requireNamespace("reticulate", quietly = TRUE),
    "reticulate unavailable"
  )
  skip_if_not(
    isTRUE(tryCatch({
      reticulate::import("serial", convert = TRUE)
      TRUE
    }, error = function(e) FALSE)),
    "pySerial unavailable"
  )
  reticulate::py_run_string(paste(
    "def _physiostream_test_pty_open_v1():",
    "    import os",
    "    master, slave = os.openpty()",
    "    name = os.ttyname(slave)",
    "    os.close(slave)",
    "    return {'master': int(master), 'name': name}",
    "",
    "def _physiostream_test_pty_read_v1(master, timeout_s):",
    "    import os, select",
    "    ready, _, _ = select.select([int(master)], [], [], float(timeout_s))",
    "    if not ready:",
    "        raise TimeoutError('PTY read timeout')",
    "    return os.read(int(master), 65536).decode('ascii')",
    "",
    "def _physiostream_test_pty_close_v1(master):",
    "    import os",
    "    os.close(int(master))",
    "    return None",
    sep = "\n"
  ))
  pty <- reticulate::py[["_physiostream_test_pty_open_v1"]]()
  if (reticulate::is_py_object(pty)) {
    pty <- reticulate::py_to_r(pty)
  }
  on.exit(try(
    reticulate::py[["_physiostream_test_pty_close_v1"]](pty$master),
    silent = TRUE
  ), add = TRUE)

  trigger <- ttlTrigger(
    "serial", "left", 10, "mA", 100, 100, 1000,
    port = pty$name, line = 4L, pulse_width_ms = 5
  )
  triggerOpen(trigger)
  opened <- reticulate::py[["_physiostream_test_pty_read_v1"]](
    pty$master, 1
  )
  expect_true(startsWith(as.character(opened), "STOP,4,"))
  armTrigger(trigger, "session", 0)
  receipt <- sendStim(trigger, 1, "left", 10, "command-1", 0)
  written <- reticulate::py[["_physiostream_test_pty_read_v1"]](
    pty$master, 1
  )
  expect_identical(
    as.character(written), receipt$transport_details$frame
  )
  expect_identical(receipt$status, "acknowledged")
  triggerClose(trigger)
  closed <- reticulate::py[["_physiostream_test_pty_read_v1"]](
    pty$master, 1
  )
  expect_true(startsWith(as.character(closed), "STOP,4,"))
})
