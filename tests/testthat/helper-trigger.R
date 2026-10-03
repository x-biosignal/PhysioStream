trigger_state_raw <- function(x) {
  serialize(triggerState(x), NULL, version = 3L)
}

test_loopback_trigger <- function(
    refractory_ms = c(left = 100, right = 200),
    deadman_ms = 1000,
    audit_capacity = 32L,
    clock = NULL) {
  loopbackTrigger(
    allowed_channels = c("left", "right"),
    max_intensity = c(left = 20, right = 25),
    intensity_unit = "mA",
    max_duration_ms = c(left = 500, right = 750),
    refractory_ms = refractory_ms,
    deadman_ms = deadman_ms,
    audit_capacity = audit_capacity,
    clock = clock
  )
}

open_and_arm <- function(trigger, session_id = "session-1",
                         now_ns = 0) {
  triggerOpen(trigger)
  armTrigger(trigger, session_id, now_ns = now_ns)
  trigger
}

test_callback_writer <- function(pulse_ok = TRUE, stop_ok = TRUE) {
  log <- new.env(parent = emptyenv())
  log$requests <- list()
  writer <- function(request) {
    log$requests[[length(log$requests) + 1L]] <- request
    if (identical(request$action, "stop")) {
      return(list(ok = stop_ok, ack_code = "callback_stop"))
    }
    now <- request$command$issued_monotonic_ns
    list(
      ok = pulse_ok,
      ack_code = "callback_pulse",
      assert_time_ns = now,
      deassert_time_ns = now + 5e6,
      ack_time_ns = now + 5e6
    )
  }
  list(writer = writer, log = log)
}
