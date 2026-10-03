test_that("open, arm, heartbeat, disarm, and close are explicit", {
  trigger <- test_loopback_trigger()
  expect_invisible(triggerOpen(trigger))
  expect_identical(triggerState(trigger)$lifecycle$status, "open")
  expect_identical(triggerState(trigger)$arm$status, "disarmed")
  before <- trigger_state_raw(trigger)
  expect_error(triggerOpen(trigger), "already open")
  expect_identical(trigger_state_raw(trigger), before)

  expect_invisible(armTrigger(trigger, "session-1", now_ns = 0))
  state <- triggerState(trigger)
  expect_identical(state$arm$status, "armed")
  expect_identical(state$arm$generation, 1)
  expect_identical(state$arm$heartbeat_at_ns, 0)
  expect_invisible(triggerHeartbeat(trigger, now_ns = 1e9))
  expect_identical(triggerState(trigger)$arm$heartbeat_at_ns, 1e9)
  expect_invisible(disarmTrigger(trigger, "manual", now_ns = 1e9))
  before <- trigger_state_raw(trigger)
  expect_invisible(disarmTrigger(trigger, "manual", now_ns = 1e9))
  expect_identical(trigger_state_raw(trigger), before)
  expect_error(
    armTrigger(trigger, "session-1", now_ns = 1e9),
    "new session id"
  )
  expect_identical(trigger_state_raw(trigger), before)
  expect_invisible(armTrigger(trigger, "session-2", now_ns = 1e9))
  expect_invisible(triggerClose(trigger))
  expect_identical(triggerState(trigger)$lifecycle$status, "closed")
  before <- trigger_state_raw(trigger)
  expect_invisible(triggerClose(trigger))
  expect_identical(trigger_state_raw(trigger), before)
})

test_that("dead-man expiry disarms and cannot be renewed", {
  trigger <- open_and_arm(test_loopback_trigger(deadman_ms = 1000))
  expect_error(
    triggerHeartbeat(trigger, now_ns = 1000000001),
    "dead-man"
  )
  state <- triggerState(trigger)
  expect_identical(state$arm$status, "disarmed")
  expect_identical(state$arm$disarm_reason, "deadman_expired")
  expect_error(triggerHeartbeat(trigger, now_ns = 1000000001), "armed")
  expect_invisible(armTrigger(trigger, "session-2", now_ns = 1000000001))
  expect_identical(triggerState(trigger)$arm$generation, 2)
})

test_that("clock domains and monotonic order bind on first timed action", {
  trigger <- open_and_arm(test_loopback_trigger(), now_ns = 10)
  before <- trigger_state_raw(trigger)
  expect_error(triggerHeartbeat(trigger), "clock domains")
  expect_identical(trigger_state_raw(trigger), before)
  expect_error(triggerHeartbeat(trigger, now_ns = 9), "decreased")
  expect_identical(trigger_state_raw(trigger), before)
  expect_error(triggerHeartbeat(trigger, now_ns = 10.5), "exact")
  expect_identical(trigger_state_raw(trigger), before)

  values <- c(100, 101, 102)
  index <- 0L
  clock <- function() {
    index <<- index + 1L
    values[[index]]
  }
  injected <- test_loopback_trigger(clock = clock)
  triggerOpen(injected)
  armTrigger(injected, "session")
  triggerHeartbeat(injected)
  expect_identical(triggerState(injected)$clock$mode, "injected")
  expect_error(triggerHeartbeat(injected, now_ns = 102), "clock domains")
})

test_that("emergency stop latches locally and requires close-open", {
  trigger <- open_and_arm(test_loopback_trigger())
  expect_invisible(emergencyStop(trigger, "operator", now_ns = 1))
  state <- triggerState(trigger)
  expect_true(state$lifecycle$stopped)
  expect_identical(state$arm$status, "disarmed")
  expect_error(armTrigger(trigger, "session-2", now_ns = 2), "not stopped")
  triggerClose(trigger)
  triggerOpen(trigger)
  expect_false(triggerState(trigger)$lifecycle$stopped)
  expect_identical(triggerState(trigger)$arm$status, "disarmed")
})

test_that("tampered state and runtime registries fail loud", {
  trigger <- test_loopback_trigger()
  trigger$state$counters$opens <- 10
  expect_error(triggerState(trigger), "hash")

  trigger <- test_loopback_trigger()
  trigger$runtime$transport <- "mqtt"
  expect_error(triggerState(trigger), "runtime registry")

  trigger <- test_loopback_trigger()
  trigger$extra <- TRUE
  expect_error(triggerState(trigger), "intact")
})
