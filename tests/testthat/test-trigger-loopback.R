test_that("loopback records exact command, timing, and acknowledgement", {
  trigger <- open_and_arm(test_loopback_trigger())
  receipt <- sendStim(
    trigger, intensity = 2.5, channel = "left",
    duration_ms = 125, command_id = "command-1", now_ns = 10
  )
  expect_identical(receipt$status, "acknowledged")
  expect_identical(receipt$ack_code, "loopback_ack")
  expect_identical(receipt$lifecycle_generation, 1)
  expect_identical(receipt$arm_generation, 1)
  expect_identical(receipt$validated_at_ns, 10)
  expect_identical(receipt$attempted_at_ns, 10)
  expect_identical(receipt$acknowledged_at_ns, 10)
  expect_null(receipt$error_class)
  expect_null(receipt$error_code)
  expect_identical(receipt$command$intensity_unit, "mA")
  expect_identical(receipt$command$duration_ms, 125)
  expect_identical(
    jsonlite::fromJSON(receipt$payload)$payload_sha256,
    receipt$command$payload_sha256
  )
  state <- triggerState(trigger)
  expect_equal(state$counters$attempted, 1)
  expect_equal(state$counters$acknowledged, 1)
  expect_equal(state$counters$unknown, 0)
  expect_identical(state$audit[[1L]], receipt)
})

test_that("portable trigger state excludes runtime objects", {
  trigger <- open_and_arm(test_loopback_trigger())
  sendStim(trigger, 1, "left", 10, "command-1", 0)
  state <- triggerState(trigger)
  expect_null(PhysioStream:::.dsp_runtime_path(state))
  expect_null(PhysioStream:::.pipeline_object_path(state, "state"))
  roundtrip <- unserialize(serialize(state, NULL, version = 3L))
  expect_identical(roundtrip, state)
  expect_identical(
    state$state_sha256,
    PhysioStream:::.trigger_state_hash(roundtrip)
  )
})

test_that("caller mutation of snapshots and receipts cannot alter state", {
  trigger <- open_and_arm(test_loopback_trigger())
  receipt <- sendStim(trigger, 1, "left", 10, "command-1", 0)
  snapshot <- triggerState(trigger)
  before <- trigger_state_raw(trigger)
  snapshot$counters$attempted <- 999
  receipt$command$intensity <- 999
  expect_identical(trigger_state_raw(trigger), before)
})

test_that("deterministic injected clock drives loopback without implicit time", {
  values <- c(0, 0, 100000000)
  index <- 0L
  clock <- function() {
    index <<- index + 1L
    values[[index]]
  }
  trigger <- test_loopback_trigger(clock = clock)
  triggerOpen(trigger)
  armTrigger(trigger, "session")
  first <- sendStim(trigger, 1, "left", 10, "command-1")
  second <- sendStim(trigger, 1, "left", 10, "command-2")
  expect_identical(first$attempted_at_ns, 0)
  expect_identical(second$attempted_at_ns, 100000000)
  expect_identical(index, 3L)
})
