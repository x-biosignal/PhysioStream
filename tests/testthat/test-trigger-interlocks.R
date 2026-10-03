test_that("pre-attempt interlock failures are byte-identical", {
  trigger <- open_and_arm(test_loopback_trigger())
  cases <- list(
    function() sendStim(trigger, 21, "left", 100, "too-high", 0),
    function() sendStim(trigger, -1, "left", 100, "negative", 0),
    function() sendStim(trigger, 1, "unknown", 100, "bad-channel", 0),
    function() sendStim(trigger, 1, "left", 501, "too-long", 0),
    function() sendStim(trigger, 1, "left", 0, "zero-duration", 0),
    function() sendStim(trigger, 1, "left", 100, "bad id", 0)
  )
  for (case in cases) {
    before <- trigger_state_raw(trigger)
    expect_error(case())
    expect_identical(trigger_state_raw(trigger), before)
  }
  expect_equal(triggerState(trigger)$counters$attempted, 0)
})

test_that("refractory is per-channel and inclusive at the boundary", {
  trigger <- open_and_arm(test_loopback_trigger())
  first <- sendStim(trigger, 20, "left", 500, "left-1", 0)
  expect_identical(first$status, "acknowledged")
  right <- sendStim(trigger, 25, "right", 750, "right-1", 1)
  expect_identical(right$status, "acknowledged")

  before <- trigger_state_raw(trigger)
  expect_error(
    sendStim(trigger, 1, "left", 1, "left-early", 99999999),
    "refractory"
  )
  expect_identical(trigger_state_raw(trigger), before)
  boundary <- sendStim(
    trigger, 1, "left", 1, "left-boundary", 100000000
  )
  expect_identical(boundary$status, "acknowledged")
})

test_that("duplicate IDs never reach transport", {
  trigger <- open_and_arm(test_loopback_trigger())
  sendStim(trigger, 1, "left", 10, "same-id", 0)
  before <- trigger_state_raw(trigger)
  expect_error(
    sendStim(trigger, 1, "right", 10, "same-id", 1),
    "duplicate"
  )
  expect_identical(trigger_state_raw(trigger), before)
  expect_equal(triggerState(trigger)$counters$attempted, 1)
})

test_that("unknown attempts reserve command ID and refractory", {
  callback <- test_callback_writer(pulse_ok = FALSE)
  trigger <- ttlTrigger(
    "callback", "left", 10, "mA", 100, 100, 1000,
    writer = callback$writer
  )
  open_and_arm(trigger)
  receipt <- sendStim(trigger, 1, "left", 10, "unknown-1", 0)
  expect_identical(receipt$status, "unknown")
  expect_identical(receipt$error_code, "negative_acknowledgement")
  expect_equal(triggerState(trigger)$counters$attempted, 1)
  expect_equal(triggerState(trigger)$counters$unknown, 1)

  before <- trigger_state_raw(trigger)
  expect_error(
    sendStim(trigger, 1, "left", 10, "unknown-2", 99999999),
    "refractory"
  )
  expect_identical(trigger_state_raw(trigger), before)
  expect_error(
    sendStim(trigger, 1, "left", 10, "unknown-1", 100000000),
    "duplicate"
  )
  expect_identical(trigger_state_raw(trigger), before)
  second <- sendStim(
    trigger, 1, "left", 10, "unknown-2", 100000000
  )
  expect_identical(second$status, "unknown")
  expect_length(callback$log$requests, 3L)
})

test_that("bounded audit retains newest records and cumulative truncation", {
  trigger <- open_and_arm(test_loopback_trigger(
    refractory_ms = 0, audit_capacity = 2L
  ))
  for (i in 1:3) {
    sendStim(
      trigger, 1, "left", 10, paste0("command-", i),
      now_ns = i
    )
  }
  state <- triggerState(trigger)
  expect_length(state$audit, 2L)
  expect_identical(
    vapply(state$audit, function(x) x$command$command_id, character(1)),
    c("command-2", "command-3")
  )
  expect_identical(state$recent_command_ids, c("command-2", "command-3"))
  expect_equal(state$audit_truncated, 1)
  expect_equal(state$counters$audit_truncated, 1)
})

test_that("send-time dead-man expiry mutates only to safe disarm", {
  trigger <- open_and_arm(test_loopback_trigger(deadman_ms = 100))
  expect_error(
    sendStim(trigger, 1, "left", 10, "late", 100000001),
    "dead-man"
  )
  state <- triggerState(trigger)
  expect_identical(state$arm$status, "disarmed")
  expect_identical(state$arm$disarm_reason, "deadman_expired")
  expect_equal(state$counters$attempted, 0)
})
