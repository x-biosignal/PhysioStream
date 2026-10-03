test_that("loopback trigger validates and seals configuration", {
  trigger <- test_loopback_trigger()
  expect_s3_class(trigger, "LoopbackTrigger")
  expect_s3_class(trigger, "TriggerBackend")
  state <- triggerState(trigger)
  expect_identical(state$schema, "1.0.0")
  expect_identical(state$lifecycle$status, "closed")
  expect_identical(state$arm$status, "disarmed")
  expect_identical(
    state$configuration$max_intensity,
    c(left = 20, right = 25)
  )
  expect_identical(
    state$configuration$refractory_ns,
    c(left = 1e8, right = 2e8)
  )
  expect_identical(
    state$state_sha256,
    PhysioStream:::.trigger_state_hash(state)
  )
  expect_match(capture.output(print(trigger)), "attempted=0")
})

test_that("trigger configuration rejects ambiguous or unsafe values", {
  args <- list(
    allowed_channels = c("left", "right"),
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 100,
    deadman_ms = 1000
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      allowed_channels = c("left", "left")
    ))),
    "unique"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      allowed_channels = matrix("left")
    ))),
    "unique"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      intensity_unit = structure("mA", class = "unit")
    ))),
    "valid non-empty string"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      max_intensity = c(left = 10, other = 20)
    ))),
    "exactly channel-named"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      max_duration_ms = 0
    ))),
    "channel-named"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      refractory_ms = -1
    ))),
    "channel-named"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      deadman_ms = 0
    ))),
    "invalid value"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(
      audit_capacity = 0
    ))),
    "invalid value"
  )
  expect_error(
    do.call(loopbackTrigger, modifyList(args, list(clock = 1))),
    "clock"
  )
})

test_that("trigger identifiers reject decorated character scalars", {
  trigger <- test_loopback_trigger()
  open_and_arm(trigger, "session-1", 0)
  before <- trigger_state_raw(trigger)

  expect_error(
    sendStim(
      trigger, 1, "left", 1, matrix("command-1"), now_ns = 1
    ),
    "valid non-empty string"
  )
  expect_identical(trigger_state_raw(trigger), before)

  expect_error(
    sendStim(
      trigger, 1, structure("left", class = "channel"), 1,
      "command-1", now_ns = 1
    ),
    "valid non-empty string"
  )
  expect_identical(trigger_state_raw(trigger), before)
})

test_that("canonical command JSON and hash use fixed fields", {
  canonical <- PhysioStream:::.trigger_canonical_command(
    "command-1", "session-1", "left", 2.5, "mA", 100, 123, "loopback"
  )
  parsed <- jsonlite::fromJSON(canonical$payload, simplifyVector = TRUE)
  expect_named(
    canonical$command,
    c(
      "schema", "command_id", "session_id", "channel", "intensity",
      "intensity_unit", "duration_ms", "issued_monotonic_ns", "backend",
      "payload_sha256"
    )
  )
  expect_identical(parsed$command_id, "command-1")
  expect_identical(parsed$payload_sha256,
                   canonical$command$payload_sha256)
  without_hash <- canonical$command
  without_hash$payload_sha256 <- NULL
  payload <- unclass(jsonlite::toJSON(
    without_hash, auto_unbox = TRUE, digits = NA, null = "null",
    pretty = FALSE
  ))
  expect_identical(
    canonical$command$payload_sha256,
    digest::digest(charToRaw(payload), algo = "sha256", serialize = FALSE)
  )
})

test_that("TTL frames have canonical fields and CRC32", {
  canonical <- PhysioStream:::.trigger_canonical_command(
    "command-1", "session-1", "left", 2.5, "mA", 100, 123, "ttl"
  )
  frame <- PhysioStream:::.trigger_ttl_frame(
    canonical$command, line = 2L, pulse_width_ms = 5
  )
  expect_true(endsWith(frame, "\n"))
  text <- sub("\n$", "", frame)
  split <- strsplit(text, ",", fixed = TRUE)[[1L]]
  crc <- tail(split, 1L)
  body <- paste(head(split, -1L), collapse = ",")
  expect_identical(
    crc,
    digest::digest(charToRaw(body), algo = "crc32", serialize = FALSE)
  )
  expect_identical(split[c(1L, 5L, 6L)], c("STIM", "2", "left"))
  stop_frame <- PhysioStream:::.trigger_stop_frame(2L)
  stop_text <- sub("\n$", "", stop_frame)
  stop_split <- strsplit(stop_text, ",", fixed = TRUE)[[1L]]
  expect_identical(stop_split[1:2], c("STOP", "2"))
  expect_identical(
    stop_split[[3L]],
    digest::digest(charToRaw("STOP,2"), algo = "crc32", serialize = FALSE)
  )
  bad_command <- canonical$command
  bad_command$channel <- "bad,channel"
  expect_error(
    PhysioStream:::.trigger_ttl_frame(
      bad_command, 2L, 5
    ),
    "safe ASCII"
  )
})
