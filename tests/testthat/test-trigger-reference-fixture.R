test_that("trigger reference fixture is hash-bound and schema-valid", {
  fixture_path <- system.file(
    "extdata", "trigger_reference.rds", package = "PhysioStream"
  )
  manifest_path <- system.file(
    "extdata", "trigger_reference.sha256", package = "PhysioStream"
  )
  expect_true(nzchar(fixture_path))
  expect_true(nzchar(manifest_path))
  expected <- strsplit(
    readLines(manifest_path, warn = FALSE)[[1L]], " +"
  )[[1L]][[1L]]
  expect_identical(
    digest::digest(
      file = fixture_path, algo = "sha256", serialize = FALSE
    ),
    expected
  )
  fixture <- readRDS(fixture_path)
  expect_identical(fixture$schema, "1.0.0")
  expect_identical(fixture$provenance$paho_version, "2.1.0")
  expect_identical(fixture$provenance$pyserial_version, "3.5")
  expect_true(all(unlist(fixture$boundaries, use.names = FALSE)))
  expect_identical(
    fixture$lifecycle_trace$case,
    c("closed_send", "disarmed_send", "stopped_send")
  )
  expect_identical(
    fixture$unknown_trace$outcome,
    c("unknown", "duplicate_error", "refractory_error", "unknown")
  )
})

test_that("public loopback matches independent command fixture", {
  fixture <- readRDS(system.file(
    "extdata", "trigger_reference.rds", package = "PhysioStream"
  ))
  config <- fixture$configuration
  trigger <- loopbackTrigger(
    config$allowed_channels, config$max_intensity,
    config$intensity_unit, config$max_duration_ms,
    config$refractory_ms, config$deadman_ms,
    audit_capacity = config$audit_capacity
  )
  open_and_arm(trigger, fixture$session_id, 0)
  accepted_index <- 0L
  for (i in seq_len(nrow(fixture$trace))) {
    case <- fixture$trace[i, ]
    before <- trigger_state_raw(trigger)
    result <- tryCatch(
      sendStim(
        trigger, case$intensity, case$channel, case$duration_ms,
        case$command_id, case$now_ns
      ),
      error = function(e) e
    )
    if (identical(case$outcome, "acknowledged")) {
      accepted_index <- accepted_index + 1L
      expected <- fixture$accepted[[accepted_index]]
      expect_false(inherits(result, "condition"), info = case$case)
      expect_identical(result$status, "acknowledged", info = case$case)
      expect_identical(result$command, expected$command, info = case$case)
      expect_identical(result$payload, expected$payload, info = case$case)
    } else {
      expect_true(
        inherits(result, "PhysioStream_error"),
        info = case$case
      )
      if (!isTRUE(case$state_changes)) {
        expect_identical(
          trigger_state_raw(trigger), before, info = case$case
        )
      }
    }
  }
  expect_equal(accepted_index, length(fixture$accepted))
  expect_identical(triggerState(trigger)$arm$status, "disarmed")
})

test_that("public TTL framing matches independent fixture", {
  fixture <- readRDS(system.file(
    "extdata", "trigger_reference.rds", package = "PhysioStream"
  ))
  for (case in fixture$ttl) {
    expect_identical(
      PhysioStream:::.trigger_ttl_frame(
        case$command, case$line, case$pulse_width_ms
      ),
      case$frame
    )
  }
  expect_identical(
    PhysioStream:::.trigger_stop_frame(fixture$ttl_stop$line),
    fixture$ttl_stop$frame
  )
})
