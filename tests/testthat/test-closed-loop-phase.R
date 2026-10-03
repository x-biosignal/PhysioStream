test_that("phase detector validates governed phase configuration", {
  expect_s3_class(
    phaseTargetOp("eeg", 1000, 10, 90),
    "PipelineOperation"
  )
  expect_error(phaseTargetOp("eeg", 100, 50, 90), "frequency")
  expect_error(
    phaseTargetOp("eeg", 1000, 10, 90, tolerance_degrees = 21),
    "tolerance"
  )
  expect_error(
    phaseTargetOp("eeg", 1000, 0.001, 90),
    "trailing window"
  )
  wrapped <- phaseTargetOp("eeg", 1000, 10, 450)
  expect_equal(wrapped$state$configuration$target_degrees, 90)
})

test_that("phase events are causal, chunk invariant, and within 20 degrees", {
  sampling_rate <- 500
  frequency <- 9
  time <- (0:799) / sampling_rate
  values <- 1.7 + 2.5 * cos(2 * pi * frequency * time + 0.73)
  detector <- phaseTargetOp(
    "eeg", sampling_rate, frequency, target_degrees = 350,
    window_cycles = 2, tolerance_degrees = 10,
    min_amplitude = 2, min_fit = 0.99
  )
  whole <- run_detector_partitions(
    detector, values, length(values), "eeg", sampling_rate
  )
  split <- run_detector_partitions(
    detector, values,
    c(1, 3, 47, 2, 101, 29, 17, 83, 211, 53, 97, 156),
    "eeg", sampling_rate
  )
  expect_gt(length(whole$events), 5L)
  expect_identical(split$events, whole$events)
  decoded <- lapply(
    whole$events,
    function(x) jsonlite::fromJSON(x$value, simplifyVector = FALSE)
  )
  errors <- vapply(decoded, `[[`, numeric(1), "phase_error_degrees")
  fits <- vapply(decoded, `[[`, numeric(1), "fit")
  amplitudes <- vapply(decoded, `[[`, numeric(1), "amplitude")
  expect_lte(max(abs(errors)), 10)
  expect_gte(min(fits), 0.99)
  expect_equal(amplitudes, rep(2.5, length(amplitudes)), tolerance = 1e-10)
})

test_that("phase delay is bound before registration", {
  sampling_rate <- 500
  frequency <- 10
  time <- (0:499) / sampling_rate
  values <- 3 * cos(2 * pi * frequency * time + 0.2)
  setup <- closed_loop_controller(
    phaseTargetOp(
      "eeg", sampling_rate, frequency, 90,
      window_cycles = 2, tolerance_degrees = 20,
      min_amplitude = 1, min_fit = 0.99
    ),
    delay_ms = 15
  )
  closedLoopStart(setup$controller, "phase-session", now_ns = 0)
  result <- closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "eeg")),
    timestamps = time,
    now_ns = 1e6
  )
  expect_gt(length(result$detections), 0L)
  errors <- vapply(
    result$detections, `[[`, numeric(1), "phase_error_degrees"
  )
  expect_lte(max(abs(errors)), 20)
  expect_length(result$actions, 0L)
  expect_equal(
    vapply(
      closedLoopState(setup$controller)$pending,
      function(x) x$due_monotonic_ns -
        x$detection_commit_monotonic_ns,
      numeric(1)
    ),
    rep(15e6, length(result$detections))
  )
  closedLoopStop(setup$controller, now_ns = 2e6)
})

test_that("phase detector gates low amplitude and poor fit", {
  sampling_rate <- 500
  time <- (0:499) / sampling_rate
  low <- run_detector_partitions(
    phaseTargetOp(
      "eeg", sampling_rate, 10, 90,
      window_cycles = 2, min_amplitude = 2, min_fit = 0.9
    ),
    0.5 * cos(2 * pi * 10 * time),
    c(123, 177, 200),
    "eeg",
    sampling_rate
  )
  expect_length(low$events, 0L)
})
