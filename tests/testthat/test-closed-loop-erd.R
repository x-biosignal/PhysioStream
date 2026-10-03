test_that("ERD detector validates band and hysteresis", {
  expect_s3_class(
    erdIntentOp("eeg", 200, c(8, 13), 50, 20),
    "PipelineOperation"
  )
  expect_error(
    erdIntentOp("eeg", 200, c(13, 8), 50, 20),
    "band"
  )
  expect_error(
    erdIntentOp("eeg", 20, c(8, 13), 50, 20),
    "Nyquist"
  )
  expect_error(
    erdIntentOp(
      "eeg", 200, c(8, 13), 50, 20,
      enter_percent = -10, release_percent = -20
    ),
    "less"
  )
  expect_error(
    erdIntentOp("eeg", 200, c(8, 13), 50, 20,
                min_baseline_power = 0),
    "min_baseline_power"
  )
})

test_that("ERD detection is causal and chunk invariant", {
  sampling_rate <- 200
  time <- (0:699) / sampling_rate
  amplitude <- c(rep(1, 350), rep(0.2, 170), rep(1, 180))
  values <- amplitude * cos(2 * pi * 10 * time)
  detector <- erdIntentOp(
    "eeg", sampling_rate, c(8, 13),
    baseline_samples = 100,
    power_window_samples = 20,
    enter_percent = -50,
    release_percent = -20,
    min_on_samples = 3,
    min_off_samples = 3
  )
  whole <- run_detector_partitions(
    detector, values, length(values), "eeg", sampling_rate
  )
  split <- run_detector_partitions(
    detector, values, c(17, 1, 43, 89, 2, 111, 53, 7, 97, 83, 41, 156),
    "eeg", sampling_rate
  )
  expect_length(whole$events, 1L)
  expect_identical(split$events, whole$events)
  event <- jsonlite::fromJSON(
    whole$events[[1L]]$value, simplifyVector = FALSE
  )
  expect_identical(event$detector, "erd_intent")
  expect_lte(event$score, -50)
  expect_gte(event$sample_index, 350)
  expect_lt(event$sample_index, 400)
})

test_that("ERD detector rejects a zero-power baseline", {
  pipeline <- streamPipeline(chunk_size = 100L)
  onChunk(
    pipeline,
    erdIntentOp(
      "eeg", 200, c(8, 13), baseline_samples = 10,
      power_window_samples = 5
    )
  )
  pipelineEnqueue(
    pipeline,
    matrix(0, nrow = 100, ncol = 1L,
           dimnames = list(NULL, "eeg"))
  )
  expect_error(pipelineStep(pipeline), "baseline power")
  expect_equal(pipelineState(pipeline)$counters$processed_chunks, 0)
})
