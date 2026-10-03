test_that("EMG onset detector validates exact governed inputs", {
  expect_s3_class(
    emgOnsetOp("emg", 1000, 40, 10),
    "PipelineOperation"
  )
  expect_error(emgOnsetOp(factor("emg"), 1000, 40, 10), "channel")
  expect_error(emgOnsetOp("emg", c(1000, 1000), 40, 10), "sampling_rate")
  expect_error(emgOnsetOp("emg", 1000, 1, 10), "baseline_samples")
  expect_error(emgOnsetOp("emg", 1000, 40, 0), "rms_window_samples")
  expect_error(
    emgOnsetOp("emg", 1000, 40, 10, enter_z = 2, release_z = 2),
    "greater"
  )
  expect_error(
    emgOnsetOp("emg", 1000, 40, 10, min_on_samples = 1.5),
    "min_on_samples"
  )
  expect_error(
    emgOnsetOp("emg", 1000, 40, 10, min_baseline_sd = 0),
    "min_baseline_sd"
  )
})

test_that("EMG onset identity is invariant to arbitrary chunking", {
  values <- closed_loop_emg()
  detector <- emgOnsetOp(
    "emg", 1000, baseline_samples = 50, rms_window_samples = 8,
    enter_z = 6, release_z = 2, min_on_samples = 3,
    min_off_samples = 4
  )
  whole <- run_detector_partitions(
    detector, values, length(values), "emg", 1000
  )
  split <- run_detector_partitions(
    detector, values, c(1, 7, 13, 2, 31, 5, 19, 11, 17, 23, 25),
    "emg", 1000
  )
  expect_length(whole$events, 2L)
  expect_identical(split$events, whole$events)
  decoded <- lapply(
    whole$events,
    function(x) jsonlite::fromJSON(x$value, simplifyVector = FALSE)
  )
  expect_identical(
    vapply(decoded, `[[`, character(1), "detector"),
    rep("emg_onset", 2L)
  )
  expect_equal(
    vapply(decoded, `[[`, numeric(1), "sample_index"),
    c(83, 125)
  )
})

test_that("EMG detector warms up and rejects degenerate calibration", {
  detector <- emgOnsetOp(
    "emg", 1000, baseline_samples = 10, rms_window_samples = 5
  )
  short <- run_detector_partitions(
    detector, rep(1, 10), c(3, 7), "emg", 1000
  )
  expect_length(short$events, 0L)

  pipeline <- streamPipeline(chunk_size = 30L)
  onChunk(pipeline, detector)
  pipelineEnqueue(
    pipeline,
    matrix(rep(1, 30), ncol = 1L, dimnames = list(NULL, "emg"))
  )
  before <- pipelineState(pipeline)
  expect_error(pipelineStep(pipeline), "baseline scale")
  after <- pipelineState(pipeline)
  expect_identical(after$sha256, before$sha256)
  expect_length(after$queue, 1L)
})

test_that("EMG detector requires exact channel identity", {
  pipeline <- streamPipeline(chunk_size = 100L)
  onChunk(
    pipeline,
    emgOnsetOp("emg", 1000, baseline_samples = 10,
               rms_window_samples = 3)
  )
  pipelineEnqueue(
    pipeline,
    matrix(stats::rnorm(30), ncol = 1L,
           dimnames = list(NULL, "other"))
  )
  expect_error(pipelineStep(pipeline), "absent")
})

test_that("detectors reject timestamps inconsistent with sampling rate", {
  pipeline <- streamPipeline(chunk_size = 30L)
  onChunk(
    pipeline,
    emgOnsetOp("emg", 1000, baseline_samples = 10,
               rms_window_samples = 3)
  )
  pipelineEnqueue(
    pipeline,
    matrix(stats::rnorm(30), ncol = 1L,
           dimnames = list(NULL, "emg")),
    timestamps = seq(0, by = 0.002, length.out = 30)
  )
  expect_error(pipelineStep(pipeline), "sampling_rate")
  expect_equal(pipelineState(pipeline)$counters$processed_chunks, 0)
})
