test_that("runtime drains bounded chunks and emits immutable frames", {
  source <- biofeedback_test_source(rate = 100)
  scope <- biofeedbackScope(
    source,
    update_hz = 20,
    window_seconds = 1,
    max_points = 64L,
    launch = FALSE,
    clock = biofeedback_test_clock()
  )
  expect_invisible(biofeedbackStart(scope))
  fixture <- biofeedback_test_feed(source, 20L)
  result <- biofeedbackStep(scope)
  expect_true(result$updated)
  expect_equal(result$n_processed, 4)

  frame <- biofeedbackFrame(scope)
  expect_equal(frame$signal_time, tail(fixture$timestamps, 1L))
  expect_length(frame$traces, 2L)
  expect_true(all(vapply(
    frame$traces, function(x) length(x$values) <= 64L, logical(1)
  )))
  expect_equal(frame$diagnostics$source$fill, 0)
  expect_equal(frame$diagnostics$source$session_overwritten, 0)
  expect_equal(frame$diagnostics$pipeline$session_dropped_samples, 0)

  copied <- biofeedbackFrame(scope)
  copied$traces[[1L]]$values[[1L]] <- -999
  expect_false(identical(copied, biofeedbackFrame(scope)))
  frame_before <- serialize(biofeedbackFrame(scope), NULL, version = 3L)
  empty <- biofeedbackStep(scope)
  expect_false(empty$updated)
  expect_identical(
    serialize(biofeedbackFrame(scope), NULL, version = 3L),
    frame_before
  )
  expect_invisible(biofeedbackStop(scope))
  expect_identical(biofeedbackState(scope)$lifecycle, "stopped")
  expect_invisible(biofeedbackStop(scope))
  expect_error(biofeedbackStep(scope), "requires a running scope")
})

test_that("committed RMS and band-power traces use pipeline output", {
  source <- biofeedback_test_source(channels = "emg", units = "uV")
  pipeline <- biofeedback_test_pipeline(source, window_samples = 2L)
  scope <- biofeedbackScope(
    source,
    pipeline = pipeline,
    channels = "emg",
    derived = list(
      rms = list(
        type = "emg_rms", channels = "emg", unit = "uV",
        operation_name = "fixture_rms"
      ),
      power = list(
        type = "band_power", channels = "emg", unit = "uV^2",
        operation_name = "fixture_rms"
      )
    ),
    update_hz = 20,
    launch = FALSE,
    clock = biofeedback_test_clock()
  )
  biofeedbackStart(scope)
  loopbackFeed(
    source,
    matrix(c(3, 4, 0, 0), 4L, 1L),
    c(0, 0.01, 0.02, 0.03)
  )
  biofeedbackStep(scope)
  traces <- biofeedbackFrame(scope)$traces
  rms <- traces[["emg_rms:rms:emg"]]$values
  power <- traces[["band_power:power:emg"]]$values
  expect_equal(power, rms^2, tolerance = 1e-14)
  expect_equal(
    rms,
    c(3, sqrt(12.5), sqrt(8), 0),
    tolerance = 1e-14
  )
})

test_that("external updates are atomic and sequence-governed", {
  source <- biofeedback_test_source(units = c("uV", "uV"))
  scope <- biofeedbackScope(
    source,
    derived = list(
      hbo_left = list(
        type = "external", unit = "uM", gain = 1,
        target_range = c(-1, 1)
      ),
      hbo_right = list(
        type = "external", unit = "uM", gain = 1,
        threshold = 0
      )
    ),
    launch = FALSE,
    clock = biofeedback_test_clock()
  )
  biofeedbackStart(scope)
  receipt <- biofeedbackUpdate(
    scope,
    c(hbo_left = 0.25, hbo_right = -0.5),
    timestamp = 1,
    units = c("uM", "uM"),
    sequence = 0
  )
  expect_identical(receipt$names, c("hbo_left", "hbo_right"))
  expect_equal(receipt$sequence, 0)
  before <- serialize(biofeedbackState(scope), NULL, version = 3L)
  before_frame <- serialize(biofeedbackFrame(scope), NULL, version = 3L)
  expect_error(
    biofeedbackUpdate(
      scope, c(hbo_left = 1, hbo_right = 2), 2,
      units = c("uM", "wrong"), sequence = 1
    ),
    "do not match"
  )
  expect_identical(
    serialize(biofeedbackState(scope), NULL, version = 3L), before
  )
  expect_identical(
    serialize(biofeedbackFrame(scope), NULL, version = 3L), before_frame
  )
  expect_error(
    biofeedbackUpdate(scope, c(hbo_left = 1), 2, sequence = 2),
    "contiguous"
  )
  expect_error(
    biofeedbackUpdate(scope, c(hbo_left = 1), 0.5, sequence = 1),
    "stale"
  )
})

test_that("owned sources close on normal and terminal paths", {
  owned <- biofeedback_test_source(open = FALSE)
  scope <- biofeedbackScope(
    owned, source_lifecycle = "own", launch = FALSE,
    clock = biofeedback_test_clock()
  )
  biofeedbackStart(scope)
  expect_identical(streamState(scope$source), "open")
  biofeedbackStop(scope)
  expect_identical(streamState(scope$source), "closed")

  failing <- biofeedback_test_source(open = FALSE)
  pipeline <- streamPipeline(source = failing, chunk_size = 8L)
  onChunk(
    pipeline,
    function(chunk, state, context) stop("fixture failure"),
    state = list(), name = "fail"
  )
  failed_scope <- biofeedbackScope(
    failing, pipeline = pipeline, source_lifecycle = "own",
    launch = FALSE, clock = biofeedback_test_clock()
  )
  biofeedbackStart(failed_scope)
  biofeedback_test_feed(failed_scope$source, 2L)
  expect_error(biofeedbackStep(failed_scope), "fixture failure")
  expect_identical(
    biofeedbackState(failed_scope)$lifecycle, "error_stopped"
  )
  expect_identical(streamState(failed_scope$source), "closed")
})
