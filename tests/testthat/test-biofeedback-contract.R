test_that("scope construction is side-effect-free and validates identity", {
  source <- biofeedback_test_source(open = FALSE)
  before <- ringStats(source@buffer)
  scope <- biofeedbackScope(
    source, source_lifecycle = "own", launch = FALSE,
    clock = biofeedback_test_clock()
  )
  expect_s3_class(scope, "BiofeedbackScope")
  expect_identical(streamState(source), "created")
  expect_identical(ringStats(source@buffer), before)
  expect_identical(biofeedbackState(scope)$lifecycle, "created")
  expect_null(biofeedbackFrame(scope))
  expect_error(biofeedbackStop(scope), "requires a running scope")

  expect_error(
    biofeedbackScope(source, channels = "lef", launch = FALSE),
    class = "PhysioStream_biofeedback_channel_error"
  )
  no_units <- loopbackSource(
    biofeedback_test_info(units = c("", "")), 32L
  )
  expect_error(
    biofeedbackScope(no_units, launch = FALSE),
    "explicit non-empty unit"
  )
  irregular <- loopbackSource(
    biofeedback_test_info(rate = 0), 32L
  )
  expect_error(
    biofeedbackScope(irregular, launch = FALSE),
    "regular-rate"
  )
})

test_that("caller clocks cannot mutate runtime state during callbacks", {
  registry <- new.env(parent = emptyenv())
  registry$armed <- FALSE
  registry$scope <- NULL
  clock <- function() {
    if (registry$armed) {
      registry$scope$state$lifecycle <- "corrupt"
    }
    cpp_monotonic_ns()
  }
  source <- biofeedback_test_source()
  scope <- biofeedbackScope(source, launch = FALSE, clock = clock)
  registry$scope <- scope
  biofeedbackStart(scope)
  biofeedback_test_feed(source, 2L)
  registry$armed <- TRUE
  expect_error(
    biofeedbackStep(scope),
    "changed during monotonic clock"
  )
  expect_identical(biofeedbackState(scope)$lifecycle, "error_stopped")
})

test_that("pipeline and derived contracts are exact", {
  source <- biofeedback_test_source()
  other <- biofeedback_test_source(channels = "other", units = "uV")
  expect_error(
    biofeedbackScope(
      source, pipeline = streamPipeline(source = other), launch = FALSE
    ),
    "exact supplied source descriptor"
  )

  queued <- streamPipeline(source = source, chunk_size = 8L)
  pipelineEnqueue(
    queued,
    matrix(1, 1L, 2L, dimnames = list(NULL, c("left", "right"))),
    0, ingest_time_ns = cpp_monotonic_ns()
  )
  expect_error(
    biofeedbackScope(source, pipeline = queued, launch = FALSE),
    "empty ingress queue"
  )

  custom <- streamPipeline(source = source, chunk_size = 8L)
  onChunk(
    custom, pipeline_identity_callback,
    state = list(count = 0), name = "not_rms"
  )
  expect_error(
    biofeedbackScope(
      source,
      pipeline = custom,
      derived = list(rms = list(
        type = "emg_rms", channels = "left", unit = "uV",
        operation_name = "not_rms"
      )),
      launch = FALSE
    ),
    "bandpassRmsOp"
  )

  pipeline <- biofeedback_test_pipeline(source)
  scope <- biofeedbackScope(
    source,
    pipeline = pipeline,
    derived = list(
      rms = list(
        type = "emg_rms", channels = "left", unit = "uV",
        operation_name = "fixture_rms"
      ),
      power = list(
        type = "band_power", channels = "right", unit = "uV^2",
        operation_name = "fixture_rms"
      )
    ),
    launch = FALSE
  )
  expect_s3_class(scope, "BiofeedbackScope")
  expect_error(
    biofeedbackScope(
      source,
      pipeline = biofeedback_test_pipeline(source),
      derived = list(power = list(
        type = "band_power", channels = "left", unit = "uV",
        operation_name = "fixture_rms"
      )),
      launch = FALSE
    ),
    "does not match"
  )
})

test_that("sealed state and runtime identity reject tampering", {
  source <- biofeedback_test_source()
  scope <- biofeedbackScope(
    source, launch = FALSE, clock = biofeedback_test_clock()
  )
  state <- biofeedbackState(scope)
  state$lifecycle <- "running"
  expect_error(
    PhysioStream:::.biofeedback_validate_sealed(
      state, 16 * 1024^2, "state"
    ),
    "integrity"
  )

  scope$state$configuration$channels <- rev(
    scope$state$configuration$channels
  )
  expect_error(
    biofeedbackState(scope),
    class = "PhysioStream_biofeedback_state_error"
  )

  source2 <- biofeedback_test_source()
  scope2 <- biofeedbackScope(source2, launch = FALSE)
  onChunk(
    scope2$pipeline, pipeline_identity_callback,
    state = list(count = 0), name = "late"
  )
  expect_error(
    biofeedbackState(scope2),
    "pipeline identity changed"
  )
})
