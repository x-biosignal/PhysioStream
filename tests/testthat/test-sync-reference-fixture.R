test_that("clock synchronization reference is hash-bound and closed form", {
  extdata <- system.file("extdata", package = "PhysioStream", mustWork = TRUE)
  path <- file.path(extdata, "sync_reference.rds")
  manifest <- readLines(
    file.path(extdata, "sync_reference.sha256"), warn = FALSE
  )
  expected_sha <- strsplit(manifest, " ", fixed = TRUE)[[1L]][[1L]]
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    expected_sha
  )
  reference <- readRDS(path)
  expect_identical(reference$schema, "1.0.0")
  expect_identical(
    reference$sign_convention, "master = device + offset"
  )

  for (case in reference$exact_cases) {
    model <- clockOffset(
      case$clock_times, case$clock_values, method = "ols"
    )
    expect_equal(model$origin, case$fit_origin, tolerance = 0)
    expect_equal(model$segments$intercept, case$intercept, tolerance = 1e-12)
    expect_equal(model$segments$drift, case$drift, tolerance = 1e-12)
    expect_equal(
      as.numeric(driftCorrect(
        case$clock_times, model, extrapolate = "error"
      )),
      case$corrected,
      tolerance = 1e-12
    )
  }

  reset <- reference$reset
  model <- clockOffset(
    reset$clock_times, reset$clock_values,
    method = "ols", segments = reset$segment
  )
  expect_equal(model$segments$intercept, reset$intercept, tolerance = 1e-12)
  expect_equal(model$segments$drift, reset$drift, tolerance = 1e-12)
  expect_equal(
    as.numeric(driftCorrect(
      reset$clock_times, model, segments = reset$segment,
      extrapolate = "error"
    )),
    reset$corrected,
    tolerance = 1e-12
  )

  jitter <- reference$dejitter
  expect_equal(
    as.numeric(dejitter(
      jitter$raw, jitter$rate, segments = jitter$segment
    )),
    jitter$expected,
    tolerance = 1e-15
  )
})
