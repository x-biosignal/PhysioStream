test_that("drift correction applies the documented sign convention", {
  observation <- seq(0, 10, by = 2)
  offset <- -0.4 + 0.005 * (observation - 5)
  model <- clockOffset(observation, offset, method = "ols")
  timestamps <- seq(0, 10, by = 0.25)
  corrected <- driftCorrect(timestamps, model, extrapolate = "error")
  expected <- timestamps - 0.4 + 0.005 * (timestamps - 5)

  expect_equal(as.numeric(corrected), expected, tolerance = 1e-14)
  audit <- attr(corrected, "clock_correction")
  expect_identical(audit$model_sha256, model$evidence_sha256)
  expect_equal(audit$applied_offset, expected - timestamps, tolerance = 1e-14)
  expect_true(all(audit$extrapolation_seconds == 0))
})

test_that("correction never crosses reset segments", {
  observation <- c(100:104, 1:5)
  labels <- rep(1:2, each = 5)
  offset <- c(rep(-90, 5), rep(20, 5))
  model <- clockOffset(
    observation, offset, method = "ols", segments = labels
  )
  corrected <- driftCorrect(
    observation, model, segments = labels, extrapolate = "error"
  )

  expect_equal(as.numeric(corrected), c(10:14, 21:25), tolerance = 1e-14)
  expect_error(driftCorrect(observation, model), "do not match")
  expect_error(
    driftCorrect(c(99, 100:104, 1:5), model,
                 segments = c(rep(1, 6), rep(2, 5)),
                 extrapolate = "error"),
    "extrapolate"
  )
})

test_that("bounded extrapolation and empty outputs are explicit", {
  model <- clockOffset(0:4, rep(0.1, 5), method = "ols")
  expect_error(
    driftCorrect(-31, model, max_extrapolation_seconds = 30),
    "bounded extrapolation"
  )
  corrected <- driftCorrect(-1, model, max_extrapolation_seconds = 1)
  expect_equal(as.numeric(corrected), -0.9, tolerance = 1e-14)
  expect_equal(attr(corrected, "clock_correction")$extrapolation_seconds, 1)

  empty <- driftCorrect(numeric(), model)
  expect_identical(as.numeric(empty), numeric())
  expect_identical(attr(empty, "clock_correction")$segment, integer())
})
