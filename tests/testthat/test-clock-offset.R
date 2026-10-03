test_that("centered OLS recovers large-origin clocks exactly", {
  times <- 1e12 + seq(0, 9.5, by = 0.5)
  origin <- median(times)
  offsets <- -0.35 + 0.005 * (times - origin)
  model <- clockOffset(times, offsets, method = "ols")

  expect_s3_class(model, "StreamClockModel")
  expect_equal(model$origin, origin, tolerance = 0)
  expect_equal(model$segments$intercept, -0.35, tolerance = 1e-12)
  expect_equal(model$segments$drift, 0.005, tolerance = 1e-12)
  expect_equal(model$segments$drift_ppm, 5000, tolerance = 1e-9)
  expect_identical(model$sign_convention, "master = device + offset")
  expect_identical(unserialize(serialize(model, NULL)), model)
  expect_output(print(model), "StreamClockModel")
})

test_that("reset segments are fitted independently", {
  times <- c(10:14, 1:5)
  labels <- rep(1:2, each = 5)
  offsets <- c(
    0.1 + 0.001 * (10:14 - 12),
    -0.2 - 0.002 * (1:5 - 3)
  )
  model <- clockOffset(
    times, offsets, method = "ols", segments = labels
  )

  expect_equal(model$segments$intercept, c(0.1, -0.2), tolerance = 1e-14)
  expect_equal(model$segments$drift, c(0.001, -0.002), tolerance = 1e-14)
  expect_equal(model$segments$n_total, c(5, 5), tolerance = 0)
})

test_that("Huber and RANSAC reject isolated clock outliers", {
  times <- as.double(1:30)
  offsets <- 0.2 + 0.005 * (times - median(times))
  contaminated <- offsets
  contaminated[c(3, 17, 29)] <- contaminated[c(3, 17, 29)] + c(1, -2, 3)

  huber <- clockOffset(times, contaminated, method = "huber")
  expect_equal(huber$segments$intercept, 0.2, tolerance = 1e-10)
  expect_equal(huber$segments$drift, 0.005, tolerance = 1e-10)
  expect_equal(huber$segments$n_inlier, 27, tolerance = 0)

  set.seed(918)
  before <- .Random.seed
  ransac <- clockOffset(times, contaminated, method = "ransac", seed = 77)
  expect_identical(.Random.seed, before)
  expect_equal(ransac$segments$intercept, 0.2, tolerance = 1e-12)
  expect_equal(ransac$segments$drift, 0.005, tolerance = 1e-12)
  expect_equal(ransac$segments$n_inlier, 27, tolerance = 0)
})

test_that("clock evidence and enum validation fail loudly", {
  expect_error(clockOffset(1:3, 1:2), "equal lengths")
  expect_error(clockOffset(c(1, 1, 2), c(0, 1, 0)), "contradictory")
  expect_error(clockOffset(1:4, 1:4, segments = c(1, 2, 1, 2)),
               "non-recurring")
  expect_error(clockOffset(1:3, 1:3, method = "o"), "exactly")
  expect_error(clockOffset(1:3, 1:3, method = c("ols", "huber")),
               "one exact scalar")

  model <- clockOffset(1:3, c(0.1, 0.2, 0.3), method = "ols")
  model$segments$drift <- 10
  expect_error(driftCorrect(1:3, model), "hash")

  model <- clockOffset(1:3, c(0.1, 0.2, 0.3), method = "ols")
  model$runtime <- new.env(parent = emptyenv())
  expect_error(driftCorrect(1:3, model), "invalid")
})
