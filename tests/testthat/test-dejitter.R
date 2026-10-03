test_that("least-squares and first-anchor grids are closed form", {
  raw <- c(10.001, 10.010, 10.021, 10.029)
  least_squares <- dejitter(raw, 100, anchor = "least_squares")
  intercept <- mean(raw - (0:3) / 100)
  expect_equal(as.numeric(least_squares), intercept + (0:3) / 100,
               tolerance = 1e-15)
  expect_equal(attr(least_squares, "dejitter")$residual,
               raw - as.numeric(least_squares), tolerance = 0)

  first <- dejitter(raw, 100, anchor = "first")
  expect_equal(as.numeric(first), raw[[1]] + (0:3) / 100, tolerance = 0)
})

test_that("de-jitter grids do not bridge resets", {
  raw <- c(10, 10.011, 10.019, 1, 1.009, 1.021)
  segment <- rep(1:2, each = 3)
  regular <- dejitter(raw, 100, segments = segment, anchor = "first")

  expect_equal(
    as.numeric(regular),
    c(10, 10.01, 10.02, 1, 1.01, 1.02),
    tolerance = 1e-15
  )
  expect_identical(attr(regular, "dejitter")$segment, segment)
  expect_error(dejitter(raw, 100), "decrease")
  expect_error(
    dejitter(raw, 100, segments = segment, max_residual_seconds = 5e-4),
    "exceeds"
  )
})

test_that("de-jitter validation is exact and empty-safe", {
  expect_error(dejitter(1:3, 0), "positive")
  expect_error(dejitter(1:3, 10, anchor = "f"), "exactly")
  expect_error(dejitter(c(1, NA), 10), "finite")
  empty <- dejitter(numeric(), 10)
  expect_identical(as.numeric(empty), numeric())
  expect_identical(attr(empty, "dejitter")$summary$segment, integer())
})
