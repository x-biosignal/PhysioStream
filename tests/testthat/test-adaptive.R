adaptive_oracle <- function(signal, reference, algorithm, n_taps,
                            step_size = NULL, epsilon = NULL,
                            leakage = 0, forgetting = NULL, delta = NULL) {
  weights <- rep(0, n_taps)
  history <- rep(0, n_taps - 1L)
  covariance <- diag(1 / if (is.null(delta)) 1 else delta, n_taps)
  estimate <- residual <- numeric(length(signal))
  for (i in seq_along(signal)) {
    u <- c(reference[[i]], history)
    estimate[[i]] <- sum(weights * u)
    residual[[i]] <- signal[[i]] - estimate[[i]]
    if (algorithm == "lms") {
      weights <- (1 - leakage) * weights +
        step_size * residual[[i]] * u
    } else if (algorithm == "nlms") {
      weights <- (1 - leakage) * weights +
        step_size * residual[[i]] * u / (epsilon + sum(u^2))
    } else {
      product <- as.numeric(covariance %*% u)
      gain <- product / (forgetting + sum(u * product))
      weights <- weights + gain * residual[[i]]
      covariance <- (
        covariance - tcrossprod(gain, as.numeric(u %*% covariance))
      ) / forgetting
      covariance <- (covariance + t(covariance)) / 2
    }
    if (length(history)) {
      history <- head(u, -1L)
    }
  }
  list(residual = residual, weights = weights)
}

test_that("adaptive filters match independent sample recursions", {
  set.seed(71)
  reference <- rnorm(300)
  signal <- stats::filter(reference, c(0.7, -0.25, 0.1),
                          method = "convolution", sides = 1)
  signal[is.na(signal)] <- 0
  signal <- as.numeric(signal) + rnorm(300, sd = 0.01)

  cases <- list(
    list(
      object = lmsFilter(3L, 0.02, leakage = 0.001),
      args = list(algorithm = "lms", n_taps = 3L,
                  step_size = 0.02, leakage = 0.001)
    ),
    list(
      object = nlmsFilter(3L, 0.5, epsilon = 1e-7, leakage = 0.001),
      args = list(algorithm = "nlms", n_taps = 3L,
                  step_size = 0.5, epsilon = 1e-7, leakage = 0.001)
    ),
    list(
      object = rlsFilter(3L, forgetting = 0.995, delta = 0.5),
      args = list(algorithm = "rls", n_taps = 3L,
                  forgetting = 0.995, delta = 0.5)
    )
  )
  for (case in cases) {
    expected <- do.call(
      adaptive_oracle,
      c(list(signal = signal, reference = reference), case$args)
    )
    first <- update(case$object, signal[1:113], reference = reference[1:113])
    second <- update(case$object, signal[114:300],
                     reference = reference[114:300])
    expect_equal(
      c(first$output, second$output), expected$residual,
      tolerance = 2e-13
    )
    expect_equal(
      drop(processorState(case$object)$weights), expected$weights,
      tolerance = 2e-13
    )
  }
})

test_that("RLS cancels the governed correlated-noise fixture by over 20 dB", {
  set.seed(72)
  reference <- rnorm(4000)
  noise <- stats::filter(reference, c(0.8, -0.3, 0.15, 0.05),
                         method = "convolution", sides = 1)
  noise[is.na(noise)] <- 0
  signal <- as.numeric(noise) + rnorm(4000, sd = 0.001)
  result <- update(
    rlsFilter(4L, forgetting = 0.999, delta = 1),
    signal, reference = reference
  )
  index <- 501:4000
  reduction_db <- 10 * log10(
    mean(signal[index]^2) / mean(result$output[index, 1L]^2)
  )
  expect_gt(reduction_db, 20)
})

test_that("adaptive channels bind without recycling", {
  processor <- nlmsFilter(2L)
  samples <- cbind(a = 1:5, b = 2:6)
  expect_silent(update(processor, samples, reference = matrix(1:5, 5L)))
  before <- dsp_serialize(processorState(processor))
  expect_error(
    update(processor, samples, reference = matrix(1:15, 5L, 3L)),
    class = "PhysioStream_dsp_channel_error"
  )
  expect_identical(dsp_serialize(processorState(processor)), before)
})

test_that("RLS rejects state allocations beyond the portable ceiling", {
  processor <- rlsFilter(2000L)
  expect_error(
    update(processor, 1, reference = 1),
    class = "PhysioStream_dsp_resource_error"
  )
})
