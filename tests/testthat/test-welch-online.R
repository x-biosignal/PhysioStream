test_that("online Welch uses governed one-sided density scaling", {
  sampling_rate <- 128
  n <- 64L
  signal <- sin(2 * pi * 16 * (0:(n - 1L)) / sampling_rate)
  for (n_fft in c(64L, 65L)) {
    result <- update(
      welchOnline(
        sampling_rate, n, n, n_fft, alpha = 1,
        window = "hamming", detrend = "none"
      ),
      signal
    )
    expect_equal(
      unname(drop(result$output[1L, , 1L])),
      dsp_periodogram(signal, sampling_rate, n_fft, "hamming"),
      tolerance = 2e-14
    )
  }
})

test_that("online Welch is exactly continuous across arbitrary chunks", {
  set.seed(76)
  signal <- rnorm(777)
  whole <- welchOnline(250, 101L, 37L, 128L, alpha = 0.17)
  split <- welchOnline(250, 101L, 37L, 128L, alpha = 0.17)
  expected <- update(whole, signal)
  outputs <- list()
  cursor <- 1L
  for (size in c(1L, 19L, 203L, 7L, 300L, 247L)) {
    current <- seq.int(cursor, cursor + size - 1L)
    part <- update(split, signal[current])
    if (part$n_emitted) {
      outputs[[length(outputs) + 1L]] <- part$output
    }
    cursor <- cursor + size
  }
  observed <- array(
    0, dim = dim(expected$output), dimnames = dimnames(expected$output)
  )
  frame <- 1L
  for (part in outputs) {
    target <- seq.int(frame, frame + dim(part)[[1L]] - 1L)
    observed[target, , ] <- part
    frame <- frame + dim(part)[[1L]]
  }
  expect_equal(observed, expected$output, tolerance = 0)
  expect_equal(
    processorState(split)$current_psd,
    processorState(whole)$current_psd,
    tolerance = 0
  )
})

test_that("a one-sample Welch window uses a one-sample default hop", {
  result <- update(
    welchOnline(100, 1L, n_fft = 1L, detrend = "none"),
    c(2, 3)
  )
  expect_identical(result$n_emitted, 2L)
  expect_equal(drop(result$output[, 1L, 1L]), c(0.04, 0.045),
               tolerance = 1e-15)
})

test_that("zero-energy window definitions are rejected before update", {
  expect_error(
    welchOnline(100, 2L, window = "hann"),
    class = "PhysioStream_dsp_validation_error"
  )
})
