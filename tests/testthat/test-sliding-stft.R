test_that("sliding STFT emits complete windows and preserves sequences", {
  signal <- sin(2 * pi * 10 * (0:199) / 100)
  processor <- slidingSTFT(
    100, 32L, 11L, 33L, window = "rectangular",
    detrend = "none", output = "complex"
  )
  first <- update(processor, signal[1:20], timestamps = (1:20) / 100)
  expect_identical(first$n_emitted, 0L)
  second <- update(processor, signal[21:200], timestamps = (21:200) / 100)
  expect_equal(Mod(second$output)^2, second$diagnostics$power,
               tolerance = 2e-14)
  expect_true(all(
    second$diagnostics$frame_sequences ==
      seq(32, 197, by = 11)
  ))
  expect_lt(second$diagnostics$retained_samples, 32)
})

test_that("band power clips and interpolates exact endpoints", {
  bands <- matrix(
    c(7.25, 12.75), 1L, 2L,
    dimnames = list("alpha", c("low", "high"))
  )
  signal <- sin(2 * pi * 10 * (0:255) / 100)
  result <- update(
    slidingSTFT(
      100, 256L, 256L, 256L, window = "rectangular",
      detrend = "none", output = "power", bands = bands
    ),
    signal
  )
  expect_gt(result$diagnostics$band_power[1L, "alpha", 1L], 0.45)
  expect_lt(result$diagnostics$band_power[1L, "alpha", 1L], 0.55)
})

test_that("odd FFT bands are clipped to the last represented frequency", {
  bands <- matrix(
    c(32 * 100 / 65, 50), 1L, 2L,
    dimnames = list("edge", c("low", "high"))
  )
  result <- update(
    slidingSTFT(
      100, 64L, 64L, 65L, detrend = "none",
      output = "power", bands = bands
    ),
    rep(1, 64L)
  )
  expect_identical(result$diagnostics$band_power[1L, "edge", 1L], 0)
})

test_that("causal filters are independent and reset with processor state", {
  factory <- function(channel) {
    PhysioPreprocess::StreamFilter(
      sos = matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L),
      warmup = FALSE
    )
  }
  processor <- slidingSTFT(
    100, 16L, 8L, causal_filter = factory, n_features = 2L
  )
  result <- update(processor, matrix(rnorm(80), 40L, 2L))
  expect_identical(result$n_emitted, 4L)
  expect_length(processorState(processor)$runtime_state, 2L)
  processorReset(processor)
  expect_false(processorState(processor)$runtime_state[[1L]]$primed)

  shared <- factory(1L)
  expect_error(
    slidingSTFT(100, 16L, 8L, causal_filter = list(shared, shared)),
    class = "PhysioStream_dsp_validation_error"
  )
})
