test_that("online ICA converges up to permutation, sign, and scale", {
  set.seed(75)
  n <- 12000L
  sources <- cbind(rexp(n) - 1, stats::rt(n, 5), stats::rlogis(n))
  sources <- scale(sources)
  mixing <- matrix(c(
    1, 0.5, 0.2,
    0.3, 1, 0.4,
    0.2, 0.4, 1
  ), 3L, 3L, byrow = TRUE)
  samples <- sources %*% t(mixing)
  seed_before <- .Random.seed
  result <- update(
    onlineICA(
      3L, learning_rate = 0.075, forgetting = 0.9999,
      block_size = 10L
    ),
    samples
  )
  expect_identical(.Random.seed, seed_before)
  expect_gt(dsp_best_correlation(sources, result$output), 0.97)
  expect_lt(result$diagnostics$decorrelation_residual, 1e-10)
  expect_true(all(is.finite(result$diagnostics$unmixing)))
})

test_that("online ICA rejects feature drift transactionally", {
  processor <- onlineICA(2L)
  update(processor, matrix(rnorm(200), 100L, 2L))
  before <- dsp_serialize(processorState(processor))
  expect_error(
    update(processor, matrix(rnorm(300), 100L, 3L)),
    class = "PhysioStream_dsp_channel_error"
  )
  expect_identical(dsp_serialize(processorState(processor)), before)
})

test_that("ICA complete-block state is invariant to chunk boundaries", {
  set.seed(751)
  samples <- matrix(rnorm(303), 101L, 3L)
  timestamps <- seq_len(101L) / 100
  whole <- onlineICA(
    3L, learning_rate = 0.01, forgetting = 0.999,
    block_size = 10L, seed = 4L
  )
  split <- onlineICA(
    3L, learning_rate = 0.01, forgetting = 0.999,
    block_size = 10L, seed = 4L
  )
  whole_result <- update(whole, samples, timestamps = timestamps)
  first <- update(
    split, samples[1:7, , drop = FALSE],
    timestamps = timestamps[1:7]
  )
  second <- update(
    split, samples[8:63, , drop = FALSE],
    timestamps = timestamps[8:63]
  )
  third <- update(
    split, samples[64:101, , drop = FALSE],
    timestamps = timestamps[64:101]
  )
  expect_identical(first$n_emitted, 0L)
  expect_identical(second$sequence_start, 1)
  expect_identical(second$timestamps[[1L]], timestamps[[1L]])
  expect_identical(
    first$n_emitted + second$n_emitted + third$n_emitted,
    whole_result$n_emitted
  )
  whole_state <- processorState(whole)
  split_state <- processorState(split)
  for (field in c(
    "effective_n", "mean", "covariance", "whitening", "unmixing",
    "source_m2", "source_m4", "source_effective", "source_sign",
    "block_count", "pending", "pending_sequences", "pending_timestamps"
  )) {
    expect_equal(split_state[[field]], whole_state[[field]], tolerance = 0)
  }
})
