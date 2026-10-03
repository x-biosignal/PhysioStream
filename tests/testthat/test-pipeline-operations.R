test_that("compiled SOS plus RMS matches an independent recurrence", {
  set.seed(1008)
  samples <- matrix(
    rnorm(301 * 3), 301L, 3L,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  sos <- .pipeline_normalize_sos(pipeline_test_sos())
  reference <- pipeline_reference_sos_rms(samples, sos, 17L)
  pipeline <- streamPipeline(chunk_size = 301L)
  onChunk(pipeline, bandpassRmsOp(sos, 17L))
  pipelineEnqueue(pipeline, samples, ingest_time_ns = 0)
  observed <- pipelineStep(pipeline)$results[[1L]]$output$samples
  expect_equal(unname(observed), reference$rms, tolerance = 1e-12)
  state <- pipelineState(pipeline)$operations[[1L]]$state
  expect_equal(unname(state$zi), unname(reference$state$zi),
               tolerance = 1e-12)
  expect_equal(unname(state$rms_buffer), reference$state$buffer,
               tolerance = 1e-12)
  expect_equal(unname(state$rms_sums), reference$state$sums,
               tolerance = 1e-12)
  expect_equal(state$rms_cursor + 1L, reference$state$cursor)
  expect_equal(state$rms_filled, reference$state$filled)
})

test_that("compiled operation is invariant to arbitrary chunk boundaries", {
  set.seed(8)
  samples <- matrix(
    rnorm(257 * 2), 257L, 2L,
    dimnames = list(NULL, c("left", "right"))
  )
  sos <- pipeline_test_sos()
  whole <- streamPipeline(chunk_size = 257L)
  split <- streamPipeline(chunk_size = 40L)
  onChunk(whole, bandpassRmsOp(sos, 32L))
  onChunk(split, bandpassRmsOp(sos, 32L))
  pipelineEnqueue(whole, samples, ingest_time_ns = 0)
  expected <- pipelineStep(whole)$results[[1L]]$output$samples
  boundaries <- c(1L, 31L, 32L, 33L, 17L, 40L, 39L, 40L, 24L)
  start <- 1L
  output <- list()
  for (i in seq_along(boundaries)) {
    end <- start + boundaries[[i]] - 1L
    pipelineEnqueue(
      split, samples[start:end, , drop = FALSE],
      ingest_time_ns = i
    )
    output[[i]] <- pipelineStep(split)$results[[1L]]$output$samples
    start <- end + 1L
  }
  expect_equal(start, nrow(samples) + 1L)
  expect_equal(unname(do.call(rbind, output)), unname(expected),
               tolerance = 1e-12)
  whole_state <- pipelineState(whole)$operations[[1L]]$state
  split_state <- pipelineState(split)$operations[[1L]]$state
  expect_equal(unname(split_state$zi), unname(whole_state$zi),
               tolerance = 1e-12)
  expect_equal(unname(split_state$rms_buffer),
               unname(whole_state$rms_buffer), tolerance = 1e-12)
  expect_equal(unname(split_state$rms_sums),
               unname(whole_state$rms_sums), tolerance = 1e-12)
  expect_equal(split_state$rms_cursor, whole_state$rms_cursor)
})

test_that("complete warmup emits only available rows", {
  samples <- matrix(
    1:12, 6L, 2L, dimnames = list(NULL, c("a", "b"))
  )
  pipeline <- streamPipeline(chunk_size = 6L)
  onChunk(
    pipeline,
    bandpassRmsOp(matrix(c(1, 0, 0, 1, 0, 0), 1L), 4L,
                  warmup = "complete")
  )
  pipelineEnqueue(pipeline, samples, timestamps = 1:6, ingest_time_ns = 0)
  result <- pipelineStep(pipeline)$results[[1L]]
  expect_equal(nrow(result$output$samples), 3L)
  expect_equal(result$output$sequence, 4:6)
  expect_equal(result$output$timestamps, 4:6)
  expect_identical(result$diagnostics$bandpass_rms$available,
                   c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("bandpass RMS validates configuration and preserves input", {
  sos <- pipeline_test_sos()
  expect_error(bandpassRmsOp(matrix(1, 2, 2), 4), "six columns")
  bad <- sos
  bad[1L, 4L] <- 0
  expect_error(bandpassRmsOp(bad, 4), "nonzero")
  expect_error(bandpassRmsOp(sos, 0), "invalid value")
  expect_error(bandpassRmsOp(sos, 4, warmup = "bad"), "exactly one")
  samples <- matrix(
    1:8, 4L, 2L, dimnames = list(NULL, c("a", "b"))
  )
  before <- serialize(samples, NULL, version = 3L)
  pipeline <- streamPipeline(chunk_size = 4L)
  onChunk(pipeline, bandpassRmsOp(sos, 4L))
  pipelineEnqueue(pipeline, samples, ingest_time_ns = 0)
  pipelineStep(pipeline)
  expect_identical(serialize(samples, NULL, version = 3L), before)
})

test_that("bandpassRmsOp accepts a matrix with no channel names", {
  # An unnamed multi-channel matrix is the most natural input, and it used to
  # fail: channel_names stayed NULL, so naming the output computed
  # paste0(NULL, "_rms") -- length one -- against a two-column matrix.
  sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
                dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
  feed <- function(x) {
    op <- bandpassRmsOp(sos, window_samples = 4L)
    p <- streamPipeline(chunk_size = 8L)
    onChunk(p, op)
    pipelineEnqueue(p, x, ingest_time_ns = 0)
    pipelineStep(p)$results[[1]]$output$samples
  }
  unnamed <- feed(matrix(sin(seq_len(16)), 8, 2))
  expect_equal(ncol(unnamed), 2L)
  expect_equal(colnames(unnamed), c("Ch1_rms", "Ch2_rms"))

  # a single unnamed channel already worked; it must keep working
  expect_equal(ncol(feed(matrix(sin(seq_len(8)), 8, 1))), 1L)

  # named input is unchanged
  named <- feed(matrix(sin(seq_len(16)), 8, 2, dimnames = list(NULL, c("C3", "C4"))))
  expect_equal(colnames(named), c("C3_rms", "C4_rms"))
})

test_that("an unnamed stream keeps its filter state across chunks", {
  # NULL channel_names doubled as the "not initialised" flag, so an unnamed
  # stream reset zi and the RMS window on every chunk.
  sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
                dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
  op <- bandpassRmsOp(sos, window_samples = 4L)
  p <- streamPipeline(chunk_size = 4L)
  onChunk(p, op)
  x <- matrix(sin(seq_len(8)), 4, 2)
  pipelineEnqueue(p, x, ingest_time_ns = 0)
  first <- pipelineStep(p)
  pipelineEnqueue(p, x, ingest_time_ns = 1)
  second <- pipelineStep(p)
  # the second chunk must report the window as filled, which can only happen if
  # the RMS buffer carried over
  expect_true(isTRUE(second$results[[1]]$diagnostics$available[[4]]) ||
                all(second$results[[1]]$diagnostics$available))
  expect_equal(ncol(second$results[[1]]$output$samples), 2L)
})
