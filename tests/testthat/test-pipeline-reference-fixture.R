test_that("pipeline reference fixture is intact and independently reproducible", {
  path <- system.file(
    "extdata", "pipeline_reference.rds", package = "PhysioStream"
  )
  manifest <- system.file(
    "extdata", "pipeline_reference.sha256", package = "PhysioStream"
  )
  expect_true(nzchar(path))
  expect_true(nzchar(manifest))
  expected_sha <- strsplit(readLines(manifest, warn = FALSE), "  ",
                           fixed = TRUE)[[1L]][[1L]]
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    expected_sha
  )
  fixture <- readRDS(path)
  expect_identical(fixture$schema, "1.0.0")
  expect_identical(fixture$provenance$license, "MIT")
  expect_equal(sum(fixture$partitions), nrow(fixture$samples))

  pipeline <- streamPipeline(chunk_size = max(fixture$partitions))
  onChunk(
    pipeline,
    bandpassRmsOp(fixture$sos, fixture$configuration$window_samples)
  )
  outputs <- list()
  start <- 1L
  for (i in seq_along(fixture$partitions)) {
    end <- start + fixture$partitions[[i]] - 1L
    pipelineEnqueue(
      pipeline, fixture$samples[start:end, , drop = FALSE],
      ingest_time_ns = i
    )
    outputs[[i]] <- pipelineStep(pipeline)$results[[1L]]$output$samples
    start <- end + 1L
  }
  expect_equal(
    unname(do.call(rbind, outputs)),
    fixture$oracle$output,
    tolerance = 1e-12
  )
  state <- pipelineState(pipeline)$operations[[1L]]$state
  expect_equal(unname(state$zi), fixture$oracle$state$zi,
               tolerance = 1e-12)
  expect_equal(unname(state$rms_buffer),
               fixture$oracle$state$rms_buffer, tolerance = 1e-12)
  expect_equal(unname(state$rms_sums),
               fixture$oracle$state$rms_sums, tolerance = 1e-12)
  expect_equal(state$rms_cursor, fixture$oracle$state$rms_cursor)
  expect_equal(state$rms_filled, fixture$oracle$state$rms_filled)
})
