test_that("latency phases are monotonic and type-7 summaries are exact", {
  pipeline <- streamPipeline(chunk_size = 2L)
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
  result <- pipelineStep(pipeline)$results[[1L]]$latency
  expect_lte(result$ingest_ns, result$process_start_ns)
  expect_lte(result$process_start_ns, result$process_end_ns)
  expect_lte(result$process_end_ns, result$emit_ns)
  expect_gte(result$queue_wait_ms, 0)
  expect_gte(result$processing_ms, 0)
  expect_gte(result$end_to_end_ms, result$queue_wait_ms)

  latency <- .pipeline_empty_latency()
  latency$end_to_end_ms <- c(1, 2, 3, 4)
  summary <- .pipeline_latency_summary(
    latency, 2.5, .pipeline_empty_counters()
  )
  expect_equal(summary$p50_ms, 2.5)
  expect_equal(summary$p95_ms, 3.85)
  expect_equal(summary$p99_ms, 3.97)
  expect_equal(summary$exceeded, 2)
})

test_that("decreasing explicit monotonic stamps fail before mutation", {
  pipeline <- streamPipeline(chunk_size = 1L)
  x <- matrix(1, 1L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 10)
  pipelineStep(pipeline)
  before <- pipeline_state_raw(pipeline)
  expect_error(
    pipelineEnqueue(pipeline, x, ingest_time_ns = 9),
    "decreased"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
})

test_that("measureLatency preserves RNG and reports exact cardinality", {
  set.seed(42)
  before <- .Random.seed
  benchmark <- measureLatency(
    n_channels = 4L, sampling_rate = 64, hop_samples = 8L,
    duration_s = 1, warmup_chunks = 2L, seed = 10L
  )
  expect_identical(.Random.seed, before)
  expect_equal(benchmark$summary$count, 8L)
  expect_equal(benchmark$configuration$samples_per_channel, 64L)
  expect_equal(benchmark$configuration$n_chunks, 8L)
  expect_equal(benchmark$summary$dropped_chunks, 0)
  expect_equal(benchmark$summary$dropped_samples, 0)
  expect_equal(nrow(benchmark$latencies), 8L)
  expect_true(all(benchmark$latencies$end_to_end_ms >= 0))
  expect_lt(benchmark$summary$p95_ms, 1000)
  low_rate <- measureLatency(
    n_channels = 2L, sampling_rate = 20, hop_samples = 5L,
    duration_s = 1, warmup_chunks = 0L
  )
  expect_equal(low_rate$summary$count, 4L)
  expect_error(
    measureLatency(
      pipeline = streamPipeline(chunk_size = 8L),
      n_channels = 2L, sampling_rate = 64, hop_samples = 8L,
      duration_s = 1, warmup_chunks = 0L
    ),
    "at least one operation"
  )
})

test_that("pipelineRun drains an open LoopbackSource without busy spin", {
  info <- streamInfo(
    "run", "signal", c("a", "b"), nominal_srate = 100,
    dtype = "float64"
  )
  source <- loopbackSource(info, capacity = 16L)
  source <- streamOpen(source)
  samples <- matrix(
    as.numeric(1:16), 8L, 2L, dimnames = list(NULL, c("a", "b"))
  )
  loopbackFeed(source, samples, seq_len(8) / 100)
  pipeline <- streamPipeline(source = source, chunk_size = 4L)
  expect_identical(
    pipelineState(pipeline)$configuration$source$channel_names,
    c("a", "b")
  )
  expect_error(pipelineRun(pipeline, max_chunks = -Inf), "non-negative")
  result <- pipelineRun(pipeline, max_chunks = 10L)
  expect_equal(result$n_processed, 2L)
  expect_identical(result$stop_reason, "empty_source")
  expect_equal(pipelineState(pipeline)$counters$processed_samples, 8)
  expect_equal(
    unlist(lapply(result$results, function(x) x$output$sequence)),
    1:8
  )
  source <- streamClose(source)
  expect_identical(streamState(source), "closed")
})
