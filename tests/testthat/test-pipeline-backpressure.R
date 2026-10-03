test_that("error backpressure leaves a full queue byte-identical", {
  pipeline <- streamPipeline(
    chunk_size = 2L, queue_capacity = 1L, backpressure = "error"
  )
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 1)
  before <- pipeline_state_raw(pipeline)
  expect_error(
    pipelineEnqueue(pipeline, x, ingest_time_ns = 2),
    class = "PhysioStream_pipeline_backpressure"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
})

test_that("drop_oldest retains whole newest sequence ranges", {
  pipeline <- streamPipeline(
    chunk_size = 2L, queue_capacity = 2L, backpressure = "drop_oldest"
  )
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 1)
  pipelineEnqueue(pipeline, x, ingest_time_ns = 2)
  pipelineEnqueue(pipeline, x, ingest_time_ns = 3)
  state <- pipelineState(pipeline)
  expect_equal(state$queue[[1L]]$sequence, 3:4)
  expect_equal(state$queue[[2L]]$sequence, 5:6)
  expect_equal(state$counters$dropped_chunks, 1)
  expect_equal(state$counters$dropped_samples, 2)
  expect_equal(state$drop_audit[[1L]]$sequence_start, 1)
  expect_equal(state$drop_audit[[1L]]$sequence_end, 2)
})

test_that("drop_newest records loss without consuming sequence identity", {
  pipeline <- streamPipeline(
    chunk_size = 2L, queue_capacity = 2L, backpressure = "drop_newest"
  )
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 1)
  pipelineEnqueue(pipeline, x, ingest_time_ns = 2)
  pipelineEnqueue(pipeline, x, ingest_time_ns = 3)
  state <- pipelineState(pipeline)
  expect_equal(state$counters$sequence_last, 4)
  expect_equal(state$counters$accepted_chunks, 2)
  expect_equal(state$counters$dropped_chunks, 1)
  expect_equal(state$counters$dropped_samples, 2)
  expect_length(state$drop_audit[[1L]]$sequence_start, 0L)
  expect_equal(state$last_ingest_time_ns, 3)
})

test_that("drop audit is bounded with an explicit truncation counter", {
  state <- .pipeline_empty_state(list(
    chunk_size = 1L, queue_capacity = 1L,
    backpressure = "drop_newest", latency_budget_ms = 50
  ))
  for (i in seq_len(.pipeline_drop_audit_limit + 3L)) {
    state <- .pipeline_add_drop(
      state,
      list(policy = "drop_newest", n_samples = 1,
           sequence_start = numeric(), sequence_end = numeric(),
           ingest_time_ns = i)
    )
  }
  expect_length(state$drop_audit, .pipeline_drop_audit_limit)
  expect_equal(state$counters$drop_audit_truncated, 3)
  expect_equal(state$drop_audit[[1L]]$ingest_time_ns, 4)
})
