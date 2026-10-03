test_that("streamPipeline validates exact configuration", {
  pipeline <- streamPipeline()
  expect_s3_class(pipeline, "StreamPipeline")
  expect_equal(pipelineState(pipeline)$configuration$chunk_size, 32L)
  expect_error(streamPipeline(chunk_size = 1.5), "invalid value")
  expect_error(streamPipeline(queue_capacity = 0), "invalid value")
  expect_error(streamPipeline(backpressure = "block"), "exactly one")
  expect_error(streamPipeline(latency_budget_ms = 0), "invalid value")
  expect_error(streamPipeline(source = new.env()), "StreamSource")
  expect_match(capture.output(print(pipeline)), "operations=0")
})

test_that("empty chunks and zero steps are byte-identical no-ops", {
  pipeline <- streamPipeline(chunk_size = 4L)
  before <- pipeline_state_raw(pipeline)
  expect_invisible(pipelineEnqueue(pipeline, numeric()))
  expect_identical(pipeline_state_raw(pipeline), before)
  result <- pipelineStep(pipeline, 0L)
  expect_equal(result$n_processed, 0L)
  expect_identical(pipeline_state_raw(pipeline), before)
  expect_error(
    pipelineEnqueue(pipeline, numeric(), timestamps = 1),
    "empty"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
})

test_that("enqueue binds channels timestamps and exact sequence", {
  pipeline <- streamPipeline(chunk_size = 3L)
  x <- matrix(1:6, 3L, 2L, dimnames = list(NULL, c("a", "b")))
  pipelineEnqueue(pipeline, x, c(1, 2, 3), ingest_time_ns = 10)
  state <- pipelineState(pipeline)
  expect_identical(state$channel_names, c("a", "b"))
  expect_identical(state$timestamp_mode, "provided")
  expect_equal(state$queue[[1L]]$sequence, 1:3)
  before <- pipeline_state_raw(pipeline)
  expect_error(
    pipelineEnqueue(
      pipeline, x[, 2:1, drop = FALSE], c(4, 5, 6),
      ingest_time_ns = 11
    ),
    "channel"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
  expect_error(
    pipelineEnqueue(pipeline, x, NULL, ingest_time_ns = 11),
    "timestamp presence"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
  expect_error(
    pipelineEnqueue(pipeline, x, c(3, 4, 5), ingest_time_ns = 11),
    "increase"
  )
  expect_identical(pipeline_state_raw(pipeline), before)
})

test_that("callbacks execute in order and commit explicit state", {
  pipeline <- streamPipeline(chunk_size = 4L)
  add <- function(chunk, state, context) {
    chunk$samples <- chunk$samples + state$value
    state$count <- state$count + 1
    list(
      output = chunk, state = state, events = list(),
      diagnostics = list(order = state$count)
    )
  }
  multiply <- function(chunk, state, context) {
    chunk$samples <- chunk$samples * state$value
    state$count <- state$count + 1
    list(
      output = chunk, state = state,
      events = list(list(timestamp = chunk$sequence[[1L]],
                         type = "processed", value = "yes")),
      diagnostics = list(order = state$count)
    )
  }
  onChunk(
    pipeline, add, state = list(value = 1, count = 0),
    name = "add", kind = "filter"
  )
  onChunk(
    pipeline, multiply, state = list(value = 2, count = 0),
    name = "multiply", kind = "feature"
  )
  x <- matrix(1:4, 4L, 1L, dimnames = list(NULL, "signal"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
  result <- pipelineStep(pipeline)
  expect_equal(result$results[[1L]]$output$samples[, 1L], (1:4 + 1) * 2)
  expect_identical(names(result$results[[1L]]$diagnostics),
                   c("add", "multiply"))
  expect_length(result$results[[1L]]$events, 1L)
  state <- pipelineState(pipeline)
  expect_equal(vapply(state$operations, function(x) x$state$count, numeric(1)),
               c(1, 1))
  expect_equal(state$counters$processed_samples, 4)
  expect_equal(state$counters$emitted_samples, 4)
})

test_that("callback failure rolls back queue and prior candidate state", {
  pipeline <- streamPipeline(chunk_size = 2L)
  onChunk(
    pipeline, pipeline_identity_callback,
    state = list(count = 0), name = "first"
  )
  onChunk(
    pipeline,
    function(chunk, state, context) stop("injected failure"),
    state = list(count = 0), name = "fail"
  )
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
  before <- pipeline_state_raw(pipeline)
  expect_error(pipelineStep(pipeline), "injected failure")
  expect_identical(pipeline_state_raw(pipeline), before)
  expect_equal(pipelineState(pipeline)$operations[[1L]]$state$count, 0)
  expect_length(pipelineState(pipeline)$queue, 1L)
})

test_that("callback reentry and direct runtime mutation are rolled back", {
  x <- matrix(1, 1L, 1L, dimnames = list(NULL, "x"))
  reentrant <- streamPipeline(chunk_size = 1L)
  callback <- function(chunk, state, context) {
    pipelineEnqueue(reentrant, x, ingest_time_ns = 2)
    list(output = chunk, state = state, events = list(),
         diagnostics = list())
  }
  onChunk(reentrant, callback, state = list(), name = "reentrant")
  pipelineEnqueue(reentrant, x, ingest_time_ns = 1)
  before <- pipeline_state_raw(reentrant)
  expect_error(pipelineStep(reentrant), "not reentrant")
  expect_identical(pipeline_state_raw(reentrant), before)

  direct <- streamPipeline(chunk_size = 1L)
  callback <- function(chunk, state, context) {
    direct$state$counters$processed_chunks <- 999
    list(output = chunk, state = state, events = list(),
         diagnostics = list())
  }
  onChunk(direct, callback, state = list(), name = "direct")
  pipelineEnqueue(direct, x, ingest_time_ns = 1)
  before <- pipeline_state_raw(direct)
  expect_error(pipelineStep(direct), "changed during")
  expect_identical(pipeline_state_raw(direct), before)
  expect_false(direct$busy)
})

test_that("malformed callback results and graph mutation are rejected", {
  pipeline <- streamPipeline(chunk_size = 2L)
  expect_error(
    onChunk(
      pipeline, pipeline_identity_callback, state = new.env(),
      name = "bad"
    ),
    "plain list"
  )
  expect_error(
    onChunk(
      pipeline, pipeline_identity_callback,
      state = list(table = data.frame(x = 1)), name = "object"
    ),
    "non-plain object"
  )
  onChunk(
    pipeline,
    function(chunk, state, context) {
      state$runtime <- new.env()
      list(output = chunk, state = state, events = list(),
           diagnostics = list())
    },
    state = list(), name = "runtime"
  )
  x <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
  before <- pipeline_state_raw(pipeline)
  expect_error(pipelineStep(pipeline), "unsupported runtime")
  expect_identical(pipeline_state_raw(pipeline), before)
  expect_error(
    onChunk(
      pipeline, pipeline_identity_callback,
      state = list(count = 0), name = "late"
    ),
    "queued"
  )
})

test_that("callbacks cannot rewrite timestamp identity or emit early events", {
  samples <- matrix(1:2, 2L, 1L, dimnames = list(NULL, "x"))
  changed <- streamPipeline(chunk_size = 2L)
  onChunk(
    changed,
    function(chunk, state, context) {
      chunk$timestamps <- chunk$timestamps + 0.1
      list(output = chunk, state = state, events = list(),
           diagnostics = list())
    },
    state = list(), name = "changed"
  )
  pipelineEnqueue(changed, samples, c(1, 2), ingest_time_ns = 0)
  before <- pipeline_state_raw(changed)
  expect_error(pipelineStep(changed), "timestamp identity")
  expect_identical(pipeline_state_raw(changed), before)

  early <- streamPipeline(chunk_size = 2L)
  onChunk(
    early,
    function(chunk, state, context) {
      list(
        output = NULL, state = state,
        events = list(list(timestamp = 0.5, type = "early", value = "x")),
        diagnostics = list()
      )
    },
    state = list(), name = "early", kind = "detector"
  )
  pipelineEnqueue(early, samples, c(1, 2), ingest_time_ns = 0)
  before <- pipeline_state_raw(early)
  expect_error(pipelineStep(early), "before its input")
  expect_identical(pipeline_state_raw(early), before)
})

test_that("pipelineReset restores operation initial states", {
  pipeline <- streamPipeline(chunk_size = 1L)
  onChunk(
    pipeline, pipeline_identity_callback,
    state = list(count = 0), name = "counter"
  )
  pipelineEnqueue(
    pipeline, matrix(1, 1L, 1L, dimnames = list(NULL, "x")),
    ingest_time_ns = 0
  )
  pipelineStep(pipeline)
  expect_equal(pipelineState(pipeline)$operations[[1L]]$state$count, 1)
  expect_invisible(pipelineReset(pipeline))
  state <- pipelineState(pipeline)
  expect_equal(state$operations[[1L]]$state$count, 0)
  expect_equal(state$reset_count, 1)
  expect_equal(state$counters$processed_chunks, 0)
  pipelineReset(pipeline, keep_operations = FALSE)
  expect_length(pipelineState(pipeline)$operations, 0L)
})
