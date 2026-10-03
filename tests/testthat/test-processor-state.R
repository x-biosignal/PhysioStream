test_that("processor state is sealed, copied, resettable, and transactional", {
  processor <- lmsFilter(3L, 0.05)
  expect_s3_class(processor, "LMSFilter")
  expect_true(inherits(processor, "StreamProcessor"))

  snapshot <- processorState(processor)
  expect_type(snapshot, "list")
  expect_identical(snapshot$schema, "1.0.0")
  expect_match(snapshot$sha256, "^[0-9a-f]{64}$")
  snapshot$n_samples <- 99
  expect_identical(processorState(processor)$n_samples, 0)

  update(processor, matrix(1:8, 4L, 2L),
         reference = matrix(1:4, 4L, 1L))
  before <- dsp_serialize(processorState(processor))
  expect_error(
    update(processor, matrix(c(1, NA_real_), 1L),
           reference = matrix(1, 1L)),
    class = "PhysioStream_dsp_validation_error"
  )
  expect_identical(dsp_serialize(processorState(processor)), before)

  processorReset(processor)
  reset <- processorState(processor)
  expect_identical(reset$n_samples, 0)
  expect_identical(reset$reset_count, 1)
  expect_identical(reset$channel_names, c("channel_1", "channel_2"))
  processorReset(processor, keep_channels = FALSE)
  expect_null(processorState(processor)$channel_names)
})

test_that("state tampering and runtime injection are rejected", {
  processor <- incrementalPCA(1L, 2L)
  processor$state$n_samples <- 1
  expect_error(
    processorState(processor),
    class = "PhysioStream_dsp_state_error"
  )

  processor <- incrementalPCA(1L, 2L)
  processor$state$payload <- new.env()
  expect_error(
    processorState(processor),
    class = "PhysioStream_dsp_state_error"
  )
})

test_that("empty chunks are no-ops", {
  processor <- rlsFilter(2L)
  before <- dsp_serialize(processorState(processor))
  result <- update(
    processor, matrix(numeric(), 0L, 1L),
    reference = matrix(numeric(), 0L, 1L)
  )
  expect_identical(result$n_input, 0L)
  expect_identical(result$n_emitted, 0L)
  expect_identical(dsp_serialize(processorState(processor)), before)
})
