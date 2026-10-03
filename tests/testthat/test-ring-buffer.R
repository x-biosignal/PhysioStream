test_that("ring buffer preserves sample-by-channel orientation and wraps", {
  b <- ringBuffer(make_stream_info(), 3L)
  ringPush(b, make_samples(2), c(0.01, 0.02))
  expect_identical(ringFill(b), 2L)
  ringPush(b, make_samples(2, offset = 4), c(0.03, 0.04))
  out <- ringPeek(b)
  expect_identical(out$count, 3L)
  expect_identical(out$sequence, as.double(1:3))
  expected <- rbind(make_samples(2), make_samples(2, offset = 4))
  expect_equal(out$samples, expected[2:4, , drop = FALSE])
  expect_identical(dim(out$samples), c(3L, 2L))
  expect_identical(ringStats(b)$total_dropped, 1)
  expect_true(ringStats(b)$loss_observed)
})

test_that("oversized pushes retain newest rows and count unread loss", {
  b <- ringBuffer(make_stream_info(channels = "x"), 2L)
  values <- matrix(as.double(1:5), ncol = 1)
  ringPush(b, values, as.double(1:5))
  expect_identical(ringPeek(b)$samples[, 1], as.double(4:5))
  expect_identical(ringPeek(b)$sequence, as.double(3:4))
  expect_identical(ringStats(b)$total_dropped, 3)

  pulled <- ringPull(b, 1L)
  expect_identical(pulled$sequence, 3)
  ringPush(b, matrix(6, 1, 1), 6)
  expect_identical(ringStats(b)$total_dropped, 3)
})

test_that("pull, peek, and zero-row results have stable shapes", {
  b <- ringBuffer(make_stream_info(), 4L)
  empty <- ringPull(b, 0L)
  expect_identical(dim(empty$samples), c(0L, 2L))
  expect_type(empty$timestamps, "double")
  expect_type(empty$sequence, "double")
  expect_identical(empty$count, 0L)

  ringPush(b, make_samples(3), c(1, 2, 3))
  latest <- ringPeek(b, 1L, "latest")
  expect_identical(dim(latest$samples), c(1L, 2L))
  expect_identical(latest$sequence, 2)
  expect_identical(ringFill(b), 3L)
  expect_identical(ringPull(b, 99L)$sequence, as.double(0:2))
  expect_identical(ringFill(b), 0L)
  expect_error(ringPeek(b, from = "old"), "exactly")
})

test_that("invalid pushes are transactional", {
  b <- ringBuffer(make_stream_info(), 3L)
  ringPush(b, make_samples(2), c(1, 2))
  before <- ringStats(b)
  values_before <- ringPeek(b)
  expect_error(ringPush(b, make_samples(1), 2), "strictly greater|strictly")
  expect_identical(ringStats(b), before)
  expect_identical(ringPeek(b)$samples, values_before$samples)
  expect_error(ringPush(b, matrix(c(1, Inf), 1, 2), 3), "finite")
  expect_identical(ringStats(b), before)
  expect_error(ringPush(b, as.double(1:2), 3), "matrix")
  expect_error(ringPush(b, matrix(as.double(1:3), 1, 3), 3), "channel")
})

test_that("integer dtype boundaries and float32 rounding are exact", {
  int8 <- ringBuffer(make_stream_info("int8", "x"), 4L)
  ringPush(int8, matrix(c(-128, 0, 127), ncol = 1), c(1, 2, 3))
  expect_identical(ringPeek(int8)$samples[, 1], c(-128, 0, 127))
  expect_error(ringPush(int8, matrix(1.5, 1, 1), 4), "integer dtype")
  expect_error(ringPush(int8, matrix(128, 1, 1), 4), "integer dtype")

  f32 <- ringBuffer(make_stream_info("float32", "x"), 4L)
  values <- c(pi, 1 / 3, 1e-30)
  ringPush(f32, matrix(values, ncol = 1), c(1, 2, 3))
  expect_identical(ringPeek(f32)$samples[, 1], float32_roundtrip(values))
  expect_identical(ringStats(f32)$storage_precision_bits, 24L)
  expect_true(ringStats(f32)$lock_free)
  expect_error(ringPush(f32, matrix(1e300, 1, 1), 4), "float32")
})

test_that("reset preserves lifetime counts and sequence identity", {
  b <- ringBuffer(make_stream_info(channels = "x"), 3L)
  ringPush(b, matrix(as.double(1:3), ncol = 1), c(1, 2, 3))
  ringPull(b, 1L)
  ringReset(b)
  stats <- ringStats(b)
  expect_identical(stats$fill, 0L)
  expect_identical(stats$total_pushed, 3)
  expect_identical(stats$total_pulled, 1)
  expect_identical(stats$reset_count, 1)
  ringPush(b, matrix(4, 1, 1), 0)
  expect_identical(ringPeek(b)$sequence, 3)
})

test_that("wrappers alias live storage and serialized pointers fail safely", {
  b <- ringBuffer(make_stream_info(channels = "x"), 3L)
  alias <- b
  ringPush(alias, matrix(1, 1, 1), 1)
  expect_identical(ringFill(b), 1L)

  path <- tempfile(fileext = ".rds")
  saveRDS(b, path)
  restored <- readRDS(path)
  expect_error(ringStats(restored), "null|restored")

  live <- ringBuffer(make_stream_info(channels = "x"), 1L)
  PhysioStream:::.ring_finalize(live)
  expect_error(ringStats(live), "null|finalized")
  expect_silent(PhysioStream:::cpp_ring_finalize(live@ptr))
})

test_that("native SPSC stress is ordered and lossless", {
  for (i in seq_len(10L)) {
    result <- PhysioStream:::.ring_native_stress(10000L, 17L)
    expect_true(result$ordered)
    expect_identical(result$produced, 10000)
    expect_identical(result$consumed, 10000)
    expect_identical(result$checksum, result$expected_checksum)
    expect_identical(result$dropped, 0)
    expect_false(result$multiple_producers_supported)
  }
})
