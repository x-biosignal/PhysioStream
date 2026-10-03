test_that("snapshots preserve acquired identity and governed metadata", {
  source <- streamOpen(loopbackSource(make_stream_info(), 8L))
  values <- matrix(sin(seq_len(10)), nrow = 5, ncol = 2)
  times <- c(10, 10.009, 10.021, 10.030, 10.041)
  loopbackFeed(source, values, times)
  pe <- streamSnapshot(source)

  expect_s4_class(pe, "PhysioExperiment")
  expect_identical(dim(pe), c(5L, 2L))
  expect_equal(
    unname(SummarizedExperiment::assay(pe, "stream")),
    unname(values)
  )
  expect_identical(SummarizedExperiment::rowData(pe)$time_seconds, times)
  expect_identical(
    SummarizedExperiment::rowData(pe)$stream_sequence,
    as.double(0:4)
  )
  expect_identical(PhysioCore::samplingRate(pe), 100)
  expect_identical(
    SummarizedExperiment::colData(pe)$label,
    c("C3", "C4")
  )
  md <- S4Vectors::metadata(pe)$stream
  expect_s4_class(md$info, "StreamInfo")
  expect_identical(nchar(md$source_fingerprint), 64L)
  expect_false(any(grepl(
    "externalptr|/workspace|root@|matsui@|0x[0-9a-f]+",
    capture.output(str(md))
  )))
  expect_identical(PhysioCore::provenance(pe)$agent, "PhysioStream")
  expect_identical(unserialize(serialize(pe, NULL)), pe)
})

test_that("newest and duration snapshot boundaries are exact", {
  source <- streamOpen(loopbackSource(make_stream_info(channels = "x"), 8L))
  values <- matrix(as.double(1:6), ncol = 1)
  times <- c(1, 1.1, 1.2, 1.31, 1.4, 1.5)
  loopbackFeed(source, values, times)

  newest <- streamSnapshot(source, n = 3L)
  expect_identical(
    SummarizedExperiment::rowData(newest)$stream_sequence,
    as.double(3:5)
  )
  duration <- streamSnapshot(source, duration_seconds = 0.19)
  expect_identical(
    SummarizedExperiment::rowData(duration)$time_seconds,
    c(1.31, 1.4, 1.5)
  )
  expect_identical(ringFill(source@buffer), 6L)
})

test_that("snapshot consumption cannot hide skipped unread rows", {
  source <- streamOpen(loopbackSource(make_stream_info(channels = "x"), 8L))
  loopbackFeed(source, matrix(as.double(1:5), ncol = 1), as.double(1:5))
  expect_error(streamSnapshot(source, n = 3L, consume = TRUE), "skip")
  expect_error(
    streamSnapshot(source, duration_seconds = 2, consume = TRUE),
    "skip"
  )
  expect_identical(ringFill(source@buffer), 5L)
  pe <- streamSnapshot(source, consume = TRUE)
  expect_identical(dim(pe), c(5L, 1L))
  expect_identical(ringFill(source@buffer), 0L)
})

test_that("snapshots are independent of later buffer mutation", {
  source <- streamOpen(loopbackSource(make_stream_info(), 3L))
  loopbackFeed(source, make_samples(3), c(1, 2, 3))
  pe <- streamSnapshot(source)
  frozen <- SummarizedExperiment::assay(pe)
  loopbackFeed(source, make_samples(3, offset = 6), c(4, 5, 6))
  expect_identical(SummarizedExperiment::assay(pe), frozen)
  expect_true(S4Vectors::metadata(streamSnapshot(source))$stream$loss_observed)
})

test_that("snapshot preconditions reject ambiguous or invalid requests", {
  source <- streamOpen(loopbackSource(make_stream_info(), 4L))
  expect_error(streamSnapshot(source), "at least two")
  loopbackFeed(source, make_samples(2), c(1, 2))
  expect_error(streamSnapshot(source, n = 2L, duration_seconds = 1),
               "at most one")
  expect_error(streamSnapshot(source, n = 1L), "at least two")

  irregular <- streamOpen(loopbackSource(make_stream_info(rate = 0), 4L))
  loopbackFeed(irregular, make_samples(2), c(1, 2))
  expect_error(streamSnapshot(irregular), "positive nominal")
})
