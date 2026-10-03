test_that("official empty XDF retains every empty and nonempty stream", {
  skip_without_pyxdf()
  x <- readXDF(xdf_fixture("empty-streams"))
  expect_identical(
    PhysioCore::streamNames(x),
    c("xdf_stream_3", "xdf_stream_4", "xdf_stream_1", "xdf_stream_2")
  )
  expect_identical(
    unname(dim(x)),
    matrix(c(0L, 10L, 1L, 0L, 1L, 1L, 1L, 1L), 4L, 2L)
  )
  expect_identical(
    unname(vapply(as.list(PhysioCore::streams(x)), function(pe) {
      S4Vectors::metadata(pe)$xdf$channel_format
    }, character(1))),
    c("float32", "int32", "string", "string")
  )
  expect_s4_class(PhysioCore::getEvents(x[["xdf_stream_2"]]),
                  "PhysioEvents")
  expect_identical(
    names(SummarizedExperiment::rowData(x[["xdf_stream_2"]])),
    c("xdf_time", "time_from_t0", "xdf_segment", "xdf_time_raw")
  )
  expect_identical(
    nrow(PhysioCore::getEvents(x[["xdf_stream_2"]])@events),
    0L
  )
})

test_that("clock reset processing preserves raw evidence and segments", {
  skip_without_pyxdf()
  x <- readXDF(xdf_fixture("clock-resets"))
  expect_identical(unname(dim(x)),
                   matrix(c(175L, 27815L, 1L, 8L), 2L, 2L))
  first <- x[[1L]]
  second <- x[[2L]]
  expect_length(S4Vectors::metadata(first)$xdf$clock_times, 115L)
  expect_length(S4Vectors::metadata(second)$xdf$clock_values, 115L)
  expect_identical(nrow(S4Vectors::metadata(first)$xdf$segments), 1L)
  expect_identical(nrow(S4Vectors::metadata(second)$xdf$segments), 2L)
  expect_true(
    any(diff(SummarizedExperiment::rowData(second)$xdf_time_raw) < 0)
  )
  segments <- SummarizedExperiment::rowData(second)$xdf_segment
  corrected <- SummarizedExperiment::rowData(second)$xdf_time
  expect_true(all(vapply(split(corrected, segments), function(value) {
    all(diff(value) >= 0)
  }, logical(1))))
  clock <- PhysioCore::commonClock(x)
  expect_equal(
    clock$offsets,
    vapply(as.list(PhysioCore::streams(x)), function(pe) {
      times <- SummarizedExperiment::rowData(pe)$xdf_time
      if (length(times)) times[[1L]] - clock$t0 else 0
    }, numeric(1)),
    tolerance = 1e-12
  )
})
