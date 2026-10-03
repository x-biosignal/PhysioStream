test_that("official minimal XDF maps exact streams and payloads", {
  skip_without_pyxdf()
  x <- readXDF(
    xdf_fixture("minimal"),
    synchronize = FALSE,
    dejitter = FALSE
  )
  expect_s4_class(x, "MultiRatePhysioExperiment")
  expect_identical(
    PhysioCore::streamNames(x),
    c("xdf_stream_0", "xdf_stream_46202862")
  )
  expect_identical(unname(dim(x)), matrix(c(9L, 9L, 3L, 1L), 2L, 2L))

  numeric_values <- rbind(
    c(192, 255, 238), c(12, 22, 32), c(13, 23, 33),
    c(14, 24, 34), c(15, 25, 35), c(12, 22, 32),
    c(13, 23, 33), c(14, 24, 34), c(15, 25, 35)
  )
  expect_equal(
    unname(SummarizedExperiment::assay(x[[1L]], "xdf")),
    numeric_values,
    tolerance = 0
  )
  expect_equal(
    SummarizedExperiment::rowData(x[[1L]])$xdf_time,
    seq(5.1, 5.9, by = 0.1),
    tolerance = 1e-12
  )
  marker <- SummarizedExperiment::assay(x[[2L]], "xdf")[, 1L]
  expect_identical(
    unname(marker[-1L]),
    c("Hello", "World", "from", "LSL", "Hello", "World", "from", "LSL")
  )
  events <- PhysioCore::getEvents(x[[2L]])
  expect_s4_class(events, "PhysioEvents")
  expect_identical(as.character(events@events$value), unname(marker))
  expect_identical(attr(events, "xdf")$stream_id, 46202862)

  metadata <- S4Vectors::metadata(x[[1L]])$xdf
  reference <- readRDS(system.file(
    "extdata", "xdf_reference.rds",
    package = "PhysioStream", mustWork = TRUE
  ))
  expect_identical(metadata$channel_format, "int16")
  expect_identical(metadata$nominal_srate, 10)
  expect_match(metadata$stream_header_xml, "<name>SendDataC</name>",
               fixed = TRUE)
  expect_match(metadata$stream_footer_xml, "<sample_count>9</sample_count>",
               fixed = TRUE)
  expect_match(metadata$stream_header_sha256, "^[0-9a-f]{64}$")
  expect_match(metadata$source_file_sha256, "^[0-9a-f]{64}$")
  expect_identical(
    metadata$stream_header_sha256,
    reference$xml$minimal$streams[["0"]]$header
  )
  expect_identical(
    metadata$stream_footer_sha256,
    reference$xml$minimal$streams[["0"]]$footer
  )
  expect_false(any(vapply(metadata$backend, reticulate::is_py_object,
                          logical(1))))
  expect_silent(serialize(x, NULL, version = 3L))
})

test_that("XDF stream selection is exact and unambiguous", {
  skip_without_pyxdf()
  by_id <- readXDF(xdf_fixture("minimal"), streams = 46202862)
  expect_identical(PhysioCore::streamNames(by_id), "xdf_stream_46202862")
  by_name <- readXDF(xdf_fixture("minimal"), streams = "SendDataC")
  expect_identical(PhysioCore::streamNames(by_name), "xdf_stream_0")
  expect_error(
    readXDF(xdf_fixture("minimal"), streams = "missing"),
    class = "PhysioStream_validation_error"
  )
  expect_error(
    readXDF(xdf_fixture("minimal"), streams = c(0, 0)),
    class = "PhysioStream_validation_error"
  )
})

test_that("XDF input path and enum guards fail before backend work", {
  expect_error(readXDF("https://example.test/a.xdf"),
               class = "PhysioStream_validation_error")
  expect_error(readXDF("missing.xdf"),
               class = "PhysioStream_validation_error")
  expect_error(readXDF(xdf_fixture("minimal"), max_file_bytes = 10),
               class = "PhysioStream_xdf_resource_error")
  expect_error(readXDF(xdf_fixture("minimal"), synchronize = 1),
               class = "PhysioStream_validation_error")
  expect_error(readXDF(xdf_fixture("minimal"), backend = "other"),
               class = "PhysioStream_validation_error")
})
