test_that("native XDF writer round-trips every governed dtype", {
  skip_without_pyxdf()
  cases <- list(
    float32 = matrix(as.numeric(c(0, 0.5, -1, 2)), 2L, 2L),
    double64 = matrix(c(pi, -pi, 1e-100, 1e100), 2L, 2L),
    int32 = matrix(c(-2147483648, 2147483647, -1, 0), 2L, 2L),
    int16 = matrix(c(-32768, 32767, -10, 10), 2L, 2L),
    int8 = matrix(c(-128, 127, -2, 2), 2L, 2L),
    string = matrix(c("alpha", "β", "<xml>&", "line\nbreak"), 2L, 2L)
  )
  for (format in names(cases)) {
    values <- cases[[format]]
    colnames(values) <- c("left", "right")
    pe <- xdf_test_pe(values, format, name = paste0("case-", format))
    path <- tempfile(fileext = ".xdf")
    on.exit(unlink(path), add = TRUE)
    expect_identical(writeXDF(pe, path), normalizePath(path))
    scan <- PhysioStream:::.xdf_scan_file(path)
    expect_identical(scan$streams[[1L]]$sample_count, 2)
    actual <- readXDF(path, synchronize = FALSE, dejitter = FALSE)
    observed <- unname(SummarizedExperiment::assay(actual[[1L]], "xdf"))
    expect_equal(observed, unname(values), tolerance = 0)
  }
})

test_that("native writer preserves multi-channel string markers as JSON events", {
  skip_without_pyxdf()
  values <- matrix(c("a", "b", "c", "d"), 2L, 2L, byrow = TRUE)
  colnames(values) <- c("code", "detail")
  pe <- xdf_test_pe(values, "string", rate = 0, timestamps = c(2, 2.5),
                    type = "marker")
  path <- tempfile(fileext = ".xdf")
  on.exit(unlink(path), add = TRUE)
  writeXDF(pe, path)
  actual <- readXDF(path, synchronize = FALSE, dejitter = FALSE)
  events <- PhysioCore::getEvents(actual[[1L]])
  expect_identical(
    as.character(events@events$value),
    c('{"code":"a","detail":"b"}', '{"code":"c","detail":"d"}')
  )
})

test_that("writer preserves private descriptor XML and supports explicit overwrite", {
  skip_without_pyxdf()
  pe <- xdf_test_pe(matrix(1:4, 2L, 2L), "int16")
  metadata <- S4Vectors::metadata(pe)
  metadata$xdf$stream_header_xml <- paste0(
    "<info><name>old</name><type>old</type><channel_count>2</channel_count>",
    "<nominal_srate>10</nominal_srate><channel_format>int16</channel_format>",
    "<desc><private><vendor>acme</vendor></private></desc></info>"
  )
  S4Vectors::metadata(pe) <- metadata
  path <- tempfile(fileext = ".xdf")
  on.exit(unlink(path), add = TRUE)
  writeXDF(pe, path)
  first_hash <- digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  )
  scan <- PhysioStream:::.xdf_scan_file(path)
  header <- xml2::read_xml(scan$streams[[1L]]$header_xml)
  expect_identical(
    xml2::xml_text(xml2::xml_find_first(
      header, "/info/desc/private/vendor"
    )),
    "acme"
  )

  SummarizedExperiment::assay(pe, "xdf") <-
    SummarizedExperiment::assay(pe, "xdf") + 1
  expect_silent(writeXDF(pe, path, overwrite = TRUE))
  expect_false(identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    first_hash
  ))
  actual <- readXDF(path, synchronize = FALSE, dejitter = FALSE)
  expect_equal(
    unname(SummarizedExperiment::assay(actual[[1L]], "xdf")),
    unname(SummarizedExperiment::assay(pe, "xdf")),
    tolerance = 0
  )
})

test_that("writer preserves chunk order across raw clock resets", {
  skip_without_pyxdf()
  timestamps <- c(10, 11, 1, 2)
  pe <- xdf_test_pe(
    matrix(1:4, ncol = 1L, dimnames = list(NULL, "value")),
    "int16",
    timestamps = timestamps,
    rate = 0
  )
  path <- tempfile(fileext = ".xdf")
  on.exit(unlink(path), add = TRUE)
  writeXDF(pe, path, chunk_samples = 2L)
  actual <- readXDF(
    path,
    synchronize = FALSE,
    dejitter = FALSE,
    keep_raw_timestamps = FALSE
  )
  expect_identical(
    as.numeric(SummarizedExperiment::rowData(actual[[1L]])$xdf_time),
    timestamps
  )
  expect_identical(
    as.numeric(SummarizedExperiment::assay(actual[[1L]], "xdf")[, 1L]),
    as.numeric(1:4)
  )
})

test_that("writer rejects ambiguous storage and preserves destinations", {
  values <- matrix(c(0.1, 0.2), ncol = 1L,
                   dimnames = list(NULL, "x"))
  pe <- xdf_test_pe(values, "float32")
  path <- tempfile(fileext = ".xdf")
  writeBin(charToRaw("original"), path)
  before <- readBin(path, "raw", n = file.info(path)$size)
  expect_error(writeXDF(pe, path),
               class = "PhysioStream_xdf_path_error")
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  expect_error(writeXDF(pe, path, overwrite = TRUE),
               class = "PhysioStream_xdf_validation_error")
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  unlink(path)

  metadata <- S4Vectors::metadata(pe)
  metadata$xdf$channel_format <- NULL
  S4Vectors::metadata(pe) <- metadata
  expect_error(writeXDF(pe, tempfile(fileext = ".xdf")),
               class = "PhysioStream_xdf_unsupported_format")
})

test_that("writer validates path, timestamp, and chunk arguments exactly", {
  pe <- xdf_test_pe(matrix(1:4, 2L, 2L), "int16")
  expect_error(writeXDF(pe, tempfile(fileext = ".bin")),
               class = "PhysioStream_validation_error")
  expect_error(writeXDF(pe, tempfile(fileext = ".xdf"), chunk_samples = 0),
               class = "PhysioStream_validation_error")
  expect_error(writeXDF(pe, tempfile(fileext = ".xdf"), timestamps = "out"),
               class = "PhysioStream_validation_error")
  expect_error(writeXDF(pe, tempfile(fileext = ".xdf"), timestamps = "raw"),
               class = "PhysioStream_xdf_validation_error")
})

test_that("injected partial writes never alter an existing destination", {
  pe <- xdf_test_pe(matrix(1:4, 2L, 2L), "int16")
  path <- tempfile(fileext = ".xdf")
  writeBin(charToRaw("original-destination"), path)
  before <- readBin(path, "raw", n = file.info(path)$size)
  testthat::local_mocked_bindings(
    .xdf_write_raw_file = function(path, bytes) {
      writeBin(bytes[seq_len(min(16L, length(bytes)))], path)
      stop("injected write failure", call. = FALSE)
    },
    .package = "PhysioStream"
  )
  expect_error(
    writeXDF(pe, path, overwrite = TRUE),
    "injected write failure",
    fixed = TRUE
  )
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  unlink(path)
})
