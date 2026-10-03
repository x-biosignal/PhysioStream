test_that("native scanner rejects malformed structural mutations", {
  path <- xdf_fixture("minimal")
  bytes <- readBin(path, "raw", n = file.info(path)$size)

  bad_magic <- bytes
  bad_magic[[1L]] <- as.raw(0L)
  expect_error(
    PhysioStream:::.xdf_scan_bytes(bad_magic),
    class = "PhysioStream_xdf_parse_error"
  )
  expect_error(
    PhysioStream:::.xdf_scan_bytes(bytes[-length(bytes)]),
    class = "PhysioStream_xdf_parse_error"
  )
  bad_width <- bytes
  bad_width[[5L]] <- as.raw(2L)
  expect_error(
    PhysioStream:::.xdf_scan_bytes(bad_width),
    class = "PhysioStream_xdf_parse_error"
  )
})

test_that("native scanner rejects external entities before XML parsing", {
  xml <- charToRaw("<!DOCTYPE info [<!ENTITY x SYSTEM \"file:///etc/passwd\">]><info>&x;</info>")
  expect_error(
    PhysioStream:::.xdf_xml_record(rawToChar(xml), "mutation"),
    class = "PhysioStream_xdf_parse_error"
  )
})

test_that("int64 headers fail loudly before backend import", {
  bytes <- readBin(
    xdf_fixture("minimal"), "raw", n = file.info(xdf_fixture("minimal"))$size
  )
  pattern <- charToRaw("int16")
  candidates <- which(bytes == pattern[[1L]])
  locations <- candidates[vapply(candidates, function(index) {
    end <- index + length(pattern) - 1L
    end <= length(bytes) && identical(bytes[index:end], pattern)
  }, logical(1))]
  expect_length(locations, 1L)
  location <- locations[[1L]]
  bytes[location:(location + 4L)] <- charToRaw("int64")
  path <- tempfile(fileext = ".xdf")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  expect_error(
    readXDF(path),
    class = "PhysioStream_xdf_unsupported_format"
  )
})
