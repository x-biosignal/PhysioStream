test_that("offline LSL metadata fixture is intact and maps independently", {
  path <- system.file(
    "extdata", "lsl_metadata_reference.rds", package = "PhysioStream"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..", "..", "inst", "extdata", "lsl_metadata_reference.rds"
    )
  }
  hash_path <- sub("\\.rds$", ".sha256", path)
  fixture <- readRDS(path)
  recorded <- strsplit(readLines(hash_path, warn = FALSE), " +")[[1L]][[1L]]
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    recorded
  )
  expect_identical(fixture$schema, "physiostream-lsl-reference/1.0.0")
  expect_identical(fixture$upstream$pylsl_version, "1.18.2")
  expect_length(fixture$records, 100L)

  for (case in fixture$records) {
    observed <- PhysioStream:::.lsl_record_to_stream_info(
      PhysioStream:::.lsl_descriptor_record(
        case$xml, fixture$upstream$liblsl_version
      )
    )
    expected <- case$expected
    expect_identical(streamName(observed), expected$name)
    expect_identical(streamType(observed), expected$type)
    expect_identical(streamChannels(observed), expected$channel_names)
    expect_identical(observed@channel_units, expected$channel_units)
    expect_equal(streamRate(observed), expected$nominal_srate, tolerance = 0)
    expect_identical(streamDtype(observed), expected$dtype)
    expect_identical(observed@source_id, expected$source_id)
    expect_identical(observed@metadata$lsl$uid, expected$uid)
    expect_equal(
      observed@metadata$lsl$created_at, expected$created_at,
      tolerance = 0
    )
  }
})
