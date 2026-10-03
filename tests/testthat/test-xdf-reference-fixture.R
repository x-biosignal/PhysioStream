test_that("XDF reference fixture and upstream files are hash-bound", {
  extdata <- system.file("extdata", package = "PhysioStream", mustWork = TRUE)
  manifest <- read.dcf(file.path(extdata, "xdf_upstream.dcf"))
  files <- c(
    Minimal = "xdf-minimal.xdf",
    `Clock-Resets` = "xdf-clock-resets.xdf",
    `Empty-Streams` = "xdf-empty-streams.xdf"
  )
  for (name in names(files)) {
    expected <- manifest[[1L, paste0(name, "-SHA256")]]
    actual <- digest::digest(
      file = file.path(extdata, files[[name]]),
      algo = "sha256", serialize = FALSE
    )
    expect_identical(actual, expected)
  }
  reference_path <- file.path(extdata, "xdf_reference.rds")
  sha_line <- readLines(
    file.path(extdata, "xdf_reference.sha256"), warn = FALSE
  )
  expected_reference_sha <- strsplit(sha_line, " ", fixed = TRUE)[[1L]][[1L]]
  expect_identical(
    digest::digest(
      file = reference_path, algo = "sha256", serialize = FALSE
    ),
    expected_reference_sha
  )
  reference <- readRDS(reference_path)
  expect_identical(reference$schema, "1.0.0")
  expect_identical(reference$minimal$sample_count, c(9L, 9L))
  expect_identical(reference$clock_resets$sample_count, c(175L, 27815L))
  expect_identical(reference$empty_streams$sample_count,
                   c(0L, 10L, 1L, 0L))
})
