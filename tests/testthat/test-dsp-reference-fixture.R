test_that("WS10-07 reference fixture is intact and independently structured", {
  extdata <- system.file("extdata", package = "PhysioStream", mustWork = TRUE)
  path <- file.path(extdata, "dsp_reference.rds")
  expected <- strsplit(
    readLines(file.path(extdata, "dsp_reference.sha256"), warn = FALSE),
    "  ", fixed = TRUE
  )[[1L]][[1L]]
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    expected
  )
  fixture <- readRDS(path)
  expect_identical(fixture$schema, "1.0.0")
  # stats::cov() is BLAS/LAPACK-backed, so recomputing it drifts by ~1 ULP across
  # platforms (the fixture was pinned on one host). Assert the covariance/samples
  # self-consistency numerically rather than bit-exactly (tolerance = 0), so the
  # check is portable across the r-universe build matrix (macOS/Windows differ).
  expect_equal(
    fixture$pca$covariance,
    stats::cov(fixture$pca$samples),
    tolerance = 1e-8
  )
  expect_true(all(c(255L, 256L, 257L) %in%
                    fixture$spectral$fft_lengths))
  expect_true(any(unlist(fixture$partitions) == 0L))
})
