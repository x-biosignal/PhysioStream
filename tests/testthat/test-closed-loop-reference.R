test_that("closed-loop reference asset matches its SHA-256 manifest", {
  path <- system.file(
    "extdata", "closed_loop_reference.rds", package = "PhysioStream"
  )
  manifest <- system.file(
    "extdata", "closed_loop_reference.sha256", package = "PhysioStream"
  )
  expect_true(nzchar(path))
  expect_true(nzchar(manifest))
  expected <- strsplit(
    readLines(manifest, warn = FALSE)[[1L]], "[[:space:]]+"
  )[[1L]][[1L]]
  observed <- digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  )
  expect_identical(observed, expected)
  reference <- readRDS(path)
  expect_identical(
    reference$schema,
    "physiostream.closed-loop-reference/1.0.0"
  )
  expect_length(reference$emg, 100L)
  expect_length(reference$erd, 100L)
  expect_length(reference$phase, 100L)
  expect_true(all(vapply(
    c(reference$emg, reference$erd, reference$phase),
    function(x) {
      sum(x$partitions) == length(x$values_scaled) &&
        is.integer(x$values_scaled) &&
        identical(x$value_scale, 1e6)
    },
    logical(1)
  )))
})

test_that("recorded WS10-10 validation artifacts are complete", {
  validation <- utils::read.csv(
    system.file(
      "validation", "ws10-10-validation.csv",
      package = "PhysioStream"
    ),
    stringsAsFactors = FALSE
  )
  mutations <- utils::read.csv(
    system.file(
      "validation", "ws10-10-mutations.csv",
      package = "PhysioStream"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(validation), 304L)
  expect_true(all(validation$pass))
  expect_equal(table(validation$category)[["emg"]], 100)
  expect_equal(table(validation$category)[["erd"]], 100)
  expect_equal(table(validation$category)[["phase"]], 100)
  expect_equal(nrow(mutations), 71L)
  expect_true(all(mutations$pass))
})
