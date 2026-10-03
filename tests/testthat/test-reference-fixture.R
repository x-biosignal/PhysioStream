test_that("the installed ring reference is hash-bound and schema-valid", {
  fixture <- system.file("extdata", "ring_buffer_reference.rds",
                         package = "PhysioStream")
  manifest <- system.file("extdata", "ring_buffer_reference.sha256",
                          package = "PhysioStream")
  expect_true(nzchar(fixture))
  expect_true(nzchar(manifest))
  expected <- strsplit(readLines(manifest, warn = FALSE)[[1L]], " +")[[1L]][1L]
  observed <- digest::digest(file = fixture, algo = "sha256", serialize = FALSE)
  expect_identical(observed, expected)
  reference <- readRDS(fixture)
  expect_identical(reference$schema_version, "1.0.0")
  expect_identical(reference$license, "CC0-1.0")
  expect_true(length(reference$cases) >= 10L)
  expect_named(
    reference$operation_scenarios,
    c("partial_pull", "empty_pull", "reset")
  )
  expect_identical(
    reference$operation_scenarios$partial_pull$retained_sequence,
    2
  )
  expect_identical(
    reference$operation_scenarios$reset$next_sequence_after,
    3
  )
  expect_true(all(vapply(reference$cases, function(x) {
    is.matrix(x$samples) && all(is.finite(x$samples)) &&
      all(diff(x$timestamps) > 0)
  }, logical(1))))
})

test_that("foundation code does not access optional transports", {
  package_path <- testthat::test_path("..", "..")
  files <- list.files(
    file.path(package_path, "R"),
    full.names = TRUE,
    pattern = "\\.[Rr]$"
  )
  files <- files[
    !startsWith(basename(files), "lsl-") &
      !(basename(files) %in% c(
        "xdf-backend.R", "trigger-mqtt.R", "trigger-ttl.R"
      ))
  ]
  code <- paste(
    vapply(
      files,
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  expect_false(grepl("reticulate::|pylsl|liblsl", code))
})
