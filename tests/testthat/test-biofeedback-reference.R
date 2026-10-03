test_that("biofeedback reference asset validates independently", {
  path <- system.file(
    "extdata", "biofeedback_reference.rds",
    package = "PhysioStream", mustWork = FALSE
  )
  skip_if(!nzchar(path), "reference asset not generated yet")
  reference <- readRDS(path)
  expect_identical(reference$schema_version, "1.0.0")
  expect_gte(length(reference$scope_cases), 100L)
  expect_gte(length(reference$video_cases), 100L)
  expect_gte(length(reference$mutations), 50L)
})
