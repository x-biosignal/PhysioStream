test_that("XDF capability arguments are exact and conservative", {
  expect_error(xdfAvailable("py"), class = "PhysioStream_validation_error")
  expect_error(xdfAvailable(NA_character_),
               class = "PhysioStream_validation_error")
  expect_error(xdfAvailable(c("auto", "pyxdf")),
               class = "PhysioStream_validation_error")
  expect_error(xdfAvailable("auto", NA),
               class = "PhysioStream_validation_error")

  result <- expect_silent(xdfAvailable(initialize = FALSE))
  expect_type(result, "logical")
  expect_length(result, 1L)
  expect_false(is.na(result))
})

test_that("configured XDF backend reports serializable versions", {
  skip_without_pyxdf()
  info <- xdfBackendInfo()
  expect_identical(
    names(info),
    c(
      "backend", "python", "python_version", "reticulate_version",
      "pyxdf_version", "numpy_version", "capability_schema"
    )
  )
  expect_identical(info$backend, "pyxdf")
  expect_identical(info$pyxdf_version, "1.17.2")
  expect_identical(info$capability_schema, "1.0.0")
  expect_silent(serialize(info, NULL, version = 3L))
  expect_false(any(vapply(info, reticulate::is_py_object, logical(1))))
})

test_that("native writing remains available when the read backend is absent", {
  testthat::local_mocked_bindings(
    .xdf_import_adapter = function(backend) {
      PhysioStream:::.stream_abort(
        "configured pyxdf is unavailable",
        "PhysioStream_xdf_unavailable"
      )
    },
    .package = "PhysioStream"
  )
  pe <- xdf_test_pe(matrix(1:4, 2L, 2L), "int16")
  path <- tempfile(fileext = ".xdf")
  on.exit(unlink(path), add = TRUE)
  expect_silent(writeXDF(pe, path))
  expect_true(file.exists(path))
  expect_error(
    readXDF(path),
    class = "PhysioStream_xdf_unavailable"
  )
})
