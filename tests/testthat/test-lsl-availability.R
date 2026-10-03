test_that("LSL capability arguments are exact", {
  expect_error(lslAvailable("py"), "exactly")
  expect_error(lslAvailable(NA_character_), "exactly")
  expect_error(lslAvailable(c("auto", "pylsl")), "exactly")
  expect_error(lslAvailable(initialize = NA), "non-missing logical")
  expect_type(lslAvailable(initialize = FALSE), "logical")
  expect_length(lslAvailable(initialize = FALSE), 1L)
})

test_that("created endpoints do not import a backend and close idempotently", {
  info <- streamInfo(
    "offline", "EEG", "Cz", 100, "float64", "offline-id", "lsl", "uV"
  )
  outlet <- lslOutlet(info)
  expect_identical(streamState(outlet), "created")
  expect_null(outlet@runtime$adapter)
  outlet <- streamClose(outlet)
  expect_identical(streamState(outlet), "closed")
  expect_null(outlet@runtime$adapter)
  expect_identical(streamState(streamClose(outlet)), "closed")
  expect_error(streamOpen(outlet), "created")
})

test_that("serialized endpoint runtimes fail before backend access", {
  info <- streamInfo(
    "offline", "EEG", "Cz", 100, "float64", "offline-id", "lsl", "uV"
  )
  restored <- unserialize(serialize(lslOutlet(info), NULL, version = 3L))
  expect_error(
    streamState(restored),
    class = "PhysioStream_lsl_lifetime_error"
  )
})

test_that("available backend reports only serializable version data", {
  skip_if_not(lslAvailable(initialize = TRUE))
  info <- lslBackendInfo()
  expect_named(
    info,
    c(
      "backend", "python", "python_version", "reticulate_version",
      "pylsl_version", "liblsl_version", "liblsl_info",
      "capability_schema"
    )
  )
  expect_identical(info$backend, "pylsl")
  expect_match(info$python_version, "^[0-9]+\\.[0-9]+\\.[0-9]+")
  expect_identical(info$capability_schema, "1.0.0")
  expect_false(any(vapply(info, reticulate::is_py_object, logical(1))))
  expect_silent(serialize(info, NULL, version = 3L))
})
