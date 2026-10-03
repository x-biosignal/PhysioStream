test_that("stopped session appends bounded structured provenance", {
  values <- closed_loop_emg()
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, 50, 8, enter_z = 6, min_on_samples = 3
    )
  )
  closedLoopStart(setup$controller, "provenance-session", now_ns = 0)
  closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "emg")),
    now_ns = 1e6
  )
  expect_error(
    closedLoopProvenance(
      setup$controller,
      PhysioCore::PhysioExperiment(
        S4Vectors::SimpleList(raw = matrix(1:20, 10, 2)),
        samplingRate = 100
      )
    ),
    "stopped"
  )
  closedLoopStop(setup$controller, now_ns = 2e6)
  pe <- PhysioCore::PhysioExperiment(
    S4Vectors::SimpleList(raw = matrix(1:20, 10, 2)),
    samplingRate = 100
  )
  original <- serialize(pe, NULL, version = 3L)
  result <- closedLoopProvenance(setup$controller, pe, "raw")
  expect_identical(serialize(pe, NULL, version = 3L), original)
  entries <- S4Vectors::metadata(result)$provenance
  expect_length(entries, 1L)
  entry <- entries[[1L]]
  expect_identical(entry$activity, "closed_loop_session")
  expect_identical(entry$params$research_only, TRUE)
  expect_identical(
    entry$params$acknowledgement_is_not_delivery,
    TRUE
  )
  expect_identical(
    entry$params$session_log,
    closedLoopLog(setup$controller)
  )
  expect_match(entry$params$session_log_sha256, "^[0-9a-f]{64}$")
  serialized <- unclass(jsonlite::toJSON(
    entry$params, auto_unbox = TRUE, null = "null"
  ))
  expect_false(any(grepl(
    "provenance-session|password|credential",
    serialized,
    fixed = FALSE
  )))
  expect_error(
    closedLoopProvenance(setup$controller, result, "raw"),
    "already"
  )
  expect_error(
    closedLoopProvenance(setup$controller, pe, "missing"),
    "existing assay"
  )
  malformed <- pe
  S4Vectors::metadata(malformed)$provenance <- list("not-an-entry")
  expect_silent(
    closedLoopProvenance(setup$controller, malformed, "raw")
  )
})

test_that("state and log accessors return deep copies", {
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5)
  )
  state <- closedLoopState(setup$controller)
  state$lifecycle$status <- "running"
  log <- closedLoopLog(setup$controller)
  log[[1L]] <- list(secret = TRUE)
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "constructed"
  )
  expect_length(closedLoopLog(setup$controller), 0L)
})

test_that("a never-started terminal controller has no session provenance", {
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5)
  )
  closedLoopStop(setup$controller, now_ns = 0)
  pe <- PhysioCore::PhysioExperiment(
    S4Vectors::SimpleList(raw = matrix(1:20, 10, 2)),
    samplingRate = 100
  )
  expect_error(
    closedLoopProvenance(setup$controller, pe, "raw"),
    "session that was started"
  )
})
