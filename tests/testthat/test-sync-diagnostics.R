test_that("diagnostics expose clock, jitter, extrapolation, and quality", {
  master_time <- seq(0, 1, by = 0.01)
  device <- master_time - 0.1
  x <- sync_test_container(list(
    master = sync_test_pe(master_time),
    device = sync_test_pe(device)
  ))
  model <- clockOffset(device[c(1, 51, 101)], rep(0.1, 3), method = "ols")
  result <- syncStreams(
    x, master = "master", models = list(device = model), dejitter = TRUE
  )
  diagnostics <- syncDiagnostics(result)

  expect_identical(names(diagnostics), c(
    "stream", "segment", "n_samples", "n_clock_observations",
    "start_master", "end_master", "offset_start_seconds",
    "offset_end_seconds", "drift_ppm", "clock_rmse_seconds",
    "jitter_rmse_seconds", "jitter_p95_seconds", "jitter_max_seconds",
    "shared_event_count", "shared_event_rmse_seconds",
    "extrapolated_fraction", "quality"
  ))
  expect_identical(diagnostics$stream, c("master", "device"))
  expect_equal(diagnostics$offset_start_seconds, c(0, 0.1),
               tolerance = 1e-14)
  expect_equal(diagnostics$drift_ppm, c(0, 0), tolerance = 1e-9)
  expect_identical(diagnostics$quality, c("ok", "ok"))
})

test_that("caller residual gates are enforced", {
  master_time <- seq(0, 1, by = 0.01)
  device <- master_time - 0.1
  x <- sync_test_container(list(
    master = sync_test_pe(master_time),
    device = sync_test_pe(device)
  ))
  observation <- device[c(1, 26, 51, 76, 101)]
  offset <- c(0.1, 0.1, 0.101, 0.1, 0.1)
  model <- clockOffset(observation, offset, method = "ols")
  expect_error(
    syncStreams(
      x, master = "master", models = list(device = model),
      max_residual_seconds = 1e-5
    ),
    "residual gate"
  )
})
