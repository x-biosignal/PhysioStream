test_that("explicit models synchronize three streams without changing assays", {
  master_time <- seq(0, 2, by = 0.01)
  device_b <- (master_time - 0.2) / 1.005
  device_c <- (master_time + 0.4) / 0.997
  x <- sync_test_container(list(
    master = sync_test_pe(master_time),
    b = sync_test_pe(device_b),
    c = sync_test_pe(device_c)
  ))
  before <- lapply(as.list(PhysioCore::streams(x)),
                   SummarizedExperiment::assay)
  observation <- seq(1, length(master_time), length.out = 20)
  observation <- unique(as.integer(round(observation)))
  model_b <- clockOffset(
    device_b[observation],
    master_time[observation] - device_b[observation],
    method = "ols"
  )
  model_c <- clockOffset(
    device_c[observation],
    master_time[observation] - device_c[observation],
    method = "ols"
  )

  set.seed(1001)
  rng <- .Random.seed
  result <- syncStreams(
    x, master = "master", models = list(b = model_b, c = model_c),
    dejitter = TRUE
  )
  expect_identical(.Random.seed, rng)
  expect_identical(PhysioCore::streamNames(result), c("master", "b", "c"))
  for (key in names(before)) {
    expect_identical(SummarizedExperiment::assay(result[[key]]), before[[key]])
    row_data <- SummarizedExperiment::rowData(result[[key]])
    expect_true(all(c(
      "stream_time_raw", "stream_time_master",
      "stream_time_dejittered"
    ) %in% names(row_data)))
    expect_equal(row_data$stream_time_master, master_time, tolerance = 2e-14)
  }
  expect_false("stream_time_master" %in%
                 names(SummarizedExperiment::rowData(x[["master"]])))
  expect_identical(PhysioCore::commonClock(result)$sync$master, "master")
  expect_equal(PhysioCore::commonClock(result)$offsets,
               c(master = 0, b = 0, c = 0), tolerance = 2e-14)
})

test_that("shared events fit streams that lack clock observations", {
  master_time <- seq(0, 4, by = 0.02)
  device <- (master_time - 0.3) / 1.002
  x <- sync_test_container(list(
    master = sync_test_pe(master_time, rate = 50),
    device = sync_test_pe(device, rate = 50)
  ), rate = 50)
  at <- c(1L, 51L, 101L, 151L, 201L)
  events <- data.frame(
    event_id = rep(paste0("e", seq_along(at)), each = 2L),
    stream = rep(c("master", "device"), length(at)),
    timestamp = as.vector(rbind(master_time[at], device[at])),
    stringsAsFactors = FALSE
  )
  result <- syncStreams(
    x, master = "master", shared_events = events
  )
  expect_equal(
    SummarizedExperiment::rowData(result[["device"]])$stream_time_master,
    master_time,
    tolerance = 2e-14
  )
  diagnostics <- syncDiagnostics(result)
  expect_identical(diagnostics$shared_event_count, c(0L, 5L))
  expect_lt(diagnostics$shared_event_rmse_seconds[[2L]], 1e-14)

  bad <- events[-1L, ]
  expect_error(
    syncStreams(x, master = "master", shared_events = bad),
    "lack matching master"
  )
})

test_that("automatic models use XDF clock evidence and preserve empty streams", {
  master_time <- seq(10, 11, by = 0.01)
  device <- master_time - 2
  clock_index <- seq(1, length(device), by = 10)
  empty <- sync_test_pe(numeric(), rate = 100, values = matrix(numeric(), 0, 1))
  x <- sync_test_container(list(
    master = sync_test_pe(master_time),
    device = sync_test_pe(
      device,
      clock_times = device[clock_index],
      clock_values = rep(2, length(clock_index))
    ),
    empty = empty
  ))
  result <- syncStreams(x, master = "master")
  expect_equal(
    SummarizedExperiment::rowData(result[["device"]])$stream_time_master,
    master_time,
    tolerance = 1e-14
  )
  expect_identical(nrow(SummarizedExperiment::rowData(result[["empty"]])), 0L)
  diagnostics <- syncDiagnostics(result)
  expect_identical(diagnostics$n_samples, c(101L, 101L, 0L))
  expect_identical(diagnostics$quality, c("ok", "ok", "ok"))
})

test_that("synchronization identity and evidence validation fail loudly", {
  x <- sync_test_container(list(
    a = sync_test_pe(0:2),
    b = sync_test_pe(1:3)
  ))
  expect_error(syncStreams(x, master = "missing"), "exact stream key")
  expect_error(syncStreams(x, master = "a", max_residual_seconds = Inf),
               "non-negative number")
  expect_error(syncStreams(x, master = "a"), "lacks a supplied model")
  expect_error(syncStreams(x, master = "a", models = list(nope =
                 clockOffset(1:3, rep(0, 3), method = "ols"))),
               "keyed by")
  events <- data.frame(
    event_id = c("one", "one"),
    stream = c("a", "b"),
    timestamp = c(0, 1),
    stringsAsFactors = FALSE
  )
  expect_error(syncStreams(x, master = "a", shared_events = events),
               "lacks a supplied model")
})

test_that("official XDF clock resets are fitted per segment", {
  skip_without_pyxdf()
  x <- readXDF(xdf_fixture("clock-resets"))
  result <- syncStreams(x, master = "xdf_stream_1")
  diagnostics <- syncDiagnostics(result)

  expect_identical(
    diagnostics$stream,
    c("xdf_stream_1", "xdf_stream_2", "xdf_stream_2")
  )
  expect_identical(diagnostics$segment, c(1L, 1L, 2L))
  expect_identical(diagnostics$n_clock_observations, c(0L, 82L, 33L))
  expect_true(all(diagnostics$quality %in% c("ok", "warn")))
  row_data <- SummarizedExperiment::rowData(result[["xdf_stream_2"]])
  expect_true(all(vapply(
    split(row_data$stream_time_master, row_data$xdf_segment),
    function(value) all(diff(value) >= 0),
    logical(1)
  )))
})
