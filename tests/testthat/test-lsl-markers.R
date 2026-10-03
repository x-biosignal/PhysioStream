test_that("marker queue maps one-channel values to PhysioEvents", {
  info <- lsl_test_info(
    type = "Markers", n_channels = 1L, nominal_srate = 0,
    format = "string", labels = "code", units = NULL
  )
  inlet <- lsl_test_open_inlet(
    info,
    chunks = list(list(list(c(""), c("停止"), c("a&<b")), c(100, 101, 102))),
    max_chunk = 4L,
    marker_capacity = 4L
  )
  pulled <- lslPull(inlet, 4L)
  expect_identical(pulled$received, 3L)
  events <- lslMarkerEvents(inlet, time_origin = 99)
  expect_s4_class(events, "PhysioEvents")
  expect_equal(events@events$onset, c(1, 2, 3), tolerance = 0)
  expect_identical(as.character(events@events$value), c("", "停止", "a&<b"))
  expect_identical(as.character(events@events$type), rep("Markers", 3L))
  expect_equal(
    attr(events, "lsl")$original_timestamps, c(100, 101, 102),
    tolerance = 0
  )
})

test_that("multi-channel markers use stable canonical JSON", {
  info <- lsl_test_info(
    type = "Markers", n_channels = 2L, nominal_srate = 0,
    format = "string", labels = c("code", "detail"), units = NULL
  )
  inlet <- lsl_test_open_inlet(
    info,
    chunks = list(list(
      list(c("go", "\"quoted\""), c("停止", "\\")),
      c(1, 2)
    ))
  )
  lslPull(inlet, 4L)
  events <- lslMarkerEvents(inlet)
  expect_identical(
    as.character(events@events$value),
    c(
      "{\"code\":\"go\",\"detail\":\"\\\"quoted\\\"\"}",
      "{\"code\":\"停止\",\"detail\":\"\\\\\"}"
    )
  )
})

test_that("marker overrun and consumption accounting is exact", {
  info <- lsl_test_info(
    type = "Markers", n_channels = 1L, nominal_srate = 0,
    format = "string", labels = "code", units = NULL
  )
  inlet <- lsl_test_open_inlet(
    info,
    chunks = list(list(
      lapply(letters[1:5], c), as.double(1:5)
    )),
    max_chunk = 3L,
    marker_capacity = 3L
  )
  # The fake backend may return more than requested; the payload is rejected.
  expect_error(lslPull(inlet, 3L), "more samples than requested")

  inlet <- lsl_test_open_inlet(
    info,
    chunks = list(
      list(lapply(letters[1:3], c), as.double(1:3)),
      list(lapply(letters[4:5], c), as.double(4:5))
    ),
    max_chunk = 3L,
    marker_capacity = 3L
  )
  lslPull(inlet, 3L)
  second <- lslPull(inlet, 3L)
  expect_equal(second$dropped_increment, 2, tolerance = 0)
  events <- lslMarkerEvents(inlet)
  expect_identical(as.character(events@events$value), letters[3:5])
  expect_error(
    lslMarkerEvents(inlet, n = 2L, consume = TRUE),
    "complete oldest"
  )
  consumed <- lslMarkerEvents(inlet, consume = TRUE)
  expect_identical(as.character(consumed@events$value), letters[3:5])
  expect_identical(
    PhysioStream:::.lsl_marker_stats(inlet@buffer)$fill, 0L
  )
  expect_equal(
    PhysioStream:::.lsl_marker_stats(inlet@buffer)$total_pulled,
    3, tolerance = 0
  )
})

test_that("marker outlet requires governed character matrices", {
  info <- streamInfo(
    "markers", "Markers", "code", 0, "string", "source", "lsl"
  )
  outlet <- lsl_test_open_outlet(info)
  expect_error(lslPush(outlet, factor("x")), "character")
  expect_error(
    lslPush(outlet, matrix(c("a", "b"), 2L, 1L)),
    "explicit timestamps"
  )
  expect_length(outlet@runtime$handle$calls, 0L)
  pushed <- lslPush(
    outlet, matrix(c("", "停止"), 2L, 1L), c(1, 2)
  )
  expect_identical(pushed$pushed, 2L)
  expect_identical(
    outlet@runtime$handle$calls[[2L]]$sample[[1L]], "停止"
  )
})
