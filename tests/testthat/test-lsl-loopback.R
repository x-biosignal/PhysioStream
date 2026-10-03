test_that("pylsl localhost round-trips every governed numeric dtype", {
  skip_if_not(lslAvailable(initialize = TRUE))
  cases <- list(
    float64 = matrix(c(0.1, -2, 3.5, 4), 2L, 2L),
    float32 = matrix(c(0.5, -1.25, 3, 4.5), 2L, 2L),
    int32 = matrix(as.double(c(-2147483648, 0, 2147483647, 42)), 2L, 2L),
    int16 = matrix(as.double(c(-32768, 0, 32767, 42)), 2L, 2L),
    int8 = matrix(as.double(c(-128, 0, 127, 42)), 2L, 2L)
  )
  for (dtype in names(cases)) {
    source_id <- paste0(
      "physiostream-", dtype, "-", Sys.getpid()
    )
    info <- streamInfo(
      paste("PhysioStream", dtype), "EEG", c("C3", "C4"), 100,
      dtype, source_id, "lsl", c("uV", "uV")
    )
    outlet <- streamOpen(lslOutlet(info))
    inlet <- NULL
    on.exit(lsl_close_quietly(inlet), add = TRUE)
    on.exit(lsl_close_quietly(outlet), add = TRUE)
    Sys.sleep(0.1)
    resolved <- lslResolveStreams(
      "source_id", source_id, minimum = 1L, timeout = 3
    )
    expect_length(resolved, 1L)
    expect_identical(streamDtype(resolved[[1L]]), dtype)
    expect_identical(streamChannels(resolved[[1L]]), c("C3", "C4"))
    expect_identical(resolved[[1L]]@channel_units, c("uV", "uV"))
    inlet <- streamOpen(lslInlet(
      resolved[[1L]], capacity = 8L, max_chunk = 4L, recover = FALSE
    ), timeout = 3)
    timestamps <- 1000 + c(0, 0.01)
    pushed <- lslPush(outlet, cases[[dtype]], timestamps)
    expect_identical(pushed$pushed, 2L)
    pulled <- lslPull(inlet, 4L, timeout = 3)
    expect_identical(pulled$received, 2L)
    observed <- ringPeek(inlet@buffer)
    expected <- if (dtype == "float32") {
      rounded <- cases[[dtype]]
      rounded[] <- PhysioStream:::.lsl_float32(rounded)
      rounded
    } else {
      cases[[dtype]]
    }
    expect_equal(observed$samples, expected, tolerance = 0)
    expect_equal(observed$timestamps, timestamps, tolerance = 0)
    inlet <- streamClose(inlet)
    outlet <- streamClose(outlet)
  }
})

test_that("pylsl localhost round-trips irregular string markers", {
  skip_if_not(lslAvailable(initialize = TRUE))
  source_id <- paste0(
    "physiostream-markers-", Sys.getpid()
  )
  info <- streamInfo(
    "PhysioStream markers", "Markers", c("code", "detail"), 0,
    "string", source_id, "lsl"
  )
  outlet <- streamOpen(lslOutlet(info))
  inlet <- NULL
  on.exit(lsl_close_quietly(inlet), add = TRUE)
  on.exit(lsl_close_quietly(outlet), add = TRUE)
  Sys.sleep(0.1)
  resolved <- lslResolveStreams(
    "source_id", source_id, minimum = 1L, timeout = 3
  )
  inlet <- streamOpen(lslInlet(
    resolved[[1L]], max_chunk = 4L, marker_capacity = 4L,
    recover = FALSE
  ), timeout = 3)
  markers <- matrix(
    c("go", "a&b", "停止", "\"quoted\""), 2L, 2L, byrow = TRUE
  )
  timestamps <- c(2000, 2001)
  lslPush(outlet, markers, timestamps)
  pulled <- lslPull(inlet, 4L, timeout = 3)
  expect_identical(pulled$received, 2L)
  events <- lslMarkerEvents(inlet)
  expect_equal(
    attr(events, "lsl")$original_timestamps, timestamps, tolerance = 0
  )
  decoded <- lapply(
    as.character(events@events$value),
    jsonlite::fromJSON, simplifyVector = TRUE
  )
  expect_identical(unname(unlist(decoded[[1L]])), markers[1L, ])
  expect_identical(unname(unlist(decoded[[2L]])), markers[2L, ])
  inlet <- streamClose(inlet)
  outlet <- streamClose(outlet)
})
