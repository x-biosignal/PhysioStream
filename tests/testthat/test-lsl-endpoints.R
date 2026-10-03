test_that("inlet construction is side-effect free and validates boundaries", {
  info <- lsl_test_info()
  inlet <- lslInlet(info, capacity = 8L, max_chunk = 4L)
  expect_identical(streamState(inlet), "created")
  expect_null(inlet@runtime$adapter)
  expect_s4_class(inlet@buffer, "RingBuffer")

  expect_error(
    lslInlet(info, capacity = 4L, max_chunk = 5L),
    "must not exceed"
  )
  expect_error(
    lslInlet(info, recover = TRUE),
    NA
  )
  no_identity <- streamInfo(
    "x", "EEG", "Cz", 100, "float64", "id", "lsl", "uV"
  )
  expect_error(lslInlet(no_identity), "lslResolveStreams")
})

test_that("numeric pulls fill the ring and account for local overrun", {
  info <- lsl_test_info(n_channels = 2L)
  chunks <- list(
    list(
      list(c(1, 10), c(2, 20), c(3, 30)),
      c(1, 2, 3)
    ),
    list(
      list(c(4, 40), c(5, 50), c(6, 60)),
      c(4, 5, 6)
    )
  )
  inlet <- lsl_test_open_inlet(
    info, chunks, capacity = 4L, max_chunk = 4L
  )
  first <- lslPull(inlet, 4L)
  expect_identical(first$received, 3L)
  expect_equal(first$dropped_increment, 0, tolerance = 0)
  second <- streamPull(inlet, max_samples = 4L)
  expect_identical(second$received, 3L)
  expect_equal(second$dropped_increment, 2, tolerance = 0)
  retained <- ringPeek(inlet@buffer)
  expect_equal(retained$samples, rbind(c(3, 30), c(4, 40), c(5, 50), c(6, 60)))
  expect_equal(retained$timestamps, 3:6, tolerance = 0)
  expect_equal(second$total_received, 6, tolerance = 0)
})

test_that("zero and empty pulls leave endpoint state unchanged", {
  inlet <- lsl_test_open_inlet(lsl_test_info(), chunks = list())
  zero <- lslPull(inlet, 0L)
  expect_identical(zero$received, 0L)
  expect_equal(zero$total_received, 0, tolerance = 0)
  empty <- lslPull(inlet, 2L)
  expect_identical(empty$received, 0L)
  expect_identical(ringFill(inlet@buffer), 0L)
  expect_error(lslPull(inlet, 9L), "max_chunk")
})

test_that("malformed pulls are transactional", {
  info <- lsl_test_info()
  malformed <- list(
    list(list(c(1, 2), c(3)), c(1, 2)),
    list(list(c(1, 2), c(3, 4)), c(2, 1)),
    list(list(c(1, 2)), "bad")
  )
  for (chunk in malformed) {
    inlet <- lsl_test_open_inlet(info, chunks = list(chunk))
    before <- ringStats(inlet@buffer)
    expect_error(lslPull(inlet, 4L))
    after <- ringStats(inlet@buffer)
    expect_identical(after, before)
    expect_equal(inlet@runtime$total_received, 0, tolerance = 0)
  }
})

test_that("numeric outlet validation precedes backend publication", {
  info <- streamInfo(
    "out", "EEG", c("C3", "C4"), 100, "int16", "source", "lsl", c("uV", "uV")
  )
  outlet <- lsl_test_open_outlet(info)
  expect_error(
    lslPush(outlet, matrix(c(1, 2.5), nrow = 1L)),
    "exactly representable"
  )
  expect_length(outlet@runtime$handle$calls, 0L)
  expect_error(
    lslPush(
      outlet, matrix(as.double(1:4), 2L, 2L),
      c(2, 1)
    ),
    "strictly increasing"
  )
  expect_length(outlet@runtime$handle$calls, 0L)

  pushed <- lslPush(
    outlet, matrix(as.double(1:4), 2L, 2L), c(10, 11)
  )
  expect_identical(pushed$pushed, 2L)
  expect_length(outlet@runtime$handle$calls, 2L)
  expect_false(outlet@runtime$handle$calls[[1L]]$pushthrough)
  expect_true(outlet@runtime$handle$calls[[2L]]$pushthrough)
  expect_error(
    lslPush(outlet, matrix(as.double(5:6), 1L, 2L), 11),
    "across outlet"
  )
})

test_that("mid-push failures retain exact confirmed accounting", {
  info <- streamInfo(
    "out", "EEG", "Cz", 100, "float64", "source", "lsl", "uV"
  )
  outlet <- lsl_test_open_outlet(info, fail_at = 2L)
  expect_error(
    lslPush(outlet, matrix(as.double(1:3), 3L, 1L), c(1, 2, 3)),
    "1 of 3 confirmed"
  )
  expect_identical(streamState(outlet), "error")
  expect_equal(outlet@runtime$total_pushed, 1, tolerance = 0)
  expect_equal(outlet@runtime$last_timestamp, 1, tolerance = 0)
})

test_that("float32 validation rounds before transport and rejects overflow", {
  info <- streamInfo(
    "out", "EEG", "Cz", 100, "float32", "source", "lsl", "uV"
  )
  outlet <- lsl_test_open_outlet(info)
  original <- matrix(1 / 10, 1L, 1L)
  pushed <- lslPush(outlet, original)
  expect_identical(pushed$pushed, 1L)
  sent <- outlet@runtime$handle$calls[[1L]]$samples[[1L]][[1L]]
  expect_equal(sent, PhysioStream:::.lsl_float32(0.1), tolerance = 0)
  expect_equal(original, matrix(0.1, 1L, 1L), tolerance = 0)
  expect_error(
    lslPush(outlet, matrix(.Machine$double.xmax, 1L, 1L)),
    "overflows finite float32"
  )
})

test_that("LSL numeric inlet snapshots retain transport evidence", {
  info <- lsl_test_info(n_channels = 1L, labels = "Cz", units = "uV")
  inlet <- lsl_test_open_inlet(
    info,
    chunks = list(list(list(c(1), c(2), c(3)), c(10, 10.01, 10.02)))
  )
  lslPull(inlet, 4L)
  pe <- streamSnapshot(inlet)
  expect_s4_class(pe, "PhysioExperiment")
  expect_equal(
    unname(SummarizedExperiment::assay(pe)),
    matrix(c(1, 2, 3), 3L, 1L)
  )
  expect_equal(SummarizedExperiment::rowData(pe)$time_seconds,
               c(10, 10.01, 10.02), tolerance = 0)
  transport <- S4Vectors::metadata(pe)$stream$transport
  expect_identical(transport$backend, "pylsl")
  expect_equal(transport$total_received, 3, tolerance = 0)
  expect_identical(
    transport$descriptor_sha256,
    info@metadata$lsl$descriptor_sha256
  )
})
