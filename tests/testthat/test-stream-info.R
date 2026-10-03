test_that("StreamInfo validates and round-trips exactly", {
  info <- make_stream_info()
  expect_s4_class(info, "StreamInfo")
  expect_identical(streamName(info), "test-stream")
  expect_identical(streamType(info), "EEG")
  expect_identical(streamChannels(info), c("C3", "C4"))
  expect_identical(streamRate(info), 100)
  expect_identical(streamDtype(info), "float64")
  expect_identical(unserialize(serialize(info, NULL)), info)
  expect_output(show(info), "channels: 2")
  expect_false(any(grepl("synthetic", capture.output(show(info)), fixed = TRUE)))
})

test_that("StreamInfo rejects malformed identity, channels, and enums", {
  args <- list(
    name = "x", type = "EEG", channel_names = c("a", "b"),
    nominal_srate = 100
  )
  expect_error(do.call(streamInfo, modifyList(args, list(name = ""))),
               "non-empty")
  expect_error(do.call(streamInfo, modifyList(args, list(type = NA_character_))),
               "non-empty")
  expect_error(do.call(streamInfo, modifyList(args, list(
    channel_names = c("a", "a")
  ))), "unique")
  expect_error(do.call(streamInfo, modifyList(args, list(
    channel_units = "uV"
  ))), "match")
  expect_error(do.call(streamInfo, modifyList(args, list(
    nominal_srate = -1
  ))), ">= 0")
  expect_error(do.call(streamInfo, modifyList(args, list(dtype = "float"))),
               "exact")
  expect_s4_class(do.call(streamInfo, modifyList(args, list(
    nominal_srate = 0, dtype = "string"
  ))), "StreamInfo")
})

test_that("StreamInfo metadata is finite, named, bounded, and inert", {
  args <- list(
    name = "x", type = "EEG", channel_names = "a", nominal_srate = 100
  )
  expect_error(do.call(streamInfo, c(args, list(metadata = list(1)))),
               "unique")
  expect_error(do.call(streamInfo, c(args, list(
    metadata = list(a = list(1))
  ))), "unique")
  expect_error(do.call(streamInfo, c(args, list(
    metadata = list(a = Inf)
  ))), "non-finite")
  expect_error(do.call(streamInfo, c(args, list(
    metadata = list(a = new.env())
  ))), "finite atomic")
  expect_error(do.call(streamInfo, c(args, list(
    metadata = list(a = raw(1024L * 1024L))
  ))), "1 MiB")
})
