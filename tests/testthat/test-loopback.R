test_that("loopback lifecycle is explicit and errors do not mutate state", {
  source <- loopbackSource(make_stream_info(), 4L)
  expect_s4_class(source, "StreamSource")
  expect_identical(streamState(source), "created")
  expect_error(loopbackFeed(source, make_samples(1), 1), "open")
  expect_identical(streamState(source), "created")

  source <- streamOpen(source)
  expect_identical(streamState(source), "open")
  expect_error(streamOpen(source), "invalid")
  expect_identical(streamState(source), "open")
  loopbackFeed(source, make_samples(2), c(1, 2))
  expect_identical(streamPull(source, 1L)$sequence, 0)

  source <- streamClose(source)
  expect_identical(streamState(source), "closed")
  expect_error(loopbackFeed(source, make_samples(1), 3), "open")
  source2 <- streamClose(source)
  expect_identical(streamState(source2), "closed")
  expect_length(source2@audit, 2L)
  expect_error(streamOpen(source2), "invalid")
})

test_that("loopback aliases only its native buffer", {
  source <- streamOpen(loopbackSource(make_stream_info(), 4L))
  alias <- source
  loopbackFeed(alias, make_samples(2), c(1, 2))
  expect_identical(ringFill(source@buffer), 2L)
  closed <- streamClose(alias)
  expect_identical(streamState(source), "open")
  expect_identical(streamState(closed), "closed")
})
