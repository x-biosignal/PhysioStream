test_that("offset and affine video maps preserve correspondence", {
  offset <- videoSync(
    c(a = 100, b = 110, c = 120),
    c(a = 1, b = 11, c = 21),
    frame_rate = 30,
    method = "offset",
    clock_domain = "fixture"
  )
  expect_equal(videoSyncTime(offset, c(x = 105, y = 115)), c(x = 6, y = 16))
  expect_equal(signalSyncTime(offset, c(x = 6, y = 16)), c(x = 105, y = 115))

  signal <- c(a = 1e6, b = 1e6 + 10, c = 1e6 + 25, d = 1e6 + 40)
  video <- 2 + 1.0002 * (signal - 1e6)
  affine <- videoSync(
    signal, video, 59.94, "affine", "fixture",
    tolerance_frames = 1e-6
  )
  expect_equal(unclass(affine)$rate, 1.0002, tolerance = 1e-12)
  mapped <- videoSyncTime(affine, c(p = 1e6 + 5, q = 1e6 + 30))
  expect_equal(
    signalSyncTime(affine, mapped),
    c(p = 1e6 + 5, q = 1e6 + 30),
    tolerance = 1e-10
  )
  expect_match(capture.output(print(affine)), "anchors=4")
})

test_that("video synchronization rejects timing and integrity failures", {
  expect_error(
    videoSync(c(1, 2), c(1, 2), 30, "affine", "fixture"),
    "at least three"
  )
  expect_error(
    videoSync(c(1, 2, 3), c(1, 2.2, 3), 30, "affine", "fixture"),
    "rate error|residual"
  )
  expect_error(
    videoSync(c(1, 2), c(1, 2.1), 30, "offset", "fixture",
              tolerance_frames = 1),
    "residual"
  )
  expect_error(
    videoSync(c(1, 1), c(1, 2), 30, "offset", "fixture"),
    "strictly increasing"
  )
  expect_error(
    videoSync(c(a = 1), c(b = 1), 30, "offset", "fixture"),
    "identical unique names"
  )

  sync <- videoSync(1, 0, 30, "offset", "fixture")
  corrupted <- sync
  corrupted$rate <- 2
  expect_error(videoSyncTime(corrupted, 1), "integrity")
  forged <- sync
  forged$rate <- 2
  forged$rate_error_ppm <- 1e6
  forged$sha256 <- PhysioStream:::.biofeedback_hash(unclass(forged))
  expect_error(videoSyncTime(forged, 1), "invalid|inconsistent")
  expect_equal(videoSyncTime(sync, 1 - 1 / 60), 0)
  expect_error(videoSyncTime(sync, 0), "precedes media start")
  expect_equal(signalSyncTime(sync, -1 / 60), 1)
  expect_error(signalSyncTime(sync, -1), "precedes media start")
  expect_error(
    videoSyncTime(sync, 1e11),
    "microsecond resolution"
  )
})

test_that("video viewer validates local and remote resources lazily", {
  source <- biofeedback_test_source()
  sync <- videoSync(0, 0, 30, "offset", "biofeedback-fixture")
  path <- tempfile(fileext = ".mp4")
  writeBin(as.raw(1:20), path)
  on.exit(unlink(path), add = TRUE)

  scope <- videoSyncViewer(
    source, path, sync, launch = FALSE,
    clock = biofeedback_test_clock()
  )
  expect_s3_class(scope, "VideoSyncViewer")
  expect_true(file.exists(path))
  expect_false(normalizePath(path) %in% unlist(
    biofeedbackState(scope), recursive = TRUE, use.names = FALSE
  ))

  if (requireNamespace("shiny", quietly = TRUE)) {
    prepared <- PhysioStream:::.video_prepare_for_shiny(scope$video)
    expect_true(startsWith(prepared$source, "/physiostream-video-"))
    expect_true(dir.exists(prepared$cleanup$root))
    PhysioStream:::.video_cleanup_shiny(prepared)
    expect_false(dir.exists(prepared$cleanup$root))
  }

  expect_s3_class(
    videoSyncViewer(source, "https://example.org/video.mp4", sync,
                    launch = FALSE),
    "VideoSyncViewer"
  )
  expect_error(
    videoSyncViewer(source, "http://example.org/video.mp4", sync,
                    launch = FALSE),
    "regular non-symlink"
  )
  expect_error(
    videoSyncViewer(
      source, path,
      videoSync(0, 0, 30, "offset", "other-domain"),
      launch = FALSE
    ),
    "clock domain"
  )
})
