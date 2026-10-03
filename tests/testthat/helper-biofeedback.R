biofeedback_test_info <- function(
    channels = c("left", "right"),
    rate = 100,
    units = rep("uV", length(channels)),
    clock_domain = "biofeedback-fixture") {
  streamInfo(
    "biofeedback-fixture",
    type = "synthetic",
    channel_names = channels,
    nominal_srate = rate,
    dtype = "float64",
    source_id = "biofeedback-fixture",
    clock_domain = clock_domain,
    channel_units = units
  )
}

biofeedback_test_source <- function(
    channels = c("left", "right"),
    rate = 100,
    capacity = 4096L,
    open = TRUE,
    units = rep("uV", length(channels))) {
  source <- loopbackSource(
    biofeedback_test_info(channels, rate, units), capacity
  )
  if (open) streamOpen(source) else source
}

biofeedback_test_clock <- function() {
  function() cpp_monotonic_ns()
}

biofeedback_test_feed <- function(source, n, start = 0, rate = NULL) {
  info <- streamInfo(source)
  if (is.null(rate)) {
    rate <- info@nominal_srate
  }
  samples <- matrix(
    as.double(seq_len(n * info@n_channels)),
    nrow = n,
    ncol = info@n_channels
  )
  timestamps <- start + (seq_len(n) - 1) / rate
  loopbackFeed(source, samples, timestamps)
  list(samples = samples, timestamps = timestamps)
}

biofeedback_test_pipeline <- function(source, window_samples = 4L) {
  pipeline <- streamPipeline(
    source = source,
    chunk_size = 8L,
    queue_capacity = 16L,
    backpressure = "error",
    latency_budget_ms = 1000
  )
  sos <- matrix(
    c(1, 0, 0, 1, 0, 0), 1L, 6L,
    dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2"))
  )
  onChunk(
    pipeline,
    bandpassRmsOp(
      sos, window_samples, warmup = "partial", name = "fixture_rms"
    )
  )
  pipeline
}
