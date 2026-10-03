closed_loop_trigger <- function(refractory_ms = 0, deadman_ms = 1000,
                                clock = NULL) {
  loopbackTrigger(
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = refractory_ms,
    deadman_ms = deadman_ms,
    audit_capacity = 256L,
    clock = clock
  )
}

closed_loop_emg <- function(seed = 101L) {
  set.seed(seed)
  c(
    stats::rnorm(80, sd = 0.08),
    rep(1.5, 12),
    stats::rnorm(30, sd = 0.08),
    rep(1.5, 12),
    stats::rnorm(20, sd = 0.08)
  )
}

run_detector_partitions <- function(detector, values, partitions,
                                    channel, sampling_rate) {
  pipeline <- streamPipeline(
    chunk_size = length(values),
    queue_capacity = max(2L, length(partitions))
  )
  onChunk(pipeline, detector)
  start <- 1L
  events <- list()
  for (size in partitions) {
    index <- seq.int(start, length.out = size)
    samples <- matrix(
      values[index], ncol = 1L, dimnames = list(NULL, channel)
    )
    timestamps <- (index - 1) / sampling_rate
    pipelineEnqueue(pipeline, samples, timestamps)
    result <- pipelineStep(pipeline)
    events <- c(events, result$results[[1L]]$events)
    start <- start + size
  }
  list(events = events, state = pipelineState(pipeline))
}

closed_loop_controller <- function(detector, delay_ms = 0,
                                   refractory_ms = 0,
                                   trigger_refractory_ms = 0,
                                   pending_capacity = 128L,
                                   trigger = NULL) {
  if (is.null(trigger)) {
    trigger <- closed_loop_trigger(trigger_refractory_ms)
  }
  pipeline <- streamPipeline(chunk_size = 2048L)
  controller <- closedLoop(
    pipeline,
    trigger,
    detector,
    intensity = 2,
    stim_channel = "left",
    duration_ms = 10,
    delay_ms = delay_ms,
    event_refractory_ms = refractory_ms,
    pending_capacity = pending_capacity,
    log_capacity = 256L
  )
  list(controller = controller, pipeline = pipeline, trigger = trigger)
}
