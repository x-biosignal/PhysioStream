test_that("controller construction is transport side-effect free", {
  trigger <- closed_loop_trigger()
  before <- triggerState(trigger)
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 30, 5),
    trigger = trigger
  )
  expect_s3_class(setup$controller, "ClosedLoopController")
  expect_identical(triggerState(trigger), before)
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "constructed"
  )
  expect_match(capture.output(print(setup$controller)), "constructed")
})

test_that("controller validates detector, dose, capacity, and clock", {
  pipeline <- streamPipeline()
  trigger <- closed_loop_trigger()
  detector <- emgOnsetOp("emg", 1000, 30, 5)
  expect_error(
    closedLoop(pipeline, trigger, list(), 1, "left", 10),
    "detector"
  )
  expect_error(
    closedLoop(streamPipeline(), closed_loop_trigger(), detector,
               21, "left", 10),
    "intensity"
  )
  expect_error(
    closedLoop(streamPipeline(), closed_loop_trigger(), detector,
               1, "right", 10),
    "stim_channel"
  )
  expect_error(
    closedLoop(streamPipeline(), closed_loop_trigger(), detector,
               1, "left", 501),
    "duration"
  )
  expect_error(
    closedLoop(streamPipeline(), closed_loop_trigger(), detector,
               1, "left", 10, delay_ms = 1 / 3),
    "exact"
  )
  expect_error(
    closedLoop(streamPipeline(), closed_loop_trigger(), detector,
               1, "left", 10, pending_capacity = 0),
    "pending_capacity"
  )
  callback_trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = function(x) list(ok = TRUE, ack_code = "ok")
  )
  expect_error(
    closedLoop(
      streamPipeline(), callback_trigger, detector, 1, "left", 10,
      clock = function() 0
    ),
    "only with LoopbackTrigger"
  )
})

test_that("controller stimulates only after a committed EMG event", {
  values <- closed_loop_emg()
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, baseline_samples = 50, rms_window_samples = 8,
      enter_z = 6, release_z = 2, min_on_samples = 3
    )
  )
  closedLoopStart(setup$controller, "emg-session", now_ns = 0)
  expect_identical(triggerState(setup$trigger)$arm$status, "armed")
  result <- closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "emg")),
    timestamps = (seq_along(values) - 1) / 1000,
    now_ns = 1e6
  )
  expect_length(result$detections, 2L)
  expect_length(result$actions, 2L)
  expect_true(all(vapply(
    result$actions, function(x) identical(x$status, "acknowledged"),
    logical(1)
  )))
  state <- closedLoopState(setup$controller)
  expect_equal(state$counters$attempted, 2)
  expect_equal(state$counters$acknowledged, 2)
  expect_equal(state$counters$unknown, 0)
  expect_equal(triggerState(setup$trigger)$counters$attempted, 2)
  closedLoopStop(setup$controller, now_ns = 2e6)
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "stopped"
  )
  expect_identical(triggerState(setup$trigger)$lifecycle$status, "closed")
})

test_that("positive delay uses an explicit due queue and exact boundary", {
  values <- closed_loop_emg()
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, baseline_samples = 50, rms_window_samples = 8,
      enter_z = 6, release_z = 2, min_on_samples = 3
    ),
    delay_ms = 10
  )
  closedLoopStart(setup$controller, "delayed-session", now_ns = 0)
  result <- closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "emg")),
    now_ns = 1e6
  )
  expect_length(result$actions, 0L)
  expect_length(closedLoopState(setup$controller)$pending, 2L)
  early <- closedLoopFlush(setup$controller, now_ns = 10999999)
  expect_length(early$actions, 0L)
  exact <- closedLoopFlush(setup$controller, now_ns = 11e6)
  expect_length(exact$actions, 2L)
  expect_equal(exact$n_pending, 0L)
  expect_equal(
    vapply(exact$actions, `[[`, numeric(1), "attempted_at_ns"),
    c(11e6, 11e6)
  )
  closedLoopStop(setup$controller, now_ns = 12e6)
})

test_that("pipeline failure occurs before every stimulation attempt", {
  pipeline <- streamPipeline(chunk_size = 512L)
  onChunk(
    pipeline,
    function(chunk, state, context) {
      stop("deliberate upstream rollback")
    },
    state = list(),
    name = "deliberate_failure",
    kind = "filter"
  )
  trigger <- closed_loop_trigger()
  controller <- closedLoop(
    pipeline,
    trigger,
    emgOnsetOp("emg", 1000, 20, 5, min_on_samples = 2),
    1, "left", 10
  )
  closedLoopStart(controller, "rollback-session", now_ns = 0)
  before_trigger <- triggerState(trigger)$counters$attempted
  expect_error(
    closedLoopStep(
      controller,
      matrix(closed_loop_emg(), ncol = 1L,
             dimnames = list(NULL, "emg")),
      now_ns = 1e6
    ),
    "before stimulation"
  )
  expect_equal(triggerState(trigger)$counters$attempted, before_trigger)
  expect_identical(
    closedLoopState(controller)$lifecycle$status,
    "error_stopped"
  )
})

test_that("controller ownership mutation fails closed before dispatch", {
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5, min_on_samples = 2)
  )
  closedLoopStart(setup$controller, "owned-session", now_ns = 0)
  onChunk(
    setup$pipeline,
    function(chunk, state, context) {
      list(output = chunk, state = state, events = list(),
           diagnostics = list())
    },
    state = list(),
    name = "external_mutation"
  )
  expect_error(
    closedLoopFlush(setup$controller, now_ns = 1e6),
    "ownership changed"
  )
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "error_stopped"
  )
  expect_identical(triggerState(setup$trigger)$lifecycle$status, "closed")
  expect_equal(triggerState(setup$trigger)$counters$attempted, 0)
})

test_that("forged events from another operation are never governed actions", {
  forged <- new.env(parent = emptyenv())
  forged$signature <- NULL
  pipeline <- streamPipeline(chunk_size = 20L)
  onChunk(
    pipeline,
    function(chunk, state, context) {
      payload <- unclass(jsonlite::toJSON(
        list(
          schema = "physiostream.closed-loop-event/1.0.0",
          detector = "emg_onset",
          detector_signature = forged$signature,
          detector_event_id = paste(rep("a", 64), collapse = ""),
          detector_generation = 1,
          sample_index = chunk$sequence[[1L]],
          signal_timestamp = chunk$timestamps[[1L]],
          score = 100,
          phase_degrees = NULL,
          predicted_phase_degrees = NULL,
          phase_error_degrees = NULL,
          amplitude = NULL,
          fit = NULL
        ),
        auto_unbox = TRUE, digits = NA, null = "null"
      ))
      list(
        output = chunk,
        state = state,
        events = list(list(
          timestamp = chunk$timestamps[[1L]],
          type = "closed_loop_detection",
          value = payload
        )),
        diagnostics = list()
      )
    },
    state = list(),
    name = "untrusted_event_source",
    kind = "detector"
  )
  trigger <- closed_loop_trigger()
  controller <- closedLoop(
    pipeline, trigger,
    emgOnsetOp("emg", 1000, baseline_samples = 50,
               rms_window_samples = 5),
    1, "left", 10
  )
  forged$signature <- closedLoopState(controller)$detector_signature
  closedLoopStart(controller, "forgery-session", now_ns = 0)
  result <- closedLoopStep(
    controller,
    matrix(stats::rnorm(20), ncol = 1L,
           dimnames = list(NULL, "emg")),
    timestamps = (0:19) / 1000,
    now_ns = 1e6
  )
  expect_length(result$detections, 0L)
  expect_length(result$actions, 0L)
  expect_equal(triggerState(trigger)$counters$attempted, 0)
  closedLoopStop(controller, now_ns = 2e6)
})

test_that("controller refractory suppresses repeated committed detections", {
  sampling_rate <- 500
  time <- (0:499) / sampling_rate
  values <- 3 * cos(2 * pi * 10 * time)
  setup <- closed_loop_controller(
    phaseTargetOp(
      "eeg", sampling_rate, 10, 90,
      window_cycles = 2, tolerance_degrees = 20,
      min_amplitude = 1, min_fit = 0.99
    ),
    refractory_ms = 1000
  )
  closedLoopStart(setup$controller, "refractory-session", now_ns = 0)
  result <- closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "eeg")),
    timestamps = time,
    now_ns = 1e6
  )
  expect_length(result$detections, 1L)
  expect_length(result$actions, 1L)
  state <- closedLoopState(setup$controller)
  expect_gt(state$counters$suppressed_refractory, 1)
  closedLoopStop(setup$controller, now_ns = 2e6)
})

test_that("dead-man expiry stops without an attempt", {
  trigger <- closed_loop_trigger(deadman_ms = 5)
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5),
    trigger = trigger
  )
  closedLoopStart(setup$controller, "deadman-session", now_ns = 0)
  expect_error(
    closedLoopFlush(setup$controller, now_ns = 5000001),
    "heartbeat failed"
  )
  expect_equal(triggerState(trigger)$counters$attempted, 0)
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "error_stopped"
  )
})

test_that("normal and emergency stop suppress pending actions", {
  values <- closed_loop_emg()
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, 50, 8, enter_z = 6, min_on_samples = 3
    ),
    delay_ms = 100
  )
  closedLoopStart(setup$controller, "stop-session", now_ns = 0)
  closedLoopStep(
    setup$controller,
    matrix(values, ncol = 1L, dimnames = list(NULL, "emg")),
    now_ns = 1e6
  )
  expect_gt(length(closedLoopState(setup$controller)$pending), 0L)
  closedLoopEmergencyStop(
    setup$controller, reason = "test_stop", now_ns = 2e6
  )
  state <- closedLoopState(setup$controller)
  expect_identical(state$lifecycle$status, "emergency_stopped")
  expect_length(state$pending, 0L)
  expect_gt(state$counters$suppressed_stop, 0)
  expect_equal(triggerState(setup$trigger)$counters$attempted, 0)
})

test_that("uncertain transport outcome is final and fails closed", {
  writer <- function(request) {
    if (identical(request$action, "stop")) {
      return(list(ok = TRUE, ack_code = "stopped"))
    }
    list(ok = FALSE, ack_code = "negative_ack")
  }
  trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = writer
  )
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, 50, 8, enter_z = 6, min_on_samples = 3
    ),
    trigger = trigger
  )
  closedLoopStart(setup$controller, "unknown-session", now_ns = 0)
  result <- closedLoopStep(
    setup$controller,
    matrix(closed_loop_emg(), ncol = 1L,
           dimnames = list(NULL, "emg")),
    now_ns = 1e6
  )
  expect_length(result$actions, 1L)
  expect_identical(result$actions[[1L]]$status, "unknown")
  state <- closedLoopState(setup$controller)
  expect_identical(state$lifecycle$status, "error_stopped")
  expect_equal(state$counters$attempted, 1)
  expect_equal(state$counters$unknown, 1)
  expect_equal(triggerState(trigger)$counters$attempted, 1)
  expect_error(
    closedLoopFlush(setup$controller, now_ns = 2e6),
    "running"
  )
  expect_equal(triggerState(trigger)$counters$attempted, 1)
})

test_that("caller interrupt is audited unknown before re-signalling", {
  writer <- function(request) {
    if (identical(request$action, "stop")) {
      return(list(ok = TRUE, ack_code = "stopped"))
    }
    stop(structure(
      list(message = "caller interrupt"),
      class = c("interrupt", "condition")
    ))
  }
  trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = writer
  )
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, 50, 8, enter_z = 6, min_on_samples = 3
    ),
    trigger = trigger
  )
  closedLoopStart(setup$controller, "interrupt-session", now_ns = 0)
  condition <- tryCatch(
    {
      closedLoopStep(
        setup$controller,
        matrix(closed_loop_emg(), ncol = 1L,
               dimnames = list(NULL, "emg")),
        now_ns = 1e6
      )
      NULL
    },
    interrupt = identity
  )
  expect_s3_class(condition, "interrupt")
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "error_stopped"
  )
  expect_equal(closedLoopState(setup$controller)$counters$unknown, 1)
  expect_equal(triggerState(trigger)$counters$unknown, 1)
  expect_equal(triggerState(trigger)$counters$attempted, 1)
})

test_that("pending capacity overflow stops before the overflowing action", {
  sampling_rate <- 500
  time <- (0:499) / sampling_rate
  setup <- closed_loop_controller(
    phaseTargetOp(
      "eeg", sampling_rate, 10, 90,
      window_cycles = 2, tolerance_degrees = 20,
      min_amplitude = 1, min_fit = 0.99
    ),
    delay_ms = 100,
    pending_capacity = 1L
  )
  closedLoopStart(setup$controller, "capacity-session", now_ns = 0)
  expect_error(
    closedLoopStep(
      setup$controller,
      matrix(
        3 * cos(2 * pi * 10 * time),
        ncol = 1L, dimnames = list(NULL, "eeg")
      ),
      timestamps = time,
      now_ns = 1e6
    ),
    "pending capacity"
  )
  state <- closedLoopState(setup$controller)
  expect_identical(state$lifecycle$status, "error_stopped")
  expect_equal(state$counters$suppressed_capacity, 1)
  expect_equal(triggerState(setup$trigger)$counters$attempted, 0)
})

test_that("runtime mutation during trigger start is stopped and rejected", {
  holder <- new.env(parent = emptyenv())
  holder$mutated <- FALSE
  holder$controller <- NULL
  writer <- function(request) {
    if (!holder$mutated && !is.null(holder$controller)) {
      holder$controller$state$lifecycle$status <- "forged"
      holder$mutated <- TRUE
    }
    list(ok = TRUE, ack_code = "stopped")
  }
  trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = writer
  )
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5),
    trigger = trigger
  )
  holder$controller <- setup$controller
  expect_error(
    closedLoopStart(setup$controller, "mutated-start", now_ns = 0),
    "runtime changed during start"
  )
  expect_identical(
    closedLoopState(setup$controller)$lifecycle$status,
    "error_stopped"
  )
  expect_identical(triggerState(trigger)$lifecycle$status, "closed")
  expect_equal(triggerState(trigger)$counters$attempted, 0)
})

test_that("stop deassert failure is surfaced and remains terminal", {
  log <- new.env(parent = emptyenv())
  log$stops <- 0L
  writer <- function(request) {
    if (identical(request$action, "stop")) {
      log$stops <- log$stops + 1L
      return(list(
        ok = log$stops == 1L,
        ack_code = if (log$stops == 1L) "opened_low" else "stop_failed"
      ))
    }
    now <- request$command$issued_monotonic_ns
    list(
      ok = TRUE,
      ack_code = "pulse",
      assert_time_ns = now,
      deassert_time_ns = now + 1,
      ack_time_ns = now + 1
    )
  }
  trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = writer
  )
  setup <- closed_loop_controller(
    emgOnsetOp("emg", 1000, 20, 5),
    trigger = trigger
  )
  closedLoopStart(setup$controller, "failed-stop", now_ns = 0)
  expect_error(
    closedLoopStop(setup$controller, now_ns = 1e6),
    "deassert/close failure"
  )
  state <- closedLoopState(setup$controller)
  expect_identical(state$lifecycle$status, "error_stopped")
  expect_identical(
    state$lifecycle$last_error_code,
    "trigger_close_failed"
  )
  expect_identical(triggerState(trigger)$lifecycle$status, "closed")
})

test_that("transport callback mutation is audited after one final attempt", {
  holder <- new.env(parent = emptyenv())
  holder$controller <- NULL
  writer <- function(request) {
    if (identical(request$action, "stop")) {
      return(list(ok = TRUE, ack_code = "stopped"))
    }
    holder$controller$state$lifecycle$status <- "forged"
    holder$controller$runtime$clock <- function() 99
    now <- request$command$issued_monotonic_ns
    list(
      ok = TRUE,
      ack_code = "pulse",
      assert_time_ns = now,
      deassert_time_ns = now + 1,
      ack_time_ns = now + 1
    )
  }
  trigger <- ttlTrigger(
    transport = "callback",
    allowed_channels = "left",
    max_intensity = 20,
    intensity_unit = "mA",
    max_duration_ms = 500,
    refractory_ms = 0,
    deadman_ms = 1000,
    writer = writer
  )
  setup <- closed_loop_controller(
    emgOnsetOp(
      "emg", 1000, 50, 8, enter_z = 6, min_on_samples = 3
    ),
    trigger = trigger
  )
  holder$controller <- setup$controller
  closedLoopStart(setup$controller, "transport-mutation", now_ns = 0)
  result <- closedLoopStep(
    setup$controller,
    matrix(closed_loop_emg(), ncol = 1L,
           dimnames = list(NULL, "emg")),
    now_ns = 1e6
  )
  expect_length(result$actions, 1L)
  expect_identical(result$actions[[1L]]$status, "acknowledged")
  state <- closedLoopState(setup$controller)
  expect_identical(state$lifecycle$status, "error_stopped")
  expect_match(
    state$lifecycle$last_error_code,
    "controller_runtime_mutation"
  )
  expect_equal(state$counters$attempted, 1)
  expect_equal(triggerState(trigger)$counters$attempted, 1)
  expect_identical(triggerState(trigger)$lifecycle$status, "closed")
})
