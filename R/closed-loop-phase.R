.closed_loop_wrap_degrees <- function(x) {
  out <- x %% 360
  out[out < 0] <- out[out < 0] + 360
  out
}

.closed_loop_signed_degrees <- function(x) {
  out <- (x + 180) %% 360 - 180
  out[out == -180 & x > 0] <- 180
  out
}

.closed_loop_phase_fit <- function(values, times, frequency) {
  omega <- 2 * pi * frequency
  design <- cbind(
    offset = 1,
    cosine = cos(omega * times),
    sine = sin(omega * times)
  )
  fit <- stats::lm.fit(design, values)
  if (fit$rank != 3L || any(!is.finite(fit$coefficients))) {
    return(NULL)
  }
  residual <- as.numeric(fit$residuals)
  centred <- values - mean(values)
  sst <- sum(centred^2)
  quality <- if (sst > 0) {
    max(0, min(1, 1 - sum(residual^2) / sst))
  } else {
    0
  }
  list(
    offset = unname(fit$coefficients[[1L]]),
    cosine = unname(fit$coefficients[[2L]]),
    sine = unname(fit$coefficients[[3L]]),
    amplitude = sqrt(
      fit$coefficients[[2L]]^2 + fit$coefficients[[3L]]^2
    ),
    fit = quality
  )
}

.closed_loop_phase_callback <- function(chunk, state, context) {
  bound <- .closed_loop_channel(chunk, state)
  state <- bound$state
  values <- bound$values
  timed <- .closed_loop_signal_times(chunk, state)
  state <- timed$state
  absolute_times <- timed$times
  events <- list()
  phases <- rep(NA_real_, length(values))
  errors <- rep(NA_real_, length(values))
  fits <- rep(NA_real_, length(values))
  amplitudes <- rep(NA_real_, length(values))

  for (i in seq_along(values)) {
    state$n_samples <- state$n_samples + 1
    if (is.null(state$time_origin)) {
      state$time_origin <- absolute_times[[i]]
    }
    relative_time <- absolute_times[[i]] - state$time_origin
    if (!is.finite(relative_time) ||
        (length(state$times) && relative_time <= tail(state$times, 1L))) {
      .closed_loop_abort(
        "phase detector signal time did not increase",
        "PhysioStream_closed_loop_timing_error"
      )
    }
    state$values <- c(state$values, values[[i]])
    state$times <- c(state$times, relative_time)
    if (length(state$values) > state$configuration$window_samples) {
      state$values <- tail(
        state$values, state$configuration$window_samples
      )
      state$times <- tail(
        state$times, state$configuration$window_samples
      )
    }
    if (length(state$values) < state$configuration$window_samples) {
      next
    }

    fitted <- .closed_loop_phase_fit(
      state$values, state$times, state$configuration$frequency
    )
    valid <- !is.null(fitted) &&
      fitted$amplitude >= state$configuration$min_amplitude &&
      fitted$fit >= state$configuration$min_fit
    if (!valid) {
      state$inside_target <- FALSE
      next
    }
    omega <- 2 * pi * state$configuration$frequency
    phase <- .closed_loop_wrap_degrees(
      atan2(-fitted$sine, fitted$cosine) * 180 / pi +
        omega * relative_time * 180 / pi
    )
    predicted <- .closed_loop_wrap_degrees(
      phase +
        360 * state$configuration$frequency *
          state$configuration$prediction_delay_ms / 1000
    )
    error <- .closed_loop_signed_degrees(
      predicted - state$configuration$target_degrees
    )
    inside <- abs(error) <= state$configuration$tolerance_degrees
    phases[[i]] <- phase
    errors[[i]] <- error
    fits[[i]] <- fitted$fit
    amplitudes[[i]] <- fitted$amplitude

    if (inside && !state$inside_target) {
      made <- .closed_loop_event(
        state,
        chunk$sequence[[i]],
        absolute_times[[i]],
        score = fitted$fit,
        phase = phase,
        predicted = predicted,
        phase_error = error,
        amplitude = fitted$amplitude,
        fit = fitted$fit
      )
      events[[length(events) + 1L]] <- made$event
      state$event_count <- state$event_count + 1
      state$last_event_id <- made$event_id
    }
    state$inside_target <- inside
  }

  list(
    output = chunk,
    state = state,
    events = events,
    diagnostics = list(
      detector = "phase_target",
      phase_degrees = phases[is.finite(phases)],
      phase_error_degrees = errors[is.finite(errors)],
      amplitude = amplitudes[is.finite(amplitudes)],
      fit = fits[is.finite(fits)]
    )
  )
}

.closed_loop_bind_delay <- function(detector, delay_ms) {
  detector <- .dsp_deep_copy(detector)
  if (!identical(
      detector$descriptor$configuration$kind,
      "phase_target"
  )) {
    return(detector)
  }
  detector$descriptor$configuration$prediction_delay_ms <- delay_ms
  detector$state$configuration$prediction_delay_ms <- delay_ms
  detector$state$signature <- .closed_loop_detector_signature(
    detector$state$configuration
  )
  detector
}

#' Construct a causal phase-target detector
#'
#' The detector fits a sinusoid at a fixed frequency using only the trailing
#' window ending at each current sample. [closedLoop()] binds its configured
#' detection-to-stimulation delay before registration so the event reports the
#' predicted phase at the due time. Explicit signal-timestamp intervals must
#' remain within five percent of the declared sampling interval.
#'
#' @param channel Exact input channel name.
#' @param sampling_rate Sampling rate in hertz.
#' @param frequency Target oscillation frequency in hertz.
#' @param target_degrees Requested phase in `[0, 360)`, modulo 360.
#' @param window_cycles Number of trailing cycles in the causal fit.
#' @param tolerance_degrees Maximum accepted predicted signed phase error.
#' @param min_amplitude Minimum fitted oscillation amplitude.
#' @param min_fit Minimum coefficient of determination for the sinusoid fit.
#' @return A governed `PipelineOperation` detector descriptor.
#' @examples
#' sr <- 500
#' t <- seq_len(800) / sr
#' eeg <- 1.7 + 2.5 * cos(2 * pi * 9 * t + 0.7)
#' detector <- phaseTargetOp("eeg", sampling_rate = sr, frequency = 9,
#'                           target_degrees = 90)
#' pipeline <- streamPipeline(chunk_size = length(eeg))
#' onChunk(pipeline, detector)
#' pipelineEnqueue(pipeline,
#'                 matrix(eeg, ncol = 1, dimnames = list(NULL, "eeg")), t)
#' length(pipelineStep(pipeline)$results[[1]]$events)
#' @export
phaseTargetOp <- function(
    channel,
    sampling_rate,
    frequency,
    target_degrees,
    window_cycles = 3,
    tolerance_degrees = 20,
    min_amplitude = 0,
    min_fit = 0.8) {
  channel <- .closed_loop_string(channel, "channel")
  sampling_rate <- .closed_loop_scalar(
    sampling_rate, "sampling_rate", lower = 0, lower_open = TRUE,
    upper = 1e6
  )
  frequency <- .closed_loop_scalar(
    frequency, "frequency", lower = 0, lower_open = TRUE,
    upper = sampling_rate / 2, upper_open = TRUE
  )
  target_degrees <- .closed_loop_scalar(
    target_degrees, "target_degrees", lower = -1e12, upper = 1e12
  )
  target_degrees <- .closed_loop_wrap_degrees(target_degrees)
  window_cycles <- .closed_loop_scalar(
    window_cycles, "window_cycles", lower = 1, upper = 1000
  )
  tolerance_degrees <- .closed_loop_scalar(
    tolerance_degrees, "tolerance_degrees", lower = 0, lower_open = TRUE,
    upper = 20
  )
  min_amplitude <- .closed_loop_scalar(
    min_amplitude, "min_amplitude", lower = 0
  )
  min_fit <- .closed_loop_scalar(
    min_fit, "min_fit", lower = 0, upper = 1
  )
  window_samples <- ceiling(window_cycles * sampling_rate / frequency)
  if (!is.finite(window_samples) || window_samples < 8L ||
      window_samples > 8192L) {
    .closed_loop_abort(
      "phase trailing window must contain 8 to 8192 samples",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  configuration <- list(
    kind = "phase_target",
    channel = channel,
    sampling_rate = sampling_rate,
    frequency = frequency,
    target_degrees = target_degrees,
    window_cycles = window_cycles,
    window_samples = as.integer(window_samples),
    tolerance_degrees = tolerance_degrees,
    min_amplitude = min_amplitude,
    min_fit = min_fit,
    prediction_delay_ms = 0,
    timestamp_tolerance_fraction = 0.05
  )
  state <- .closed_loop_empty_detector_state(configuration)
  state$values <- numeric()
  state$times <- numeric()
  state$time_origin <- NULL
  state$inside_target <- FALSE
  structure(
    list(
      callback = .closed_loop_phase_callback,
      state = state,
      name = "closed_loop_phase_target",
      kind = "detector",
      descriptor = list(
        type = "closed_loop_detector",
        configuration = configuration
      )
    ),
    class = "PipelineOperation"
  )
}
