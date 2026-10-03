.closed_loop_schema <- "physiostream.closed-loop/1.0.0"
.closed_loop_event_schema <- "physiostream.closed-loop-event/1.0.0"
.closed_loop_log_limit_bytes <- 4096L

.closed_loop_abort <- function(message,
                               class = "PhysioStream_closed_loop_error") {
  .stream_abort(message, class)
}

.closed_loop_scalar <- function(x, name, lower = -Inf, upper = Inf,
                                integer = FALSE, lower_open = FALSE,
                                upper_open = FALSE) {
  valid <- !is.factor(x) && !is.object(x) && is.numeric(x) &&
    is.null(dim(x)) && length(x) == 1L && is.finite(x)
  valid <- valid && if (lower_open) x > lower else x >= lower
  valid <- valid && if (upper_open) x < upper else x <= upper
  if (integer) {
    valid <- valid && x == floor(x) && x <= .Machine$integer.max
  }
  if (!valid) {
    .closed_loop_abort(
      sprintf("`%s` has an invalid value", name),
      "PhysioStream_closed_loop_validation_error"
    )
  }
  if (integer) as.integer(x) else as.numeric(x)
}

.closed_loop_string <- function(x, name, max_bytes = 256L,
                                pattern = NULL) {
  valid <- !is.factor(x) && !is.object(x) && is.character(x) &&
    is.null(dim(x)) && length(x) == 1L && !is.na(x) && nzchar(x) &&
    nchar(x, type = "bytes") <= max_bytes
  if (valid && !is.null(pattern)) {
    valid <- grepl(pattern, x, perl = TRUE)
  }
  if (!valid) {
    .closed_loop_abort(
      sprintf("`%s` must be one valid non-empty string", name),
      "PhysioStream_closed_loop_validation_error"
    )
  }
  enc2utf8(x)
}

.closed_loop_detector_signature <- function(configuration) {
  digest::digest(
    serialize(configuration, NULL, version = 3L),
    algo = "sha256", serialize = FALSE
  )
}

.closed_loop_signal_times <- function(chunk, state) {
  sampling_rate <- state$configuration$sampling_rate
  times <- if (!is.null(chunk$timestamps)) {
    as.numeric(chunk$timestamps)
  } else {
    (as.numeric(chunk$sequence) - 1) / sampling_rate
  }
  expected <- 1 / sampling_rate
  tolerance <- max(
    1e-12,
    expected * state$configuration$timestamp_tolerance_fraction
  )
  intervals <- diff(times)
  if (!is.null(state$last_signal_timestamp)) {
    intervals <- c(
      times[[1L]] - state$last_signal_timestamp,
      intervals
    )
  }
  if (length(intervals) &&
      any(abs(intervals - expected) > tolerance)) {
    .closed_loop_abort(
      "detector timestamps are inconsistent with `sampling_rate`",
      "PhysioStream_closed_loop_timing_error"
    )
  }
  state$last_signal_timestamp <- tail(times, 1L)
  list(times = times, state = state)
}

.closed_loop_channel <- function(chunk, state) {
  names_now <- colnames(chunk$samples)
  if (is.null(state$channel_index)) {
    index <- match(state$configuration$channel, names_now)
    if (is.na(index)) {
      .closed_loop_abort(
        sprintf(
          "detector channel `%s` is absent",
          state$configuration$channel
        ),
        "PhysioStream_closed_loop_channel_error"
      )
    }
    state$channel_index <- as.integer(index)
    state$channel_names <- names_now
  } else if (!identical(names_now, state$channel_names) ||
             !identical(
               names_now[[state$channel_index]],
               state$configuration$channel
             )) {
    .closed_loop_abort(
      "detector channel identity changed",
      "PhysioStream_closed_loop_channel_error"
    )
  }
  list(
    values = as.numeric(chunk$samples[, state$channel_index]),
    state = state
  )
}

.closed_loop_event_id <- function(state, sequence) {
  digest::digest(
    paste(
      state$signature, state$generation, format(sequence, scientific = FALSE),
      state$event_count + 1, sep = ":"
    ),
    algo = "sha256", serialize = FALSE
  )
}

.closed_loop_event <- function(state, sequence, timestamp, score,
                               phase = NULL, predicted = NULL,
                               phase_error = NULL, amplitude = NULL,
                               fit = NULL) {
  event_id <- .closed_loop_event_id(state, sequence)
  value <- list(
    schema = .closed_loop_event_schema,
    detector = state$configuration$kind,
    detector_signature = state$signature,
    detector_event_id = event_id,
    detector_generation = state$generation,
    sample_index = as.numeric(sequence),
    signal_timestamp = as.numeric(timestamp),
    score = as.numeric(score),
    phase_degrees = phase,
    predicted_phase_degrees = predicted,
    phase_error_degrees = phase_error,
    amplitude = amplitude,
    fit = fit
  )
  payload <- unclass(jsonlite::toJSON(
    value, auto_unbox = TRUE, digits = NA, null = "null", pretty = FALSE
  ))
  if (nchar(payload, type = "bytes") > .closed_loop_log_limit_bytes) {
    .closed_loop_abort(
      "detector event exceeds its bounded payload",
      "PhysioStream_closed_loop_resource_error"
    )
  }
  list(
    event = list(
      timestamp = as.numeric(timestamp),
      type = "closed_loop_detection",
      value = payload
    ),
    event_id = event_id
  )
}

.closed_loop_empty_detector_state <- function(configuration) {
  list(
    configuration = configuration,
    signature = .closed_loop_detector_signature(configuration),
    generation = 1,
    channel_names = NULL,
    channel_index = NULL,
    last_signal_timestamp = NULL,
    n_samples = 0,
    event_count = 0,
    last_event_id = NULL,
    active = FALSE,
    on_count = 0L,
    off_count = 0L
  )
}

.closed_loop_rms_update <- function(state, value) {
  cursor <- state$rms_cursor %% state$configuration$rms_window_samples + 1L
  old <- state$rms_buffer[[cursor]]
  if (state$rms_filled < state$configuration$rms_window_samples) {
    state$rms_filled <- state$rms_filled + 1L
    old <- 0
  }
  state$rms_buffer[[cursor]] <- value
  state$rms_sum_squares <- state$rms_sum_squares - old^2 + value^2
  state$rms_cursor <- cursor
  list(
    value = sqrt(max(0, state$rms_sum_squares / state$rms_filled)),
    complete =
      state$rms_filled == state$configuration$rms_window_samples,
    state = state
  )
}

.closed_loop_emg_callback <- function(chunk, state, context) {
  bound <- .closed_loop_channel(chunk, state)
  state <- bound$state
  values <- bound$values
  timed <- .closed_loop_signal_times(chunk, state)
  state <- timed$state
  times <- timed$times
  events <- list()
  z_values <- rep(NA_real_, length(values))

  for (i in seq_along(values)) {
    state$n_samples <- state$n_samples + 1
    envelope <- .closed_loop_rms_update(state, values[[i]])
    state <- envelope$state
    if (!envelope$complete) {
      next
    }
    if (state$baseline_n < state$configuration$baseline_samples) {
      state$baseline_n <- state$baseline_n + 1L
      delta <- envelope$value - state$baseline_mean
      state$baseline_mean <- state$baseline_mean +
        delta / state$baseline_n
      state$baseline_m2 <- state$baseline_m2 +
        delta * (envelope$value - state$baseline_mean)
      if (state$baseline_n == state$configuration$baseline_samples) {
        state$baseline_sd <- sqrt(
          state$baseline_m2 / (state$baseline_n - 1L)
        )
        if (!is.finite(state$baseline_sd) ||
            state$baseline_sd < state$configuration$min_baseline_sd) {
          .closed_loop_abort(
            "EMG baseline scale is below `min_baseline_sd`",
            "PhysioStream_closed_loop_calibration_error"
          )
        }
      }
      next
    }

    z <- (envelope$value - state$baseline_mean) / state$baseline_sd
    z_values[[i]] <- z
    if (!state$active) {
      state$on_count <- if (z >= state$configuration$enter_z) {
        state$on_count + 1L
      } else {
        0L
      }
      if (state$on_count >= state$configuration$min_on_samples) {
        made <- .closed_loop_event(
          state, chunk$sequence[[i]], times[[i]], z
        )
        events[[length(events) + 1L]] <- made$event
        state$event_count <- state$event_count + 1
        state$last_event_id <- made$event_id
        state$active <- TRUE
        state$on_count <- 0L
        state$off_count <- 0L
      }
    } else {
      state$off_count <- if (z <= state$configuration$release_z) {
        state$off_count + 1L
      } else {
        0L
      }
      if (state$off_count >= state$configuration$min_off_samples) {
        state$active <- FALSE
        state$on_count <- 0L
        state$off_count <- 0L
      }
    }
  }

  list(
    output = chunk,
    state = state,
    events = events,
    diagnostics = list(
      detector = "emg_onset",
      calibrated =
        state$baseline_n == state$configuration$baseline_samples,
      active = state$active,
      score = z_values[is.finite(z_values)]
    )
  )
}

#' Construct a causal streaming EMG-onset detector
#'
#' The detector uses a trailing RMS envelope, a frozen warm-up baseline, and
#' explicit enter/release hysteresis. It is a pure [onChunk()] operation:
#' stimulation is attempted only by [closedLoopStep()] after pipeline commit.
#' Explicit signal-timestamp intervals must remain within five percent of the
#' declared sampling interval.
#'
#' @param channel Exact input channel name.
#' @param sampling_rate Sampling rate in hertz.
#' @param baseline_samples Number of complete-window envelope samples used for
#'   the frozen baseline.
#' @param rms_window_samples Trailing causal RMS window length.
#' @param enter_z,release_z Baseline-standardized enter/release thresholds.
#' @param min_on_samples,min_off_samples Consecutive confirmation counts.
#' @param min_baseline_sd Minimum accepted baseline scale.
#' @return A governed `PipelineOperation` detector descriptor.
#' @examples
#' set.seed(1)
#' emg <- c(rnorm(80, sd = 0.08), rep(1.5, 12), rnorm(30, sd = 0.08))
#' detector <- emgOnsetOp("emg", sampling_rate = 1000,
#'                        baseline_samples = 50, rms_window_samples = 8)
#' pipeline <- streamPipeline(chunk_size = length(emg))
#' onChunk(pipeline, detector)
#' pipelineEnqueue(pipeline,
#'                 matrix(emg, ncol = 1, dimnames = list(NULL, "emg")),
#'                 seq_along(emg) / 1000)
#' length(pipelineStep(pipeline)$results[[1]]$events)
#' @export
emgOnsetOp <- function(
    channel,
    sampling_rate,
    baseline_samples,
    rms_window_samples,
    enter_z = 5,
    release_z = 2,
    min_on_samples = 3L,
    min_off_samples = 3L,
    min_baseline_sd = 1e-8) {
  channel <- .closed_loop_string(channel, "channel")
  sampling_rate <- .closed_loop_scalar(
    sampling_rate, "sampling_rate", lower = 0, lower_open = TRUE,
    upper = 1e6
  )
  baseline_samples <- .closed_loop_scalar(
    baseline_samples, "baseline_samples", lower = 2, upper = 1e6,
    integer = TRUE
  )
  rms_window_samples <- .closed_loop_scalar(
    rms_window_samples, "rms_window_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  enter_z <- .closed_loop_scalar(enter_z, "enter_z", lower = 0)
  release_z <- .closed_loop_scalar(release_z, "release_z", lower = 0)
  if (enter_z <= release_z) {
    .closed_loop_abort(
      "`enter_z` must be greater than `release_z`",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  min_on_samples <- .closed_loop_scalar(
    min_on_samples, "min_on_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  min_off_samples <- .closed_loop_scalar(
    min_off_samples, "min_off_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  min_baseline_sd <- .closed_loop_scalar(
    min_baseline_sd, "min_baseline_sd", lower = 0, lower_open = TRUE
  )
  configuration <- list(
    kind = "emg_onset",
    channel = channel,
    sampling_rate = sampling_rate,
    baseline_samples = baseline_samples,
    rms_window_samples = rms_window_samples,
    enter_z = enter_z,
    release_z = release_z,
    min_on_samples = min_on_samples,
    min_off_samples = min_off_samples,
    min_baseline_sd = min_baseline_sd,
    timestamp_tolerance_fraction = 0.05
  )
  state <- .closed_loop_empty_detector_state(configuration)
  state$rms_buffer <- numeric(rms_window_samples)
  state$rms_cursor <- 0L
  state$rms_filled <- 0L
  state$rms_sum_squares <- 0
  state$baseline_n <- 0L
  state$baseline_mean <- 0
  state$baseline_m2 <- 0
  state$baseline_sd <- NULL
  structure(
    list(
      callback = .closed_loop_emg_callback,
      state = state,
      name = "closed_loop_emg_onset",
      kind = "detector",
      descriptor = list(
        type = "closed_loop_detector",
        configuration = configuration
      )
    ),
    class = "PipelineOperation"
  )
}

.closed_loop_erd_filter <- function(state, value) {
  cfs <- state$configuration$biquad
  output <- cfs$b0 * value + state$z1
  z1 <- cfs$b1 * value - cfs$a1 * output + state$z2
  z2 <- cfs$b2 * value - cfs$a2 * output
  state$z1 <- z1
  state$z2 <- z2
  list(value = output, state = state)
}

.closed_loop_power_update <- function(state, value) {
  cursor <- state$power_cursor %%
    state$configuration$power_window_samples + 1L
  old <- state$power_buffer[[cursor]]
  if (state$power_filled < state$configuration$power_window_samples) {
    state$power_filled <- state$power_filled + 1L
    old <- 0
  }
  square <- value^2
  state$power_buffer[[cursor]] <- square
  state$power_sum <- state$power_sum - old + square
  state$power_cursor <- cursor
  list(
    value = max(0, state$power_sum / state$power_filled),
    complete =
      state$power_filled == state$configuration$power_window_samples,
    state = state
  )
}

.closed_loop_erd_callback <- function(chunk, state, context) {
  bound <- .closed_loop_channel(chunk, state)
  state <- bound$state
  values <- bound$values
  timed <- .closed_loop_signal_times(chunk, state)
  state <- timed$state
  times <- timed$times
  events <- list()
  scores <- rep(NA_real_, length(values))

  for (i in seq_along(values)) {
    state$n_samples <- state$n_samples + 1
    filtered <- .closed_loop_erd_filter(state, values[[i]])
    state <- filtered$state
    power <- .closed_loop_power_update(state, filtered$value)
    state <- power$state
    if (!power$complete) {
      next
    }
    if (state$baseline_n < state$configuration$baseline_samples) {
      state$baseline_n <- state$baseline_n + 1L
      state$baseline_sum <- state$baseline_sum + power$value
      if (state$baseline_n == state$configuration$baseline_samples) {
        state$baseline_power <-
          state$baseline_sum / state$baseline_n
        if (!is.finite(state$baseline_power) ||
            state$baseline_power <
              state$configuration$min_baseline_power) {
          .closed_loop_abort(
            "EEG baseline power is below `min_baseline_power`",
            "PhysioStream_closed_loop_calibration_error"
          )
        }
      }
      next
    }

    score <- 100 *
      (power$value - state$baseline_power) / state$baseline_power
    scores[[i]] <- score
    if (!state$active) {
      state$on_count <- if (
          score <= state$configuration$enter_percent) {
        state$on_count + 1L
      } else {
        0L
      }
      if (state$on_count >= state$configuration$min_on_samples) {
        made <- .closed_loop_event(
          state, chunk$sequence[[i]], times[[i]], score
        )
        events[[length(events) + 1L]] <- made$event
        state$event_count <- state$event_count + 1
        state$last_event_id <- made$event_id
        state$active <- TRUE
        state$on_count <- 0L
        state$off_count <- 0L
      }
    } else {
      state$off_count <- if (
          score >= state$configuration$release_percent) {
        state$off_count + 1L
      } else {
        0L
      }
      if (state$off_count >= state$configuration$min_off_samples) {
        state$active <- FALSE
        state$on_count <- 0L
        state$off_count <- 0L
      }
    }
  }

  list(
    output = chunk,
    state = state,
    events = events,
    diagnostics = list(
      detector = "erd_intent",
      calibrated =
        state$baseline_n == state$configuration$baseline_samples,
      active = state$active,
      erd_percent = scores[is.finite(scores)]
    )
  )
}

#' Construct a causal streaming EEG ERD detector
#'
#' A fixed causal biquad and trailing power window estimate band-power change
#' from a frozen baseline. The result is an ERD proxy, not a clinical
#' movement-intention diagnosis. Explicit signal-timestamp intervals must
#' remain within five percent of the declared sampling interval.
#'
#' @param channel Exact input channel name.
#' @param sampling_rate Sampling rate in hertz.
#' @param band Two strictly increasing band edges in hertz.
#' @param baseline_samples Number of complete power windows in the baseline.
#' @param power_window_samples Trailing causal mean-square window length.
#' @param enter_percent,release_percent ERD enter/release percentages.
#' @param min_on_samples,min_off_samples Consecutive confirmation counts.
#' @param min_baseline_power Minimum accepted frozen baseline power.
#' @return A governed `PipelineOperation` detector descriptor.
#' @examples
#' detector <- erdIntentOp("eeg", sampling_rate = 1000, band = c(8, 13),
#'                         baseline_samples = 40, power_window_samples = 10)
#' class(detector)
#' @export
erdIntentOp <- function(
    channel,
    sampling_rate,
    band = c(8, 13),
    baseline_samples,
    power_window_samples,
    enter_percent = -30,
    release_percent = -15,
    min_on_samples = 3L,
    min_off_samples = 3L,
    min_baseline_power = 1e-12) {
  channel <- .closed_loop_string(channel, "channel")
  sampling_rate <- .closed_loop_scalar(
    sampling_rate, "sampling_rate", lower = 0, lower_open = TRUE,
    upper = 1e6
  )
  if (is.factor(band) || is.object(band) || !is.numeric(band) ||
      !is.null(dim(band)) || length(band) != 2L ||
      any(!is.finite(band)) || band[[1L]] <= 0 ||
      band[[2L]] <= band[[1L]] ||
      band[[2L]] >= sampling_rate / 2) {
    .closed_loop_abort(
      "`band` must contain two increasing positive values below Nyquist",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  band <- as.numeric(band)
  baseline_samples <- .closed_loop_scalar(
    baseline_samples, "baseline_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  power_window_samples <- .closed_loop_scalar(
    power_window_samples, "power_window_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  enter_percent <- .closed_loop_scalar(
    enter_percent, "enter_percent", upper = 0
  )
  release_percent <- .closed_loop_scalar(
    release_percent, "release_percent", upper = 0
  )
  if (enter_percent >= release_percent) {
    .closed_loop_abort(
      "`enter_percent` must be less than `release_percent`",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  min_on_samples <- .closed_loop_scalar(
    min_on_samples, "min_on_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  min_off_samples <- .closed_loop_scalar(
    min_off_samples, "min_off_samples", lower = 1, upper = 1e6,
    integer = TRUE
  )
  min_baseline_power <- .closed_loop_scalar(
    min_baseline_power, "min_baseline_power", lower = 0,
    lower_open = TRUE
  )

  centre <- sqrt(band[[1L]] * band[[2L]])
  q <- centre / (band[[2L]] - band[[1L]])
  omega <- 2 * pi * centre / sampling_rate
  alpha <- sin(omega) / (2 * q)
  a0 <- 1 + alpha
  biquad <- list(
    b0 = alpha / a0,
    b1 = 0,
    b2 = -alpha / a0,
    a1 = -2 * cos(omega) / a0,
    a2 = (1 - alpha) / a0
  )
  configuration <- list(
    kind = "erd_intent",
    channel = channel,
    sampling_rate = sampling_rate,
    band = band,
    baseline_samples = baseline_samples,
    power_window_samples = power_window_samples,
    enter_percent = enter_percent,
    release_percent = release_percent,
    min_on_samples = min_on_samples,
    min_off_samples = min_off_samples,
    min_baseline_power = min_baseline_power,
    biquad = biquad,
    timestamp_tolerance_fraction = 0.05
  )
  state <- .closed_loop_empty_detector_state(configuration)
  state$z1 <- 0
  state$z2 <- 0
  state$power_buffer <- numeric(power_window_samples)
  state$power_cursor <- 0L
  state$power_filled <- 0L
  state$power_sum <- 0
  state$baseline_n <- 0L
  state$baseline_sum <- 0
  state$baseline_power <- NULL
  structure(
    list(
      callback = .closed_loop_erd_callback,
      state = state,
      name = "closed_loop_erd_intent",
      kind = "detector",
      descriptor = list(
        type = "closed_loop_detector",
        configuration = configuration
      )
    ),
    class = "PipelineOperation"
  )
}
