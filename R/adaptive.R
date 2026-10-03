.adaptive_constructor <- function(algorithm, n_taps, step_size = NULL,
                                  forgetting = NULL, epsilon = NULL,
                                  leakage = 0, delta = NULL,
                                  initial_weights = NULL) {
  n_taps <- .dsp_scalar(
    n_taps, "n_taps", 1, 65536, integer = TRUE
  )
  leakage <- .dsp_scalar(
    leakage, "leakage", 0, 1, upper_open = TRUE
  )
  if (algorithm %in% c("lms", "nlms")) {
    step_size <- .dsp_scalar(
      step_size, "step_size", 0, Inf, lower_open = TRUE
    )
  }
  if (algorithm == "nlms") {
    epsilon <- .dsp_scalar(
      epsilon, "epsilon", 0, Inf, lower_open = TRUE
    )
  }
  if (algorithm == "rls") {
    forgetting <- .dsp_scalar(
      forgetting, "forgetting", 0, 1, lower_open = TRUE
    )
    delta <- .dsp_scalar(delta, "delta", 0, Inf, lower_open = TRUE)
  }
  if (!is.null(initial_weights)) {
    initial_weights <- .dsp_normalize_matrix(
      initial_weights, "initial_weights"
    )
    if (nrow(initial_weights) != n_taps) {
      .dsp_abort(
        "`initial_weights` must have exactly `n_taps` rows",
        "PhysioStream_dsp_validation_error"
      )
    }
  }
  config <- list(
    algorithm = algorithm,
    n_taps = n_taps,
    step_size = step_size,
    forgetting = forgetting,
    epsilon = epsilon,
    leakage = leakage,
    delta = delta,
    initial_weights = initial_weights
  )
  state <- list(
    algorithm = algorithm,
    config = config,
    channel_names = NULL,
    reference_names = NULL,
    n_samples = 0,
    n_chunks = 0,
    reset_count = 0,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    weights = NULL,
    history = NULL,
    covariance = NULL,
    residual_ss = NULL,
    reference_ss = NULL
  )
  resetter <- function(current, keep_channels) {
    channels <- if (keep_channels) current$channel_names else NULL
    references <- if (keep_channels) current$reference_names else NULL
    n_channels <- if (is.null(channels)) NULL else length(channels)
    weights <- NULL
    if (!is.null(n_channels)) {
      initial <- current$config$initial_weights
      if (is.null(initial)) {
        weights <- matrix(0, current$config$n_taps, n_channels)
      } else {
        if (ncol(initial) != n_channels) {
          .dsp_abort(
            "initial weights do not match retained channel count",
            "PhysioStream_dsp_state_error"
          )
        }
        weights <- initial
      }
    }
    covariance <- NULL
    if (identical(current$algorithm, "rls") && !is.null(n_channels)) {
      covariance <- array(
        0, c(current$config$n_taps, current$config$n_taps, n_channels)
      )
      for (j in seq_len(n_channels)) {
        covariance[, , j] <- diag(
          1 / current$config$delta, current$config$n_taps
        )
      }
    }
    list(
      algorithm = current$algorithm,
      config = current$config,
      channel_names = channels,
      reference_names = references,
      n_samples = 0,
      n_chunks = 0,
      reset_count = current$reset_count + 1,
      last_timestamp = NULL,
      timestamp_mode = NULL,
      weights = weights,
      history = if (is.null(n_channels)) NULL else
        matrix(0, max(0L, current$config$n_taps - 1L), n_channels),
      covariance = covariance,
      residual_ss = if (is.null(n_channels)) NULL else rep(0, n_channels),
      reference_ss = if (is.null(n_channels)) NULL else rep(0, n_channels)
    )
  }
  class <- switch(
    algorithm,
    lms = "LMSFilter",
    nlms = "NLMSFilter",
    rls = "RLSFilter"
  )
  .dsp_new_processor(class, state, resetter)
}

#' Stateful least-mean-squares adaptive filter
#'
#' Adaptive filters estimate the part of each signal channel linearly
#' predictable from its paired reference. The returned residual is not
#' inherently a clean physiological signal; that interpretation requires a
#' caller-owned reference-noise assumption.
#'
#' @param n_taps Exact adaptive-filter order.
#' @param step_size Positive LMS step size.
#' @param leakage Weight leakage in `[0, 1)`.
#' @param initial_weights Optional finite `n_taps` by channel matrix.
#' @return A mutable `LMSFilter` streaming processor.
#' @examples
#' # Cancel a reference-correlated component from a short synthetic signal.
#' set.seed(1)
#' reference <- sin(2 * pi * 5 * seq_len(200) / 100)
#' signal <- 0.4 * reference + rnorm(200, sd = 0.05)
#' filt <- lmsFilter(n_taps = 4L, step_size = 0.05)
#' result <- update(filt, matrix(signal, ncol = 1),
#'                  reference = matrix(reference, ncol = 1))
#' str(result$output)
#' @export
lmsFilter <- function(n_taps, step_size, leakage = 0,
                      initial_weights = NULL) {
  .adaptive_constructor(
    "lms", n_taps, step_size = step_size, leakage = leakage,
    initial_weights = initial_weights
  )
}

#' Stateful normalized LMS adaptive filter
#'
#' @inheritParams lmsFilter
#' @param step_size Positive normalized step size; values in `(0, 2)` are the
#'   conventional stable range.
#' @param epsilon Positive denominator regularizer.
#' @return A mutable `NLMSFilter` streaming processor.
#' @examples
#' set.seed(1)
#' reference <- sin(2 * pi * 8 * seq_len(200) / 100)
#' signal <- 0.3 * reference + rnorm(200, sd = 0.05)
#' filt <- nlmsFilter(n_taps = 4L, step_size = 0.5)
#' result <- update(filt, matrix(signal, ncol = 1),
#'                  reference = matrix(reference, ncol = 1))
#' tail(result$output)
#' @export
nlmsFilter <- function(n_taps, step_size = 0.5, epsilon = 1e-8,
                       leakage = 0, initial_weights = NULL) {
  .adaptive_constructor(
    "nlms", n_taps, step_size = step_size, epsilon = epsilon,
    leakage = leakage, initial_weights = initial_weights
  )
}

#' Stateful recursive-least-squares adaptive filter
#'
#' @inheritParams lmsFilter
#' @param forgetting Forgetting factor in `(0, 1]`.
#' @param delta Positive initial inverse-covariance scale.
#' @return A mutable `RLSFilter` streaming processor.
#' @examples
#' set.seed(1)
#' reference <- cos(2 * pi * 6 * seq_len(150) / 100)
#' signal <- 0.5 * reference + rnorm(150, sd = 0.05)
#' filt <- rlsFilter(n_taps = 3L, forgetting = 0.995)
#' result <- update(filt, matrix(signal, ncol = 1),
#'                  reference = matrix(reference, ncol = 1))
#' length(result$output)
#' @export
rlsFilter <- function(n_taps, forgetting = 0.99, delta = 1,
                      initial_weights = NULL) {
  .adaptive_constructor(
    "rls", n_taps, forgetting = forgetting, delta = delta,
    initial_weights = initial_weights
  )
}

.adaptive_bind <- function(state, samples, reference) {
  n_channels <- ncol(samples)
  taps <- state$config$n_taps
  numeric_count <- as.double(taps) * n_channels +
    as.double(max(0L, taps - 1L)) * n_channels
  if (identical(state$algorithm, "rls")) {
    numeric_count <- numeric_count +
      as.double(taps) * taps * n_channels
  }
  if (!is.finite(numeric_count) ||
      numeric_count * 8 > .dsp_state_limit * 0.9) {
    .dsp_abort(
      "adaptive processor state would exceed the governed ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  reference_names <- .dsp_channel_names(reference)
  if (!(ncol(reference) %in% c(1L, n_channels))) {
    .dsp_abort(
      "`reference` must have one column or exactly one per signal channel",
      "PhysioStream_dsp_channel_error"
    )
  }
  if (ncol(reference) == 1L && n_channels > 1L) {
    reference <- reference[, rep.int(1L, n_channels), drop = FALSE]
    reference_names <- rep.int(reference_names, n_channels)
  }
  if (!is.null(state$reference_names) &&
      !identical(reference_names, state$reference_names)) {
    .dsp_abort(
      "reference channel count, order, or names changed",
      "PhysioStream_dsp_channel_error"
    )
  }
  if (is.null(state$channel_names)) {
    state$channel_names <- .dsp_channel_names(samples)
    state$reference_names <- reference_names
    initial <- state$config$initial_weights
    if (is.null(initial)) {
      state$weights <- matrix(0, state$config$n_taps, n_channels)
    } else {
      if (ncol(initial) != n_channels) {
        .dsp_abort(
          "`initial_weights` columns must match the first signal chunk",
          "PhysioStream_dsp_channel_error"
        )
      }
      state$weights <- initial
    }
    state$history <- matrix(
      0, max(0L, state$config$n_taps - 1L), n_channels
    )
    state$residual_ss <- rep(0, n_channels)
    state$reference_ss <- rep(0, n_channels)
    if (identical(state$algorithm, "rls")) {
      p <- state$config$n_taps
      state$covariance <- array(0, c(p, p, n_channels))
      for (j in seq_len(n_channels)) {
        state$covariance[, , j] <- diag(1 / state$config$delta, p)
      }
    }
  }
  list(state = state, reference = reference)
}

.adaptive_update <- function(object, samples, reference, timestamps) {
  .dsp_assert_processor(object)
  old <- .dsp_deep_copy(object$state)
  chunk <- .dsp_validate_chunk(old, samples, timestamps)
  if (chunk$empty) {
    if (!is.null(reference)) {
      ref <- .dsp_normalize_matrix(reference, "reference")
      if (nrow(ref) != 0L) {
        .dsp_abort(
          "`reference` rows must match `samples`",
          "PhysioStream_dsp_validation_error"
        )
      }
    }
    return(.dsp_empty_result(old))
  }
  if (is.null(reference)) {
    .dsp_abort(
      "`reference` is required for adaptive filtering",
      "PhysioStream_dsp_validation_error"
    )
  }
  reference <- .dsp_normalize_matrix(reference, "reference")
  if (nrow(reference) != nrow(chunk$samples)) {
    .dsp_abort(
      "`reference` rows must match `samples`",
      "PhysioStream_dsp_validation_error"
    )
  }
  bound <- .adaptive_bind(old, chunk$samples, reference)
  state <- bound$state
  reference <- bound$reference
  n <- nrow(chunk$samples)
  channels <- ncol(chunk$samples)
  estimate <- matrix(0, n, channels)
  residual <- matrix(0, n, channels)
  for (j in seq_len(channels)) {
    history <- state$history[, j]
    weights <- state$weights[, j]
    covariance <- if (identical(state$algorithm, "rls")) {
      state$covariance[, , j]
    } else {
      NULL
    }
    for (i in seq_len(n)) {
      u <- c(reference[i, j], history)
      estimate[i, j] <- sum(weights * u)
      residual[i, j] <- chunk$samples[i, j] - estimate[i, j]
      if (identical(state$algorithm, "lms")) {
        weights <- (1 - state$config$leakage) * weights +
          state$config$step_size * residual[i, j] * u
      } else if (identical(state$algorithm, "nlms")) {
        denominator <- state$config$epsilon + sum(u * u)
        weights <- (1 - state$config$leakage) * weights +
          state$config$step_size * residual[i, j] * u / denominator
      } else {
        product <- as.numeric(covariance %*% u)
        denominator <- state$config$forgetting + sum(u * product)
        tolerance <- 64 * .Machine$double.eps *
          max(1, abs(state$config$forgetting), sum(abs(u * product)))
        if (!is.finite(denominator) || denominator <= tolerance) {
          .dsp_abort(
            "RLS covariance update has a non-positive denominator",
            "PhysioStream_dsp_numeric_error"
          )
        }
        gain <- product / denominator
        weights <- weights + gain * residual[i, j]
        covariance <- (
          covariance - tcrossprod(gain, as.numeric(u %*% covariance))
        ) / state$config$forgetting
        covariance <- (covariance + t(covariance)) / 2
        eigenvalues <- eigen(
          covariance, symmetric = TRUE, only.values = TRUE
        )$values
        psd_tolerance <- 256 * .Machine$double.eps *
          max(1, max(abs(eigenvalues)))
        if (min(eigenvalues) < -psd_tolerance) {
          .dsp_abort(
            "RLS inverse covariance lost positive semidefiniteness",
            "PhysioStream_dsp_numeric_error"
          )
        }
      }
      if (length(history)) {
        history <- head(u, -1L)
      }
      if (any(!is.finite(weights))) {
        .dsp_abort(
          "adaptive weights became non-finite",
          "PhysioStream_dsp_numeric_error"
        )
      }
    }
    state$weights[, j] <- weights
    state$history[, j] <- history
    if (identical(state$algorithm, "rls")) {
      state$covariance[, , j] <- covariance
    }
  }
  colnames(estimate) <- colnames(residual) <- state$channel_names
  state$residual_ss <- state$residual_ss + colSums(residual^2)
  state$reference_ss <- state$reference_ss + colSums(reference^2)
  state$n_samples <- state$n_samples + n
  state$n_chunks <- state$n_chunks + 1
  supplied_timestamps <- !is.null(chunk$timestamps)
  if (is.null(state$timestamp_mode)) {
    state$timestamp_mode <- supplied_timestamps
  } else if (!identical(state$timestamp_mode, supplied_timestamps)) {
    .dsp_abort(
      "timestamp presence cannot change after the first nonempty chunk",
      "PhysioStream_dsp_timestamp_error"
    )
  }
  if (supplied_timestamps) {
    state$last_timestamp <- tail(chunk$timestamps, 1L)
  }
  diagnostics <- list(
    signal = chunk$samples,
    estimate = estimate,
    residual = residual,
    weights = state$weights,
    coefficient_norm = sqrt(colSums(state$weights^2)),
    reference_power = state$reference_ss / state$n_samples,
    residual_power = state$residual_ss / state$n_samples,
    converged = is.finite(sqrt(colSums(state$weights^2)))
  )
  .dsp_commit(
    object, old, state, residual, chunk$timestamps,
    n_input = n, n_emitted = n, diagnostics = diagnostics
  )
}

#' @export
update.LMSFilter <- function(object, samples, reference = NULL,
                             timestamps = NULL, ...) {
  .adaptive_update(object, samples, reference, timestamps)
}

#' @export
update.NLMSFilter <- function(object, samples, reference = NULL,
                              timestamps = NULL, ...) {
  .adaptive_update(object, samples, reference, timestamps)
}

#' @export
update.RLSFilter <- function(object, samples, reference = NULL,
                             timestamps = NULL, ...) {
  .adaptive_update(object, samples, reference, timestamps)
}
