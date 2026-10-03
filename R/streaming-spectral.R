.spectral_window <- function(type, n) {
  if (n == 1L) {
    return(1)
  }
  index <- 0:(n - 1L)
  switch(
    type,
    hann = 0.5 - 0.5 * cos(2 * pi * index / (n - 1L)),
    hamming = 0.54 - 0.46 * cos(2 * pi * index / (n - 1L)),
    rectangular = rep(1, n)
  )
}

.spectral_bands <- function(bands, sampling_rate) {
  if (is.null(bands)) {
    return(NULL)
  }
  if (is.data.frame(bands)) {
    bands <- as.matrix(bands)
  }
  if (!is.matrix(bands) || !is.numeric(bands) || ncol(bands) != 2L ||
      any(!is.finite(bands)) || any(bands < 0) ||
      any(bands[, 2L] < bands[, 1L]) ||
      any(bands[, 2L] > sampling_rate / 2) ||
      is.null(rownames(bands)) || any(!nzchar(rownames(bands))) ||
      anyDuplicated(rownames(bands))) {
    .dsp_abort(
      "`bands` must be a uniquely named finite two-column Hz matrix",
      "PhysioStream_dsp_validation_error"
    )
  }
  matrix(
    as.numeric(bands), nrow = nrow(bands), ncol = 2L,
    dimnames = list(rownames(bands), c("low", "high"))
  )
}

.spectral_filter_spec <- function(causal_filter) {
  if (is.null(causal_filter)) {
    return(list(kind = "none", value = NULL))
  }
  if (is.function(causal_filter)) {
    return(list(kind = "factory", value = causal_filter))
  }
  if (inherits(causal_filter, "StreamFilter")) {
    return(list(kind = "list", value = list(causal_filter)))
  }
  if (is.list(causal_filter) && length(causal_filter) &&
      all(vapply(causal_filter, inherits, logical(1), "StreamFilter"))) {
    for (i in seq_along(causal_filter)) {
      if (i > 1L && any(vapply(
        causal_filter[seq_len(i - 1L)], identical, logical(1),
        causal_filter[[i]]
      ))) {
        .dsp_abort(
          "a mutable causal filter cannot be shared across channels",
          "PhysioStream_dsp_validation_error"
        )
      }
    }
    return(list(kind = "list", value = causal_filter))
  }
  .dsp_abort(
    "`causal_filter` must be NULL, a StreamFilter factory, or independent filters",
    "PhysioStream_dsp_validation_error"
  )
}

.spectral_filter_snapshot <- function(filters) {
  if (is.null(filters)) {
    return(NULL)
  }
  lapply(filters, function(filter) {
    list(
      sos = unclass(filter$sos),
      warmup = isTRUE(filter$warmup),
      steady = unclass(filter$steady),
      zi = unclass(filter$zi),
      primed = isTRUE(filter$primed)
    )
  })
}

.spectral_restore_filters <- function(filters, snapshots) {
  if (is.null(filters)) {
    return(invisible(NULL))
  }
  for (i in seq_along(filters)) {
    filters[[i]]$zi <- snapshots[[i]]$zi
    filters[[i]]$primed <- snapshots[[i]]$primed
  }
  invisible(NULL)
}

.spectral_bind_filters <- function(object, n_channels) {
  if (!is.null(object$filters)) {
    if (length(object$filters) != n_channels) {
      .dsp_abort(
        "causal filter count must match signal channel count",
        "PhysioStream_dsp_channel_error"
      )
    }
    return(object$filters)
  }
  if (identical(object$filter_kind, "none")) {
    return(NULL)
  }
  if (identical(object$filter_kind, "list")) {
    filters <- object$filter_value
    if (length(filters) != n_channels) {
      .dsp_abort(
        "causal filter count must match signal channel count",
        "PhysioStream_dsp_channel_error"
      )
    }
  } else {
    filters <- lapply(seq_len(n_channels), function(i) {
      filter <- object$filter_value(i)
      if (!inherits(filter, "StreamFilter")) {
        .dsp_abort(
          "causal filter factory must return a StreamFilter",
          "PhysioStream_dsp_validation_error"
        )
      }
      filter
    })
  }
  if (length(filters) > 1L) {
    for (i in 2:length(filters)) {
      if (any(vapply(
        filters[seq_len(i - 1L)], identical, logical(1), filters[[i]]
      ))) {
        .dsp_abort(
          "causal filter factory returned shared mutable state",
          "PhysioStream_dsp_validation_error"
        )
      }
    }
  }
  object$filters <- filters
  filters
}

.spectral_reset_state <- function(current, keep_channels) {
  channels <- if (keep_channels) current$channel_names else NULL
  n_channels <- if (is.null(channels)) {
    current$config$n_features
  } else {
    length(channels)
  }
  list(
    algorithm = current$algorithm,
    config = current$config,
    channel_names = channels,
    n_samples = 0,
    n_chunks = 0,
    reset_count = current$reset_count + 1,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    frame_count = 0,
    tail_samples = if (is.null(n_channels)) NULL else
      matrix(numeric(), 0L, n_channels),
    tail_sequences = numeric(),
    tail_timestamps = NULL,
    current_psd = NULL,
    runtime_state = NULL
  )
}

.spectral_constructor <- function(algorithm, sampling_rate, window_samples,
                                  hop_samples, n_fft, window, detrend, bands,
                                  alpha = NULL, output = NULL,
                                  causal_filter = NULL, n_features = NULL) {
  sampling_rate <- .dsp_scalar(
    sampling_rate, "sampling_rate", 0, Inf, lower_open = TRUE
  )
  window_samples <- .dsp_scalar(
    window_samples, "window_samples", 1, .Machine$integer.max,
    integer = TRUE
  )
  hop_samples <- .dsp_scalar(
    hop_samples, "hop_samples", 1, window_samples, integer = TRUE
  )
  n_fft <- .dsp_scalar(
    n_fft, "n_fft", window_samples, .Machine$integer.max, integer = TRUE
  )
  window <- .dsp_enum(window[[1L]], c("hann", "hamming", "rectangular"),
                      "window")
  if (sum(.spectral_window(window, window_samples)^2) <= 0) {
    .dsp_abort(
      "the selected window has zero energy at this frame length",
      "PhysioStream_dsp_validation_error"
    )
  }
  detrend <- .dsp_enum(detrend[[1L]], c("mean", "none"), "detrend")
  bands <- .spectral_bands(bands, sampling_rate)
  if (!is.null(alpha)) {
    alpha <- .dsp_scalar(alpha, "alpha", 0, 1, lower_open = TRUE)
  }
  if (!is.null(output)) {
    output <- .dsp_enum(output[[1L]], c("power", "complex"), "output")
  }
  if (!is.null(n_features)) {
    n_features <- .dsp_scalar(
      n_features, "n_features", 1, .Machine$integer.max, integer = TRUE
    )
  }
  frequencies <- floor(n_fft / 2) + 1
  bytes <- 16 * as.double(frequencies) * max(1, n_features %||% 1)
  if (!is.finite(bytes) || bytes > .dsp_allocation_limit) {
    .dsp_abort(
      "spectral output allocation exceeds the governed ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  filter_spec <- .spectral_filter_spec(causal_filter)
  config <- list(
    sampling_rate = sampling_rate,
    window_samples = window_samples,
    hop_samples = hop_samples,
    n_fft = n_fft,
    alpha = alpha,
    window = window,
    detrend = detrend,
    bands = bands,
    output = output,
    n_features = n_features,
    causal_filter = filter_spec$kind != "none"
  )
  state <- list(
    algorithm = algorithm,
    config = config,
    channel_names = NULL,
    n_samples = 0,
    n_chunks = 0,
    reset_count = 0,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    frame_count = 0,
    tail_samples = if (is.null(n_features)) NULL else
      matrix(numeric(), 0L, n_features),
    tail_sequences = numeric(),
    tail_timestamps = NULL,
    current_psd = NULL,
    runtime_state = NULL
  )
  class <- if (algorithm == "welch") "OnlineWelch" else "SlidingSTFT"
  object <- .dsp_new_processor(class, state, .spectral_reset_state)
  object$filter_kind <- filter_spec$kind
  object$filter_value <- filter_spec$value
  object$filters <- if (filter_spec$kind == "list") filter_spec$value else NULL
  if (!is.null(object$filters)) {
    object$state$runtime_state <- .spectral_filter_snapshot(object$filters)
    object$state <- .dsp_seal_state(object$state)
  }
  object$runtime_resetter <- function(keep_channels) {
    filters <- object$filters
    if (is.null(filters)) {
      return(NULL)
    }
    snapshots <- .spectral_filter_snapshot(filters)
    committed <- FALSE
    on.exit({
      if (!committed) {
        .spectral_restore_filters(filters, snapshots)
      }
    }, add = TRUE)
    for (filter in filters) {
      filter$reset()
    }
    result <- .spectral_filter_snapshot(filters)
    committed <- TRUE
    result
  }
  object
}

# `%||%` is local to this file to avoid an extra dependency.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Exponentially weighted online Welch spectrum
#'
#' Complete windows are converted to one-sided power spectral density and
#' combined as `alpha * newest + (1 - alpha) * previous`.
#'
#' @param sampling_rate Positive samples per second.
#' @param window_samples Exact frame length.
#' @param hop_samples Exact frame advance, no larger than the window.
#' @param n_fft Exact FFT length, at least the frame length.
#' @param alpha Weight assigned to the newest periodogram.
#' @param window Exact governed window definition.
#' @param detrend Per-frame mean removal or no detrending.
#' @param bands Optional uniquely named two-column Hz matrix.
#' @param causal_filter Optional independent
#'   [PhysioPreprocess::StreamFilter] objects or factory.
#' @param n_features Optional channel count to bind at construction.
#' @return A mutable `OnlineWelch` streaming processor.
#' @examples
#' sr <- 100
#' signal <- sin(2 * pi * 10 * seq_len(256) / sr)
#' welch <- welchOnline(sr, window_samples = 64L, hop_samples = 32L)
#' result <- update(welch, signal)
#' dim(result$output)
#' @export
welchOnline <- function(sampling_rate, window_samples,
                        hop_samples = floor(window_samples / 2),
                        n_fft = window_samples, alpha = 0.1,
                        window = c("hann", "hamming", "rectangular"),
                        detrend = c("mean", "none"), bands = NULL,
                        causal_filter = NULL, n_features = NULL) {
  if (missing(hop_samples) && identical(as.numeric(window_samples), 1)) {
    hop_samples <- 1L
  }
  .spectral_constructor(
    "welch", sampling_rate, window_samples, hop_samples, n_fft,
    window, detrend, bands, alpha = alpha,
    causal_filter = causal_filter, n_features = n_features
  )
}

#' Sliding short-time Fourier transform
#'
#' @inheritParams welchOnline
#' @param output Return one-sided density power or normalized complex
#'   coefficients whose squared modulus equals that density.
#' @return A mutable `SlidingSTFT` streaming processor.
#' @examples
#' sr <- 100
#' signal <- sin(2 * pi * 10 * seq_len(200) / sr)
#' stft <- slidingSTFT(sr, window_samples = 64L, hop_samples = 16L,
#'                     output = "power")
#' result <- update(stft, signal)
#' dim(result$output)
#' @export
slidingSTFT <- function(sampling_rate, window_samples, hop_samples,
                        n_fft = window_samples,
                        window = c("hann", "hamming", "rectangular"),
                        detrend = c("mean", "none"),
                        output = c("power", "complex"), bands = NULL,
                        causal_filter = NULL, n_features = NULL) {
  .spectral_constructor(
    "stft", sampling_rate, window_samples, hop_samples, n_fft,
    window, detrend, bands, output = output,
    causal_filter = causal_filter, n_features = n_features
  )
}

.spectral_periodogram <- function(frame, config) {
  if (config$detrend == "mean") {
    frame <- sweep(frame, 2L, colMeans(frame))
  }
  weights <- .spectral_window(config$window, config$window_samples)
  weighted <- frame * weights
  n_frequency <- floor(config$n_fft / 2) + 1L
  transform <- vapply(
    seq_len(ncol(weighted)),
    function(j) {
      padded <- c(
        weighted[, j],
        numeric(config$n_fft - config$window_samples)
      )
      stats::fft(padded)[seq_len(n_frequency)]
    },
    complex(n_frequency)
  )
  if (is.null(dim(transform))) {
    transform <- matrix(transform, ncol = 1L)
  }
  factor <- rep(2, n_frequency)
  factor[[1L]] <- 1
  if (config$n_fft %% 2L == 0L) {
    factor[[n_frequency]] <- 1
  }
  normalization <- config$sampling_rate * sum(weights^2)
  power <- Mod(transform)^2 / normalization
  power <- power * factor
  normalized_complex <- transform /
    sqrt(normalization) * sqrt(factor)
  list(power = power, complex = normalized_complex)
}

.band_integral <- function(frequency, density, interval) {
  low <- max(interval[[1L]], min(frequency))
  high <- min(interval[[2L]], max(frequency))
  if (high <= low) {
    return(0)
  }
  if (low == high) {
    return(0)
  }
  inside <- frequency > low & frequency < high
  x <- c(low, frequency[inside], high)
  y <- stats::approx(
    frequency, density, xout = x, method = "linear",
    ties = "ordered", rule = 2
  )$y
  sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
}

.spectral_band_power <- function(power, frequencies, bands) {
  if (is.null(bands)) {
    return(NULL)
  }
  result <- matrix(
    0, nrow(bands), ncol(power),
    dimnames = list(rownames(bands), colnames(power))
  )
  for (band in seq_len(nrow(bands))) {
    for (channel in seq_len(ncol(power))) {
      result[band, channel] <- .band_integral(
        frequencies, power[, channel], bands[band, ]
      )
    }
  }
  result
}

.spectral_update <- function(object, samples, timestamps) {
  .dsp_assert_processor(object)
  old <- .dsp_deep_copy(object$state)
  chunk <- .dsp_validate_chunk(old, samples, timestamps)
  if (chunk$empty) {
    return(.dsp_empty_result(old))
  }
  state <- old
  channels <- ncol(chunk$samples)
  if (is.null(state$channel_names)) {
    if (!is.null(state$config$n_features) &&
        channels != state$config$n_features) {
      .dsp_abort(
        "first chunk does not match configured `n_features`",
        "PhysioStream_dsp_channel_error"
      )
    }
    state$channel_names <- chunk$names
    state$tail_samples <- matrix(numeric(), 0L, channels)
  }
  supplied_timestamps <- !is.null(chunk$timestamps)
  if (is.null(state$timestamp_mode)) {
    state$timestamp_mode <- supplied_timestamps
  } else if (!identical(state$timestamp_mode, supplied_timestamps)) {
    .dsp_abort(
      "timestamp presence cannot change after the first nonempty chunk",
      "PhysioStream_dsp_timestamp_error"
    )
  }
  frequency_count <- floor(state$config$n_fft / 2) + 1L
  buffered_rows <- nrow(state$tail_samples) + nrow(chunk$samples)
  possible_frames <- if (buffered_rows < state$config$window_samples) {
    0
  } else {
    floor(
      (buffered_rows - state$config$window_samples) /
        state$config$hop_samples
    ) + 1
  }
  output_bytes <- as.double(possible_frames) * frequency_count * channels *
    if (state$algorithm == "stft" && state$config$output == "complex") 24 else 16
  if (!is.finite(output_bytes) || output_bytes > .dsp_allocation_limit) {
    .dsp_abort(
      "spectral frame output exceeds the 512 MiB allocation ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  filters <- .spectral_bind_filters(object, channels)
  filter_before <- .spectral_filter_snapshot(filters)
  if (!identical(filter_before, old$runtime_state)) {
    first_factory_bind <- is.null(old$channel_names) &&
      is.null(old$runtime_state) && identical(object$filter_kind, "factory")
    if (!(is.null(filter_before) && is.null(old$runtime_state)) &&
        !first_factory_bind) {
      .dsp_abort(
        "causal filter runtime state differs from the governed snapshot",
        "PhysioStream_dsp_state_error"
      )
    }
  }
  committed <- FALSE
  on.exit({
    if (!committed) {
      .spectral_restore_filters(filters, filter_before)
    }
  }, add = TRUE)
  filtered <- chunk$samples
  if (!is.null(filters)) {
    for (j in seq_len(channels)) {
      filtered[, j] <- filters[[j]]$apply(filtered[, j])
    }
    if (any(!is.finite(filtered)) || nrow(filtered) != nrow(chunk$samples)) {
      .dsp_abort(
        "causal filter did not preserve finite row-aligned samples",
        "PhysioStream_dsp_numeric_error"
      )
    }
  }
  new_sequences <- old$n_samples + seq_len(nrow(filtered))
  buffer <- rbind(state$tail_samples, filtered)
  sequences <- c(state$tail_sequences, new_sequences)
  times <- if (supplied_timestamps) {
    c(state$tail_timestamps, chunk$timestamps)
  } else {
    NULL
  }
  n_frequency <- floor(state$config$n_fft / 2) + 1L
  frequencies <- (0:(n_frequency - 1L)) *
    state$config$sampling_rate / state$config$n_fft
  frame_values <- list()
  frame_power <- list()
  frame_sequences <- numeric()
  frame_times <- if (supplied_timestamps) numeric() else NULL
  band_values <- list()
  start <- 1L
  while (start + state$config$window_samples - 1L <= nrow(buffer)) {
    index <- seq.int(start, start + state$config$window_samples - 1L)
    spectral <- .spectral_periodogram(buffer[index, , drop = FALSE],
                                      state$config)
    colnames(spectral$power) <- colnames(spectral$complex) <-
      state$channel_names
    if (state$algorithm == "welch") {
      if (is.null(state$current_psd)) {
        state$current_psd <- spectral$power
      } else {
        state$current_psd <- state$config$alpha * spectral$power +
          (1 - state$config$alpha) * state$current_psd
      }
      value <- state$current_psd
      power <- state$current_psd
    } else {
      value <- if (state$config$output == "power") {
        spectral$power
      } else {
        spectral$complex
      }
      power <- spectral$power
    }
    frame_values[[length(frame_values) + 1L]] <- value
    frame_power[[length(frame_power) + 1L]] <- power
    frame_sequences <- c(frame_sequences, tail(sequences[index], 1L))
    if (supplied_timestamps) {
      frame_times <- c(frame_times, tail(times[index], 1L))
    }
    band_values[[length(band_values) + 1L]] <- .spectral_band_power(
      power, frequencies, state$config$bands
    )
    start <- start + state$config$hop_samples
  }
  if (start <= nrow(buffer)) {
    retained <- seq.int(start, nrow(buffer))
    state$tail_samples <- buffer[retained, , drop = FALSE]
    state$tail_sequences <- sequences[retained]
    state$tail_timestamps <- if (supplied_timestamps) times[retained] else NULL
  } else {
    state$tail_samples <- matrix(numeric(), 0L, channels)
    state$tail_sequences <- numeric()
    state$tail_timestamps <- NULL
  }
  if (nrow(state$tail_samples) >= state$config$window_samples) {
    .dsp_abort(
      "spectral processor retained an invalid complete frame",
      "PhysioStream_dsp_state_error"
    )
  }
  frames <- length(frame_values)
  complex_output <- state$algorithm == "stft" &&
    state$config$output == "complex"
  output <- array(
    if (complex_output) 0 + 0i else 0,
    dim = c(frames, n_frequency, channels),
    dimnames = list(
      NULL, sprintf("%.17g", frequencies), state$channel_names
    )
  )
  power_output <- array(
    0, dim = c(frames, n_frequency, channels),
    dimnames = dimnames(output)
  )
  if (frames) {
    for (i in seq_len(frames)) {
      output[i, , ] <- frame_values[[i]]
      power_output[i, , ] <- frame_power[[i]]
    }
  }
  band_output <- NULL
  if (!is.null(state$config$bands)) {
    band_output <- array(
      0, dim = c(frames, nrow(state$config$bands), channels),
      dimnames = list(
        NULL, rownames(state$config$bands), state$channel_names
      )
    )
    if (frames) {
      for (i in seq_len(frames)) {
        band_output[i, , ] <- band_values[[i]]
      }
    }
  }
  state$n_samples <- state$n_samples + nrow(chunk$samples)
  state$n_chunks <- state$n_chunks + 1
  state$frame_count <- state$frame_count + frames
  if (supplied_timestamps) {
    state$last_timestamp <- tail(chunk$timestamps, 1L)
  }
  state$runtime_state <- .spectral_filter_snapshot(filters)
  diagnostics <- list(
    frequencies = frequencies,
    frame_sequences = frame_sequences,
    frame_timestamps = frame_times,
    power = power_output,
    band_power = band_output,
    retained_samples = nrow(state$tail_samples),
    total_frames = state$frame_count
  )
  result <- .dsp_commit(
    object, old, state, output, frame_times,
    n_input = nrow(chunk$samples), n_emitted = frames,
    diagnostics = diagnostics,
    sequence_start = if (frames) frame_sequences[[1L]] else numeric(),
    sequence_end = if (frames) tail(frame_sequences, 1L) else numeric()
  )
  committed <- TRUE
  result
}

#' @export
update.OnlineWelch <- function(object, samples, reference = NULL,
                               timestamps = NULL, ...) {
  if (!is.null(reference)) {
    .dsp_abort(
      "`reference` is not used by online Welch spectra",
      "PhysioStream_dsp_validation_error"
    )
  }
  .spectral_update(object, samples, timestamps)
}

#' @export
update.SlidingSTFT <- function(object, samples, reference = NULL,
                               timestamps = NULL, ...) {
  if (!is.null(reference)) {
    .dsp_abort(
      "`reference` is not used by sliding STFT",
      "PhysioStream_dsp_validation_error"
    )
  }
  .spectral_update(object, samples, timestamps)
}
