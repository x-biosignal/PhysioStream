dsp_serialize <- function(x) {
  serialize(x, NULL, version = 3L)
}

dsp_periodogram <- function(x, sampling_rate, n_fft, window = "hann",
                            detrend = "none") {
  n <- length(x)
  index <- 0:(n - 1L)
  weights <- switch(
    window,
    hann = if (n == 1L) 1 else
      0.5 - 0.5 * cos(2 * pi * index / (n - 1L)),
    hamming = if (n == 1L) 1 else
      0.54 - 0.46 * cos(2 * pi * index / (n - 1L)),
    rectangular = rep(1, n)
  )
  if (detrend == "mean") {
    x <- x - mean(x)
  }
  padded <- c(x * weights, numeric(n_fft - n))
  n_frequency <- floor(n_fft / 2) + 1L
  transformed <- fft(padded)[seq_len(n_frequency)]
  factor <- rep(2, n_frequency)
  factor[[1L]] <- 1
  if (n_fft %% 2L == 0L) {
    factor[[n_frequency]] <- 1
  }
  Mod(transformed)^2 / (sampling_rate * sum(weights^2)) * factor
}

dsp_best_correlation <- function(expected, observed) {
  permutations <- rbind(
    c(1L, 2L, 3L), c(1L, 3L, 2L), c(2L, 1L, 3L),
    c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L)
  )
  correlation <- abs(stats::cor(expected, observed))
  max(apply(permutations, 1L, function(permutation) {
    mean(correlation[cbind(seq_len(3L), permutation)])
  }))
}
