# Exponentially weighted online Welch spectrum

Complete windows are converted to one-sided power spectral density and
combined as `alpha * newest + (1 - alpha) * previous`.

## Usage

``` r
welchOnline(
  sampling_rate,
  window_samples,
  hop_samples = floor(window_samples/2),
  n_fft = window_samples,
  alpha = 0.1,
  window = c("hann", "hamming", "rectangular"),
  detrend = c("mean", "none"),
  bands = NULL,
  causal_filter = NULL,
  n_features = NULL
)
```

## Arguments

- sampling_rate:

  Positive samples per second.

- window_samples:

  Exact frame length.

- hop_samples:

  Exact frame advance, no larger than the window.

- n_fft:

  Exact FFT length, at least the frame length.

- alpha:

  Weight assigned to the newest periodogram.

- window:

  Exact governed window definition.

- detrend:

  Per-frame mean removal or no detrending.

- bands:

  Optional uniquely named two-column Hz matrix.

- causal_filter:

  Optional independent
  [PhysioPreprocess::StreamFilter](https://x-biosignal.r-universe.dev/PhysioPreprocess/reference/StreamFilter.html)
  objects or factory.

- n_features:

  Optional channel count to bind at construction.

## Value

A mutable `OnlineWelch` streaming processor.
