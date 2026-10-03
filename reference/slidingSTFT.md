# Sliding short-time Fourier transform

Sliding short-time Fourier transform

## Usage

``` r
slidingSTFT(
  sampling_rate,
  window_samples,
  hop_samples,
  n_fft = window_samples,
  window = c("hann", "hamming", "rectangular"),
  detrend = c("mean", "none"),
  output = c("power", "complex"),
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

- window:

  Exact governed window definition.

- detrend:

  Per-frame mean removal or no detrending.

- output:

  Return one-sided density power or normalized complex coefficients
  whose squared modulus equals that density.

- bands:

  Optional uniquely named two-column Hz matrix.

- causal_filter:

  Optional independent
  [PhysioPreprocess::StreamFilter](https://x-biosignal.r-universe.dev/PhysioPreprocess/reference/StreamFilter.html)
  objects or factory.

- n_features:

  Optional channel count to bind at construction.

## Value

A mutable `SlidingSTFT` streaming processor.

## Examples

``` r
sr <- 100
signal <- sin(2 * pi * 10 * seq_len(200) / sr)
stft <- slidingSTFT(sr, window_samples = 64L, hop_samples = 16L,
                    output = "power")
result <- update(stft, signal)
dim(result$output)
#> [1]  9 33  1
```
