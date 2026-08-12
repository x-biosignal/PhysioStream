# Apply a governed stream-clock model

Apply a governed stream-clock model

## Usage

``` r
driftCorrect(
  timestamps,
  model,
  segments = NULL,
  extrapolate = c("bounded", "error"),
  max_extrapolation_seconds = 30
)
```

## Arguments

- timestamps:

  Finite device-clock timestamps.

- model:

  A valid `StreamClockModel`.

- segments:

  Optional positive labels or zero-based inclusive ranges.

- extrapolate:

  Exact extrapolation policy.

- max_extrapolation_seconds:

  Non-negative bounded extrapolation limit.

## Value

Corrected timestamps with a plain `clock_correction` attribute.
