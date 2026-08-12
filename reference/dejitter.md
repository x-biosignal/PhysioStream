# Regularize stream timestamps on a nominal grid

This changes timestamps only. It never interpolates or reorders samples.

## Usage

``` r
dejitter(
  timestamps,
  nominal_srate,
  segments = NULL,
  anchor = c("least_squares", "first"),
  max_residual_seconds = Inf
)
```

## Arguments

- timestamps:

  Finite non-decreasing timestamps.

- nominal_srate:

  Positive nominal sampling rate.

- segments:

  Optional positive labels or zero-based inclusive ranges.

- anchor:

  Exact grid anchoring rule.

- max_residual_seconds:

  Maximum permitted absolute jitter residual.

## Value

Regular timestamps with a plain `dejitter` diagnostics attribute.
