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

## Examples

``` r
device <- seq(0, 10, by = 0.5)
offset <- -0.4 + 0.005 * (device - median(device))
model <- clockOffset(device, offset, method = "ols")
corrected <- driftCorrect(seq(0, 10, by = 1), model)
as.numeric(corrected)
#>  [1] -0.425  0.580  1.585  2.590  3.595  4.600  5.605  6.610  7.615  8.620
#> [11]  9.625
```
