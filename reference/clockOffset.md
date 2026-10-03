# Estimate a governed stream-clock mapping

Fits recorded clock offsets under the sign convention
`master_time = device_time + offset(device_time)`. Fits are centered
within each reset segment.

## Usage

``` r
clockOffset(
  clock_times,
  clock_values,
  method = c("huber", "ols", "ransac"),
  origin = c("median", "first"),
  segments = NULL,
  huber_k = 1.345,
  ransac_threshold = NULL,
  ransac_trials = 200L,
  seed = 1L
)
```

## Arguments

- clock_times:

  Finite non-decreasing device-clock observation times.

- clock_values:

  Finite observed offsets in seconds.

- method:

  Exact fitting method.

- origin:

  Exact centering rule.

- segments:

  Optional positive labels or zero-based inclusive ranges.

- huber_k:

  Positive Huber tuning constant.

- ransac_threshold:

  Optional positive inlier threshold in seconds.

- ransac_trials:

  Positive number of deterministic RANSAC trials.

- seed:

  Non-negative deterministic RANSAC seed.

## Value

A serializable `StreamClockModel`.

## Examples

``` r
device <- seq(0, 10, by = 0.5)
offset <- -0.4 + 0.005 * (device - median(device))
model <- clockOffset(device, offset, method = "ols")
model$segments$drift_ppm
#> [1] 5000
```
