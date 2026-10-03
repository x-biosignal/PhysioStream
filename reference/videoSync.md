# Fit a governed marker-anchored video/signal time map

The mapping convention is
`video_seconds = intercept + rate * signal_seconds`. Evaluation uses an
equivalent centered representation to retain precision for large clocks.

## Usage

``` r
videoSync(
  signal_time,
  video_time,
  frame_rate,
  method = c("offset", "affine"),
  clock_domain,
  tolerance_frames = 1,
  max_rate_error_ppm = 5000
)
```

## Arguments

- signal_time:

  Strictly increasing signal-domain marker times.

- video_time:

  Strictly increasing corresponding media times.

- frame_rate:

  Positive media frames per second.

- method:

  Exact offset-only or affine fit.

- clock_domain:

  Exact signal clock-domain label.

- tolerance_frames:

  Maximum accepted anchor residual in frames.

- max_rate_error_ppm:

  Maximum affine rate deviation from one.

## Value

A sealed portable `VideoSync`.

## Examples

``` r
sync <- videoSync(
  signal_time = c(a = 100, b = 110, c = 120),
  video_time = c(a = 1, b = 11, c = 21),
  frame_rate = 30, method = "offset", clock_domain = "demo")
videoSyncTime(sync, c(x = 105))
#> x 
#> 6 
```
