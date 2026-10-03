# Map between synchronized signal and video time

Map between synchronized signal and video time

## Usage

``` r
videoSyncTime(sync, signal_time)

signalSyncTime(sync, video_time)
```

## Arguments

- sync:

  A validated `VideoSync`.

- signal_time:

  Finite signal-domain times.

- video_time:

  Finite media times.

## Value

A numeric vector preserving names.

## Examples

``` r
sync <- videoSync(c(a = 100, b = 110, c = 120), c(a = 1, b = 11, c = 21),
                  frame_rate = 30, method = "offset", clock_domain = "demo")
videoSyncTime(sync, c(x = 105, y = 115))
#>  x  y 
#>  6 16 
signalSyncTime(sync, c(x = 6, y = 16))
#>   x   y 
#> 105 115 
```
