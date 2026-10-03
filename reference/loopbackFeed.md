# Feed a loopback source

Feed a loopback source

## Usage

``` r
loopbackFeed(x, samples, timestamps)
```

## Arguments

- x:

  An open `LoopbackSource`.

- samples:

  A finite sample-by-channel real matrix.

- timestamps:

  Strictly increasing finite timestamps.

## Value

`x`, invisibly. The native buffer is modified by reference.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
src <- streamOpen(loopbackSource(info, capacity = 32L))
loopbackFeed(src, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
streamPull(src)$count
#> [1] 3
```
