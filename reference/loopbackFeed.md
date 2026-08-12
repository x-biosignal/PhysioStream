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
