# Stream endpoint lifecycle and transport operations

Transport packages extend these generics. Base virtual endpoints provide
no implicit transport.

## Usage

``` r
streamOpen(x, ...)

streamClose(x, ...)

streamPull(x, ...)

streamPush(x, ...)

streamState(x)

# S4 method for class 'StreamEndpoint'
streamState(x)

# S4 method for class 'StreamEndpoint'
streamOpen(x, ...)

# S4 method for class 'StreamEndpoint'
streamClose(x, ...)

# S4 method for class 'StreamEndpoint'
streamPull(x, ...)

# S4 method for class 'StreamEndpoint'
streamPush(x, ...)

# S4 method for class 'LoopbackSource'
streamOpen(x, ...)

# S4 method for class 'LoopbackSource'
streamClose(x, ...)

# S4 method for class 'LoopbackSource'
streamPull(x, ...)

# S4 method for class 'LoopbackSource'
streamPush(x, ...)

# S4 method for class 'LSLInlet'
streamState(x)

# S4 method for class 'LSLOutlet'
streamState(x)

# S4 method for class 'LSLInlet'
streamOpen(x, timeout = 2, ...)

# S4 method for class 'LSLOutlet'
streamOpen(x, ...)

# S4 method for class 'LSLInlet'
streamClose(x, ...)

# S4 method for class 'LSLOutlet'
streamClose(x, ...)

# S4 method for class 'LSLInlet'
streamPull(x, ...)

# S4 method for class 'LSLOutlet'
streamPush(x, ...)
```

## Arguments

- x:

  A stream endpoint.

- ...:

  Backend-specific arguments.

- timeout:

  Optional finite non-negative LSL inlet open timeout.

## Value

Backend-specific output.

## Examples

``` r
# Drive the lifecycle on a device-free loopback source.
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
src <- streamOpen(loopbackSource(info, capacity = 16L))
streamState(src)
#> [1] "open"
loopbackFeed(src, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
streamPull(src)$samples
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
src <- streamClose(src)
streamState(src)
#> [1] "closed"
```
