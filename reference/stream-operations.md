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

- timeout:

  Optional finite non-negative LSL inlet open timeout.

- ...:

  Backend-specific arguments.

## Value

Backend-specific output.
