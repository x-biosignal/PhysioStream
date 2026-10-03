# Pull one LSL chunk into an inlet buffer

Pull one LSL chunk into an inlet buffer

## Usage

``` r
lslPull(x, max_samples = x@max_chunk, timeout = 0)
```

## Arguments

- x:

  An open `LSLInlet`.

- max_samples:

  Exact number of samples requested, at most the inlet chunk bound.

- timeout:

  Finite non-negative backend timeout.

## Value

A serializable pull summary. Payload remains in the bounded inlet buffer
or marker queue.

## Examples

``` r
# \donttest{
# Requires an open inlet bound to a live LSL stream.
if (lslAvailable()) {
  found <- lslResolveStreams(timeout = 0.2)
  if (length(found)) {
    inlet <- streamOpen(lslInlet(found[[1]]))
    summary <- lslPull(inlet, max_samples = 32)
    streamClose(inlet)
  }
}
# }
```
