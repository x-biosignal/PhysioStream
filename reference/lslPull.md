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
