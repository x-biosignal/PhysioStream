# Resolve visible Lab Streaming Layer streams

pylsl resolution returns short descriptors. PhysioStream therefore opens
a bounded, non-recovering metadata inlet for each result, retrieves its
full descriptor, and closes the temporary inlet before returning. No
samples are pulled.

## Usage

``` r
lslResolveStreams(
  property = NULL,
  value = NULL,
  minimum = 0L,
  timeout = 1,
  backend = getOption("PhysioStream.lsl_backend", "auto")
)
```

## Arguments

- property, value:

  Either both `NULL`, or an exact core LSL property and one non-empty
  value.

- minimum:

  Exact minimum number of descriptors required.

- timeout:

  Finite non-negative resolver timeout in seconds.

- backend:

  Exact backend, currently `"auto"` or `"pylsl"`.

## Value

A list of validated `StreamInfo` objects in backend order.
