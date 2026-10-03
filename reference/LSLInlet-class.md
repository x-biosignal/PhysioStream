# Lab Streaming Layer inlet

Live backend and buffer state are intentionally reference-like and do
not survive serialization.

## Slots

- `runtime`:

  Private live backend state.

- `buffer`:

  Numeric ring buffer or bounded marker queue.

- `max_chunk`:

  Maximum rows requested per pull.

- `recover`:

  Whether liblsl source recovery is enabled.

- `processing`:

  Exact timestamp processing mode.

- `marker_capacity`:

  Marker queue capacity.

## Examples

``` r
# Concrete inlet objects come from lslInlet(); see ?lslInlet.
isVirtualClass("LSLInlet")
#> [1] FALSE
```
