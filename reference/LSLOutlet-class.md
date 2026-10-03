# Lab Streaming Layer outlet

Lab Streaming Layer outlet

## Slots

- `runtime`:

  Private live backend state.

- `chunk_size`:

  Preferred LSL chunk size.

- `max_buffered`:

  LSL sender buffer bound.

## Examples

``` r
# Concrete outlet objects come from lslOutlet(); see ?lslOutlet.
isVirtualClass("LSLOutlet")
#> [1] FALSE
```
