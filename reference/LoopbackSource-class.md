# Deterministic in-process stream source

Deterministic in-process stream source

## Slots

- `buffer`:

  Live native ring buffer.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
src <- loopbackSource(info, capacity = 16L)
is(src, "LoopbackSource")
#> [1] TRUE
```
