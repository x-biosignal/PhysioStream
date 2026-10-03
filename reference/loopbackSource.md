# Construct a loopback source

Construct a loopback source

## Usage

``` r
loopbackSource(info, capacity = 1024L)
```

## Arguments

- info:

  Numeric `StreamInfo`.

- capacity:

  Exact positive ring capacity.

## Value

A closed-over loopback source in the `"created"` state.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
src <- streamOpen(loopbackSource(info, capacity = 32L))
streamState(src)
#> [1] "open"
```
