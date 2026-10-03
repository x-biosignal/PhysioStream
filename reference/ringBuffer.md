# Construct a native ring buffer

Construct a native ring buffer

## Usage

``` r
ringBuffer(info, capacity)
```

## Arguments

- info:

  Numeric `StreamInfo`.

- capacity:

  Exact positive sample capacity.

## Value

A live `RingBuffer`.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 16L)
ringCapacity(buffer)
#> [1] 16
```
