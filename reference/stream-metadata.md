# Stream metadata accessors

Stream metadata accessors

## Usage

``` r
streamName(x)

streamType(x)

streamChannels(x)

streamRate(x)

streamDtype(x)
```

## Arguments

- x:

  A `StreamInfo`, endpoint, or ring buffer.

## Value

The requested metadata field.

## Examples

``` r
info <- streamInfo("eeg-demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 250,
                   dtype = "float32")
streamName(info)
#> [1] "eeg-demo"
streamType(info)
#> [1] "EEG"
streamChannels(info)
#> [1] "C3" "C4"
streamRate(info)
#> [1] 250
streamDtype(info)
#> [1] "float32"
```
