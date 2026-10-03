# Inspect a biofeedback frame or portable state

Inspect a biofeedback frame or portable state

## Usage

``` r
biofeedbackFrame(scope)

biofeedbackState(scope)
```

## Arguments

- scope:

  A `BiofeedbackScope`.

## Value

A deep plain-list copy, or `NULL` before the first frame.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("left", "right"), nominal_srate = 100,
                   channel_units = c("uV", "uV"))
source <- loopbackSource(info, capacity = 256L)
scope <- biofeedbackScope(source, source_lifecycle = "own", launch = FALSE)
biofeedbackState(scope)$lifecycle
#> [1] "created"
biofeedbackFrame(scope)
#> NULL
```
