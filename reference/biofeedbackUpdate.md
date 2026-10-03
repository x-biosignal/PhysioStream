# Publish a bounded external feedback update

This is the sink used by downstream live metric adapters. It updates
only declared external display traces and has no pipeline or stimulation
effect.

## Usage

``` r
biofeedbackUpdate(
  scope,
  values,
  timestamp,
  names = base::names(values),
  units = NULL,
  sequence = NULL
)
```

## Arguments

- scope:

  A running `BiofeedbackScope`.

- values:

  Finite named values.

- timestamp:

  One signal-domain timestamp.

- names:

  Exact names corresponding to `values`.

- units:

  Optional exact units corresponding to `values`.

- sequence:

  Optional exact contiguous update sequence.

## Value

An immutable plain receipt.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("left", "right"), nominal_srate = 100,
                   channel_units = c("uV", "uV"))
source <- streamOpen(loopbackSource(info, capacity = 4096L))
scope <- biofeedbackScope(source, derived = list(
  score = list(type = "external", unit = "ratio", gain = 1)
), launch = FALSE)
biofeedbackStart(scope)
receipt <- biofeedbackUpdate(scope, c(score = 0.8), timestamp = 1,
                             units = "ratio", sequence = 0)
receipt$names
#> [1] "score"
biofeedbackStop(scope)
```
