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
