# Stateful least-mean-squares adaptive filter

Adaptive filters estimate the part of each signal channel linearly
predictable from its paired reference. The returned residual is not
inherently a clean physiological signal; that interpretation requires a
caller-owned reference-noise assumption.

## Usage

``` r
lmsFilter(n_taps, step_size, leakage = 0, initial_weights = NULL)
```

## Arguments

- n_taps:

  Exact adaptive-filter order.

- step_size:

  Positive LMS step size.

- leakage:

  Weight leakage in `[0, 1)`.

- initial_weights:

  Optional finite `n_taps` by channel matrix.

## Value

A mutable `LMSFilter` streaming processor.
