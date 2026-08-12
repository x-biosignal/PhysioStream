# Stateful recursive-least-squares adaptive filter

Stateful recursive-least-squares adaptive filter

## Usage

``` r
rlsFilter(n_taps, forgetting = 0.99, delta = 1, initial_weights = NULL)
```

## Arguments

- n_taps:

  Exact adaptive-filter order.

- forgetting:

  Forgetting factor in `(0, 1]`.

- delta:

  Positive initial inverse-covariance scale.

- initial_weights:

  Optional finite `n_taps` by channel matrix.

## Value

A mutable `RLSFilter` streaming processor.
