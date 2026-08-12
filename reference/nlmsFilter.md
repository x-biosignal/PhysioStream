# Stateful normalized LMS adaptive filter

Stateful normalized LMS adaptive filter

## Usage

``` r
nlmsFilter(
  n_taps,
  step_size = 0.5,
  epsilon = 1e-08,
  leakage = 0,
  initial_weights = NULL
)
```

## Arguments

- n_taps:

  Exact adaptive-filter order.

- step_size:

  Positive normalized step size; values in `(0, 2)` are the conventional
  stable range.

- epsilon:

  Positive denominator regularizer.

- leakage:

  Weight leakage in `[0, 1)`.

- initial_weights:

  Optional finite `n_taps` by channel matrix.

## Value

A mutable `NLMSFilter` streaming processor.
