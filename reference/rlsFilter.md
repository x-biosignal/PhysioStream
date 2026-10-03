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

## Examples

``` r
set.seed(1)
reference <- cos(2 * pi * 6 * seq_len(150) / 100)
signal <- 0.5 * reference + rnorm(150, sd = 0.05)
filt <- rlsFilter(n_taps = 3L, forgetting = 0.995)
result <- update(filt, matrix(signal, ncol = 1),
                 reference = matrix(reference, ncol = 1))
length(result$output)
#> [1] 150
```
