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

## Examples

``` r
set.seed(1)
reference <- sin(2 * pi * 8 * seq_len(200) / 100)
signal <- 0.3 * reference + rnorm(200, sd = 0.05)
filt <- nlmsFilter(n_taps = 4L, step_size = 0.5)
result <- update(filt, matrix(signal, ncol = 1),
                 reference = matrix(reference, ncol = 1))
tail(result$output)
#>          channel_1
#> [195,] -0.08781831
#> [196,] -0.04421128
#> [197,]  0.09448897
#> [198,] -0.09282238
#> [199,]  0.01511049
#> [200,] -0.03894647
```
