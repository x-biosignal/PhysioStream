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

## Examples

``` r
# Cancel a reference-correlated component from a short synthetic signal.
set.seed(1)
reference <- sin(2 * pi * 5 * seq_len(200) / 100)
signal <- 0.4 * reference + rnorm(200, sd = 0.05)
filt <- lmsFilter(n_taps = 4L, step_size = 0.05)
result <- update(filt, matrix(signal, ncol = 1),
                 reference = matrix(reference, ncol = 1))
str(result$output)
#>  num [1:200, 1] 0.0923 0.2435 0.2727 0.4295 0.3312 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "channel_1"
```
