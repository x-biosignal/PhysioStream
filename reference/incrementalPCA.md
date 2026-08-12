# Incremental principal-component analysis

For `forgetting = 1`, the processor retains Chan-Golub-LeVeque
sufficient statistics and is batch-equivalent without retaining samples.
Smaller forgetting factors produce an explicitly exponentially weighted
covariance.

## Usage

``` r
incrementalPCA(n_components, n_features = NULL, forgetting = 1, center = TRUE)
```

## Arguments

- n_components:

  Number of retained components.

- n_features:

  Optional feature count to bind at construction.

- forgetting:

  Per-sample covariance forgetting factor in `(0, 1]`.

- center:

  Whether to estimate and subtract a running mean.

## Value

A mutable `IncrementalPCA` streaming processor.
