# Push samples or markers through an LSL outlet

Push samples or markers through an LSL outlet

## Usage

``` r
lslPush(x, samples, timestamps = NULL, pushthrough = TRUE)
```

## Arguments

- x:

  An open `LSLOutlet`.

- samples:

  A strict sample-by-channel numeric or character matrix.

- timestamps:

  Optional explicit timestamp per row.

- pushthrough:

  Whether the final backend operation flushes the chunk.

## Value

A serializable push summary.
