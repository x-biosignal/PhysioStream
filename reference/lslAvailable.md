# Test Lab Streaming Layer backend availability

The default probe is conservative and does not initialize Python.
Explicit transport operations initialize only the already configured
reticulate interpreter; no function installs or selects an environment.

## Usage

``` r
lslAvailable(backend = c("auto", "pylsl"), initialize = FALSE)
```

## Arguments

- backend:

  Exact backend, currently `"auto"` or `"pylsl"`.

- initialize:

  Whether the configured Python interpreter may be initialized to verify
  that pylsl and liblsl load.

## Value

One logical value.

## Examples

``` r
# Reports FALSE unless pylsl and liblsl are installed and loadable.
lslAvailable()
#> [1] FALSE
```
