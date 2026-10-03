# Test XDF backend availability

The default probe is conservative and does not initialize Python.
Explicit read operations initialize only the already configured
reticulate interpreter. PhysioStream never installs or selects a Python
environment.

## Usage

``` r
xdfAvailable(backend = c("auto", "pyxdf"), initialize = FALSE)
```

## Arguments

- backend:

  Exact backend, currently `"auto"` or `"pyxdf"`.

- initialize:

  Whether the configured Python interpreter may be initialized to verify
  pyxdf and NumPy.

## Value

One non-missing logical value.

## Examples

``` r
# Reports FALSE unless the pyxdf backend is installed.
xdfAvailable()
#> [1] FALSE
```
