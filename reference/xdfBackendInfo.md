# Report the configured XDF backend

Report the configured XDF backend

## Usage

``` r
xdfBackendInfo(backend = c("auto", "pyxdf"))
```

## Arguments

- backend:

  Exact backend, currently `"auto"` or `"pyxdf"`.

## Value

A serializable named list of backend and runtime versions.

## Examples

``` r
# \donttest{
# Resolving the backend version requires the pyxdf backend.
if (xdfAvailable()) str(xdfBackendInfo())
# }
```
