# Report the configured LSL backend

Report the configured LSL backend

## Usage

``` r
lslBackendInfo(backend = c("auto", "pylsl"))
```

## Arguments

- backend:

  Exact backend, currently `"auto"` or `"pylsl"`.

## Value

A serializable named list of backend and runtime versions.

## Examples

``` r
# \donttest{
# Resolving the backend version requires pylsl/liblsl to be installed.
if (lslAvailable()) str(lslBackendInfo())
# }
```
