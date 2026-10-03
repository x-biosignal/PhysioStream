# Virtual stream endpoint

Virtual stream endpoint

## Slots

- `info`:

  Immutable `StreamInfo`.

- `state`:

  Exact lifecycle state.

- `audit`:

  Append-only state transition records.

## Examples

``` r
# StreamEndpoint is virtual; a loopback source is a concrete endpoint.
info <- streamInfo("demo", type = "EEG",
                   channel_names = "C3", nominal_srate = 100)
src <- loopbackSource(info)
is(src, "StreamEndpoint")
#> [1] TRUE
streamState(src)
#> [1] "created"
```
