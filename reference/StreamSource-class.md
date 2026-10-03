# Virtual stream source

Transport backends extend this class and the stream-operation generics.

## Examples

``` r
# StreamSource is virtual; loopbackSource() returns a concrete instance.
info <- streamInfo("demo", type = "EEG",
                   channel_names = "C3", nominal_srate = 100)
is(loopbackSource(info), "StreamSource")
#> [1] TRUE
```
