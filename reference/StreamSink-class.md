# Virtual stream sink

Transport backends extend this class and the stream-operation generics.

## Examples

``` r
# StreamSink is virtual; transport outlets such as lslOutlet() extend it.
isVirtualClass("StreamSink")
#> [1] TRUE
```
