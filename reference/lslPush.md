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

## Examples

``` r
# \donttest{
# Requires the LSL runtime (pylsl/liblsl).
if (lslAvailable()) {
  info <- streamInfo("demo", type = "EEG",
                     channel_names = c("C3", "C4"), nominal_srate = 100)
  outlet <- streamOpen(lslOutlet(info))
  lslPush(outlet, matrix(as.double(1:4), 2, 2))
  streamClose(outlet)
}
# }
```
