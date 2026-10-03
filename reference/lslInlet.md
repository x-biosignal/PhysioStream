# Construct an LSL inlet

Construction validates R state only. Assign the result of
[`streamOpen()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
to open the already configured backend.

## Usage

``` r
lslInlet(
  info,
  capacity = 4096L,
  max_chunk = 1024L,
  recover = TRUE,
  processing = c("none", "clocksync"),
  marker_capacity = 4096L,
  backend = getOption("PhysioStream.lsl_backend", "auto")
)
```

## Arguments

- info:

  A resolved `StreamInfo`.

- capacity:

  Numeric ring capacity.

- max_chunk:

  Maximum rows per backend pull.

- recover:

  Whether liblsl may recover by non-empty source id.

- processing:

  Exact timestamp processing mode.

- marker_capacity:

  String marker queue capacity.

- backend:

  Exact backend, currently `"auto"` or `"pylsl"`.

## Value

An `LSLInlet` in state `"created"`.

## Examples

``` r
# \donttest{
# Requires a running LSL stream on the local network.
if (lslAvailable()) {
  found <- lslResolveStreams(timeout = 0.2)
  if (length(found)) {
    inlet <- streamOpen(lslInlet(found[[1]]))
    streamClose(inlet)
  }
}
# }
```
