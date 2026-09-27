# Convert buffered LSL markers to PhysioEvents

Marker timestamps remain in the LSL clock domain unless an explicit
`time_origin` is subtracted. Cross-stream alignment is handled
separately.

## Usage

``` r
lslMarkerEvents(x, n = NULL, consume = FALSE, time_origin = 0, type = NULL)
```

## Arguments

- x:

  An open string/irregular-rate `LSLInlet`.

- n:

  Optional newest marker count.

- consume:

  Whether to consume the complete oldest selection.

- time_origin:

  Finite scalar subtracted from LSL timestamps.

- type:

  Optional event type; defaults to the stream type.

## Value

A valid
[`PhysioCore::PhysioEvents`](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioEvents.html).
