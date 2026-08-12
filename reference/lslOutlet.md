# Construct an LSL outlet

Construct an LSL outlet

## Usage

``` r
lslOutlet(
  info,
  chunk_size = 0L,
  max_buffered = 360L,
  backend = getOption("PhysioStream.lsl_backend", "auto")
)
```

## Arguments

- info:

  A valid numeric regular-rate or string irregular-rate `StreamInfo`.

- chunk_size:

  Exact non-negative LSL chunk preference.

- max_buffered:

  Exact positive LSL sender buffer bound.

- backend:

  Exact backend, currently `"auto"` or `"pylsl"`.

## Value

An `LSLOutlet` in state `"created"`.
