# Construct the compiled causal SOS plus rolling-RMS operation

The operation uses direct-form-II-transposed second-order sections
followed by a per-channel rolling root-mean-square. It is a descriptor
for
[`onChunk()`](https://x-biosignal.github.io/PhysioStream/reference/onChunk.md)
and performs no processing until registered and stepped.

## Usage

``` r
bandpassRmsOp(
  sos,
  window_samples,
  warmup = c("partial", "complete"),
  name = "bandpass_rms"
)
```

## Arguments

- sos:

  Finite second-order-section matrix with columns `b0,b1,b2,a0,a1,a2`.

- window_samples:

  Exact positive RMS window length.

- warmup:

  Whether to emit partial-window RMS values or only rows after a
  complete window exists.

- name:

  Default operation name.

## Value

A `PipelineOperation` descriptor.
