# Construct a causal phase-target detector

The detector fits a sinusoid at a fixed frequency using only the
trailing window ending at each current sample.
[`closedLoop()`](https://x-biosignal.github.io/PhysioStream/reference/closedLoop.md)
binds its configured detection-to-stimulation delay before registration
so the event reports the predicted phase at the due time. Explicit
signal-timestamp intervals must remain within five percent of the
declared sampling interval.

## Usage

``` r
phaseTargetOp(
  channel,
  sampling_rate,
  frequency,
  target_degrees,
  window_cycles = 3,
  tolerance_degrees = 20,
  min_amplitude = 0,
  min_fit = 0.8
)
```

## Arguments

- channel:

  Exact input channel name.

- sampling_rate:

  Sampling rate in hertz.

- frequency:

  Target oscillation frequency in hertz.

- target_degrees:

  Requested phase in `[0, 360)`, modulo 360.

- window_cycles:

  Number of trailing cycles in the causal fit.

- tolerance_degrees:

  Maximum accepted predicted signed phase error.

- min_amplitude:

  Minimum fitted oscillation amplitude.

- min_fit:

  Minimum coefficient of determination for the sinusoid fit.

## Value

A governed `PipelineOperation` detector descriptor.
