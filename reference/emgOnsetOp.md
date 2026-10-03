# Construct a causal streaming EMG-onset detector

The detector uses a trailing RMS envelope, a frozen warm-up baseline,
and explicit enter/release hysteresis. It is a pure
[`onChunk()`](https://x-biosignal.github.io/PhysioStream/reference/onChunk.md)
operation: stimulation is attempted only by
[`closedLoopStep()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
after pipeline commit. Explicit signal-timestamp intervals must remain
within five percent of the declared sampling interval.

## Usage

``` r
emgOnsetOp(
  channel,
  sampling_rate,
  baseline_samples,
  rms_window_samples,
  enter_z = 5,
  release_z = 2,
  min_on_samples = 3L,
  min_off_samples = 3L,
  min_baseline_sd = 1e-08
)
```

## Arguments

- channel:

  Exact input channel name.

- sampling_rate:

  Sampling rate in hertz.

- baseline_samples:

  Number of complete-window envelope samples used for the frozen
  baseline.

- rms_window_samples:

  Trailing causal RMS window length.

- enter_z, release_z:

  Baseline-standardized enter/release thresholds.

- min_on_samples, min_off_samples:

  Consecutive confirmation counts.

- min_baseline_sd:

  Minimum accepted baseline scale.

## Value

A governed `PipelineOperation` detector descriptor.

## Examples

``` r
set.seed(1)
emg <- c(rnorm(80, sd = 0.08), rep(1.5, 12), rnorm(30, sd = 0.08))
detector <- emgOnsetOp("emg", sampling_rate = 1000,
                       baseline_samples = 50, rms_window_samples = 8)
pipeline <- streamPipeline(chunk_size = length(emg))
onChunk(pipeline, detector)
pipelineEnqueue(pipeline,
                matrix(emg, ncol = 1, dimnames = list(NULL, "emg")),
                seq_along(emg) / 1000)
length(pipelineStep(pipeline)$results[[1]]$events)
#> [1] 1
```
