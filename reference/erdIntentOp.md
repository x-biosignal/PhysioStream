# Construct a causal streaming EEG ERD detector

A fixed causal biquad and trailing power window estimate band-power
change from a frozen baseline. The result is an ERD proxy, not a
clinical movement-intention diagnosis. Explicit signal-timestamp
intervals must remain within five percent of the declared sampling
interval.

## Usage

``` r
erdIntentOp(
  channel,
  sampling_rate,
  band = c(8, 13),
  baseline_samples,
  power_window_samples,
  enter_percent = -30,
  release_percent = -15,
  min_on_samples = 3L,
  min_off_samples = 3L,
  min_baseline_power = 1e-12
)
```

## Arguments

- channel:

  Exact input channel name.

- sampling_rate:

  Sampling rate in hertz.

- band:

  Two strictly increasing band edges in hertz.

- baseline_samples:

  Number of complete power windows in the baseline.

- power_window_samples:

  Trailing causal mean-square window length.

- enter_percent, release_percent:

  ERD enter/release percentages.

- min_on_samples, min_off_samples:

  Consecutive confirmation counts.

- min_baseline_power:

  Minimum accepted frozen baseline power.

## Value

A governed `PipelineOperation` detector descriptor.

## Examples

``` r
detector <- erdIntentOp("eeg", sampling_rate = 1000, band = c(8, 13),
                        baseline_samples = 40, power_window_samples = 10)
class(detector)
#> [1] "PipelineOperation"
```
