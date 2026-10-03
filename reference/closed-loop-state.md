# Inspect governed closed-loop state and audit

Returned values are deep portable copies. They contain no runtime
object, trigger credential, patient identifier, or raw physiological
samples.

## Usage

``` r
closedLoopState(controller)

closedLoopLog(controller)
```

## Arguments

- controller:

  A `ClosedLoopController`.

## Value

`closedLoopState()` returns the sealed state; `closedLoopLog()` returns
its bounded structured session log.

## Examples

``` r
pipeline <- streamPipeline(chunk_size = 2048L)
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
detector <- emgOnsetOp("emg", sampling_rate = 1000,
                       baseline_samples = 30, rms_window_samples = 5)
controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
                         stim_channel = "left", duration_ms = 10)
closedLoopState(controller)$lifecycle$status
#> [1] "constructed"
closedLoopLog(controller)
#> list()
```
