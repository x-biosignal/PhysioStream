# Run a governed closed-loop session

The controller is synchronous and terminal. Start explicitly opens and
arms its trigger. Step processes one committed chunk and then dispatches
due actions. Flush dispatches due delayed actions without ingesting
samples. Stop and emergency stop suppress pending actions and close the
trigger.

## Usage

``` r
closedLoopStart(controller, session_id, now_ns = NULL)

closedLoopStep(controller, samples, timestamps = NULL, now_ns = NULL)

closedLoopFlush(controller, now_ns = NULL)

closedLoopStop(controller, reason = "caller", now_ns = NULL)

closedLoopEmergencyStop(controller, reason = "caller", now_ns = NULL)
```

## Arguments

- controller:

  A `ClosedLoopController`.

- session_id:

  New bounded trigger session identifier.

- now_ns:

  Optional exact test monotonic nanosecond value.

- samples:

  Finite sample-by-channel chunk.

- timestamps:

  Optional signal-domain timestamps.

- reason:

  Bounded non-sensitive stop reason.

## Value

Start/stop functions return `controller` invisibly. Step and flush
return bounded plain result lists.

## Examples

``` r
set.seed(1)
pipeline <- streamPipeline(chunk_size = 2048L)
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
detector <- emgOnsetOp("emg", sampling_rate = 1000,
                       baseline_samples = 30, rms_window_samples = 5)
controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
                         stim_channel = "left", duration_ms = 10)
closedLoopStart(controller, "demo-session", now_ns = 0)
emg <- rnorm(100, sd = 0.08)
result <- closedLoopStep(controller,
  matrix(emg, ncol = 1, dimnames = list(NULL, "emg")),
  timestamps = (seq_along(emg) - 1) / 1000, now_ns = 1e6)
length(result$actions)
#> [1] 0
closedLoopStop(controller, now_ns = 2e6)
```
