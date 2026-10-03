# Start, step, and stop a biofeedback runtime

Start, step, and stop a biofeedback runtime

## Usage

``` r
biofeedbackStart(scope)

biofeedbackStep(scope, max_chunks = 16L)

biofeedbackStop(scope)
```

## Arguments

- scope:

  A `BiofeedbackScope`.

- max_chunks:

  Exact per-step work bound.

## Value

Lifecycle functions return `scope` invisibly. `biofeedbackStep()`
returns a plain update summary.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("left", "right"), nominal_srate = 100,
                   channel_units = c("uV", "uV"))
source <- streamOpen(loopbackSource(info, capacity = 4096L))
scope <- biofeedbackScope(source, update_hz = 20, window_seconds = 1,
                          max_points = 64L, launch = FALSE)
biofeedbackStart(scope)
loopbackFeed(source, matrix(as.double(1:40), 20, 2), seq_len(20) / 100)
invisible(biofeedbackStep(scope))
biofeedbackStop(scope)
biofeedbackState(scope)$lifecycle
#> [1] "stopped"
```
