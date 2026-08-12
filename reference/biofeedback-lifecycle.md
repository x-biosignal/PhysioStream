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
