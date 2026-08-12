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
