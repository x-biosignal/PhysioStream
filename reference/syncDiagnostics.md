# Summarize governed stream synchronization

Returns stored diagnostics for a synchronized container. For an
unsynchronized container, models or shared events are evaluated through
[`syncStreams()`](https://x-biosignal.github.io/PhysioStream/reference/syncStreams.md)
on a copy and only the diagnostics table is returned.

## Usage

``` r
syncDiagnostics(x, models = NULL, shared_events = NULL)
```

## Arguments

- x:

  A
  [`PhysioCore::MultiRatePhysioExperiment`](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/MultiPhysioExperiment.html).

- models:

  Optional uniquely named `StreamClockModel` list.

- shared_events:

  Optional exact `event_id`, `stream`, `timestamp` table.

## Value

One plain data-frame row per stream/reset segment.
