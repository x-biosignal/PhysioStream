# Synchronize a multi-rate stream container

Applies explicit clock models or recorded XDF/shared-event evidence
without interpolating sample values. Authoritative raw and master-domain
timestamps are appended to each stream's row data.

## Usage

``` r
syncStreams(
  x,
  master = NULL,
  models = NULL,
  shared_events = NULL,
  dejitter = FALSE,
  max_residual_seconds = NULL
)
```

## Arguments

- x:

  A
  [`PhysioCore::MultiRatePhysioExperiment`](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/MultiPhysioExperiment.html).

- master:

  Exact master stream key, or `NULL` for deterministic choice.

- models:

  Optional uniquely named `StreamClockModel` list.

- shared_events:

  Optional exact `event_id`, `stream`, `timestamp` table.

- dejitter:

  Whether to append a nominal-grid timestamp column.

- max_residual_seconds:

  Optional non-negative residual gate.

## Value

A synchronized
[`PhysioCore::MultiRatePhysioExperiment`](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/MultiPhysioExperiment.html).
