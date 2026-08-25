# Construct a governed detector-to-stimulation controller

`closedLoop()` is side-effect free with respect to the trigger
transport. It registers one governed detector operation in the supplied
pipeline and owns that graph and trigger for a terminal session
lifecycle. Detector events are decoded only after
[`pipelineStep()`](https://x-biosignal.github.io/PhysioStream/reference/pipelineStep.md)
commits; callbacks never stimulate.

## Usage

``` r
closedLoop(
  pipeline,
  trigger,
  detector,
  intensity,
  stim_channel,
  duration_ms,
  delay_ms = 0,
  event_refractory_ms = 0,
  pending_capacity = 128L,
  log_capacity = 4096L,
  clock = NULL
)
```

## Arguments

- pipeline:

  An empty-queue `StreamPipeline`.

- trigger:

  A closed, disarmed `TriggerBackend`.

- detector:

  An
  [`emgOnsetOp()`](https://x-biosignal.github.io/PhysioStream/reference/emgOnsetOp.md),
  [`erdIntentOp()`](https://x-biosignal.github.io/PhysioStream/reference/erdIntentOp.md),
  or
  [`phaseTargetOp()`](https://x-biosignal.github.io/PhysioStream/reference/phaseTargetOp.md)
  descriptor.

- intensity, stim_channel, duration_ms:

  Explicit stimulation dose fields.

- delay_ms:

  Detection-commit to attempt delay. Positive delays require a later
  step or
  [`closedLoopFlush()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md);
  no sleep or background task is used.

- event_refractory_ms:

  Controller-level detection refractory period.

- pending_capacity:

  Maximum delayed actions.

- log_capacity:

  Maximum retained audit records and detector identities.

- clock:

  Optional deterministic monotonic clock, for loopback tests.

## Value

A closed `ClosedLoopController`.
