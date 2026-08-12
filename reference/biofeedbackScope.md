# Construct and optionally launch a governed live biofeedback scope

Construction is side-effect-free. With `launch = FALSE`, the returned
runtime is advanced synchronously with
[`biofeedbackStart()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-lifecycle.md)
and
[`biofeedbackStep()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-lifecycle.md).
Shiny is required only for `launch = TRUE`.

## Usage

``` r
biofeedbackScope(
  source,
  pipeline = NULL,
  channels = NULL,
  derived = list(),
  window_seconds = 10,
  update_hz = 20,
  max_points = 2000L,
  gain = 1,
  threshold = NULL,
  target_range = NULL,
  source_lifecycle = c("borrow", "own"),
  launch = interactive(),
  host = "127.0.0.1",
  port = NULL,
  browser = interactive(),
  clock = NULL
)
```

## Arguments

- source:

  A regular-rate numeric `StreamSource`.

- pipeline:

  Optional source-backed `StreamPipeline`.

- channels:

  Exact source channel names to display.

- derived:

  Strictly named derived/external trace descriptors.

- window_seconds:

  Positive visible signal-time window.

- update_hz:

  Requested display update rate.

- max_points:

  Maximum plotted representatives per trace.

- gain:

  Positive display-only gain.

- threshold:

  Optional finite display threshold.

- target_range:

  Optional increasing display target range.

- source_lifecycle:

  Whether the scope borrows an open source or owns its open/close
  lifecycle.

- launch:

  Whether to run the installed Shiny app immediately.

- host:

  Local Shiny bind host.

- port:

  Optional exact TCP port.

- browser:

  Whether Shiny should launch a browser.

- clock:

  Optional monotonic nanosecond clock for deterministic tests.

## Value

A `BiofeedbackScope` when `launch = FALSE`, otherwise the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
