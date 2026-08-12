# Launch a synchronized video and signal viewer

`launch = FALSE` returns a side-effect-free `BiofeedbackScope` carrying
a validated video descriptor. Local media are copied and exposed only
when the Shiny viewer starts.

## Usage

``` r
videoSyncViewer(
  source,
  video,
  sync,
  pipeline = NULL,
  channels = NULL,
  window_seconds = 10,
  update_hz = 20,
  max_points = 2000L,
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

- video:

  Local mp4/webm/ogg path, HTTPS URL, or `"webcam"`.

- sync:

  A `VideoSync` in the source clock domain.

- pipeline:

  Optional source-backed `StreamPipeline`.

- channels:

  Exact source channel names to display.

- window_seconds:

  Positive visible signal-time window.

- update_hz:

  Requested display update rate.

- max_points:

  Maximum plotted representatives per trace.

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

A `BiofeedbackScope` or the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
