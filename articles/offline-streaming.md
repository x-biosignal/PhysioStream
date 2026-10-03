# An offline streaming workflow

PhysioStream governs *live* physiological data streams: Lab Streaming
Layer inlets and outlets, XDF interchange, MQTT/TTL stimulation
triggers, and a live biofeedback scope. Those transports need a device
or a running network and are not exercised here.

Everything else runs **in process, with no device**. This vignette walks
the device-free path end to end on a tiny synthetic signal: stream
metadata, the deterministic loopback source, a snapshot to a
`PhysioExperiment`, a synchronous processing pipeline, online DSP, clock
correction, and a governed stimulation trigger. Every code block below
is executed when the vignette is built.

``` r

library(PhysioStream)
#> Loading required package: PhysioExperiment
```

## Stream metadata

A `StreamInfo` is validated, immutable, and serializable. It is the
single source of truth for channel identity, rate, and storage type.

``` r

info <- streamInfo(
  "demo-eeg",
  type = "EEG",
  channel_names = c("C3", "C4"),
  nominal_srate = 100,
  channel_units = c("uV", "uV")
)
info
#> StreamInfo<1.0.0>: demo-eeg
#>   type: EEG; channels: 2; rate: 100 Hz; dtype: float64
streamChannels(info)
#> [1] "C3" "C4"
```

## A deterministic loopback stream

[`loopbackSource()`](https://x-biosignal.github.io/PhysioStream/reference/loopbackSource.md)
is an in-process `StreamSource` backed by a bounded native ring buffer.
It exercises the same lifecycle (`created` -\> `open` -\> `closed`) and
the same sample/timestamp governance as a live transport, but you feed
it yourself with
[`loopbackFeed()`](https://x-biosignal.github.io/PhysioStream/reference/loopbackFeed.md).

``` r

source <- streamOpen(loopbackSource(info, capacity = 256L))
streamState(source)
#> [1] "open"

n <- 50
t <- seq_len(n) / 100
samples <- cbind(
  C3 = sin(2 * pi * 10 * t),
  C4 = sin(2 * pi * 10 * t + 0.5)
)
loopbackFeed(source, samples, t)
```

## Snapshot to a PhysioExperiment

[`streamSnapshot()`](https://x-biosignal.github.io/PhysioStream/reference/streamSnapshot.md)
copies buffered samples into a `PhysioExperiment`, preserving the
original timestamps and the monotonic stream sequence. From here the
rest of the ecosystem applies.

``` r

pe <- streamSnapshot(source)
dim(pe)
#> [1] 50  2
SummarizedExperiment::assayNames(pe)
#> [1] "stream"
```

## A synchronous processing pipeline

A `StreamPipeline` owns a bounded chunk queue and an ordered, causal
operation graph. Here we register the compiled second-order-section +
rolling-RMS operation. A pass-through SOS keeps the example
self-contained; use
[`PhysioPreprocess::sosDesign()`](https://x-biosignal.r-universe.dev/PhysioPreprocess/reference/sosDesign.html)
for a real band definition.

``` r

sos <- matrix(
  c(1, 0, 0, 1, 0, 0), 1L, 6L,
  dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2"))
)
pipeline <- streamPipeline(chunk_size = 64L)
onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))

pipelineEnqueue(pipeline, samples, t, ingest_time_ns = 0)
result <- pipelineStep(pipeline)
head(result$results[[1]]$output$samples)
#>         C3_rms    C4_rms
#> [1,] 0.5877853 0.9036935
#> [2,] 0.7905694 0.9440659
#> [3,] 0.8474488 0.8667517
#> [4,] 0.7905694 0.7533510
#> [5,] 0.7339122 0.6487176
#> [6,] 0.6315638 0.6193022
```

## Online DSP

The DSP processors are stateful and chunk-invariant. An adaptive filter,
for example, estimates the part of a channel that is linearly
predictable from a paired reference.

``` r

set.seed(1)
reference <- sin(2 * pi * 10 * t)
observed <- 0.4 * reference + rnorm(n, sd = 0.05)
filt <- lmsFilter(n_taps = 4L, step_size = 0.1)
residual <- update(
  filt,
  matrix(observed, ncol = 1),
  reference = matrix(reference, ncol = 1)
)
length(residual$output)
#> [1] 50
```

## Clock correction

Explicit clock models correct offset and drift and regularize jitter.
They change timestamps only – samples are never interpolated or
reordered.

``` r

device <- seq(0, 10, by = 0.5)
offset <- -0.4 + 0.005 * (device - median(device))
model <- clockOffset(device, offset, method = "ols")
model$segments$drift_ppm
#> [1] 5000

jittered <- c(10.001, 10.010, 10.021, 10.029)
as.numeric(dejitter(jittered, nominal_srate = 100))
#> [1] 10.00025 10.01025 10.02025 10.03025
```

## A governed stimulation trigger

The loopback trigger records commands and timing with no device, while
exercising the same arm, dead-man, maximum, duplicate-ID, and refractory
interlocks as the MQTT and TTL transports. Software interlocks are
safeguards only; independently fail-safe stimulation hardware is still
required.

``` r

trigger <- loopbackTrigger(
  allowed_channels = "left",
  max_intensity = 20,
  intensity_unit = "mA",
  max_duration_ms = 500,
  refractory_ms = 0,
  deadman_ms = 1000
)
triggerOpen(trigger)
armTrigger(trigger, "demo-session", now_ns = 0)
receipt <- sendStim(
  trigger, intensity = 2.5, channel = "left",
  duration_ms = 125, command_id = "cmd-1", now_ns = 10
)
receipt$status
#> [1] "acknowledged"
triggerClose(trigger)
```

## Where to go next

- **Live acquisition.**
  [`lslResolveStreams()`](https://x-biosignal.github.io/PhysioStream/reference/lslResolveStreams.md),
  [`lslInlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslInlet.md),
  and
  [`lslOutlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslOutlet.md)
  bind Lab Streaming Layer streams;
  [`lslAvailable()`](https://x-biosignal.github.io/PhysioStream/reference/lslAvailable.md)
  reports whether the backend is installed.
- **File interchange.**
  [`readXDF()`](https://x-biosignal.github.io/PhysioStream/reference/readXDF.md)
  /
  [`writeXDF()`](https://x-biosignal.github.io/PhysioStream/reference/writeXDF.md)
  require the pyxdf backend
  ([`xdfAvailable()`](https://x-biosignal.github.io/PhysioStream/reference/xdfAvailable.md)).
- **Closed-loop control.**
  [`closedLoop()`](https://x-biosignal.github.io/PhysioStream/reference/closedLoop.md)
  binds a detector
  ([`emgOnsetOp()`](https://x-biosignal.github.io/PhysioStream/reference/emgOnsetOp.md),
  [`erdIntentOp()`](https://x-biosignal.github.io/PhysioStream/reference/erdIntentOp.md),
  [`phaseTargetOp()`](https://x-biosignal.github.io/PhysioStream/reference/phaseTargetOp.md))
  to a trigger with commit-before-stimulation semantics.
- **Live visualization.**
  [`biofeedbackScope()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedbackScope.md)
  provides a bounded display-only scope (Shiny is required only to
  launch it).
