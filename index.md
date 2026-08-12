# PhysioStream

PhysioStream provides loss-aware, deterministic foundations for
physiological data streams. The package defines transport-neutral source
and sink contracts, a bounded native SPSC buffer, in-process loopback
streams, and governed snapshots into `PhysioExperiment`. Optional Lab
Streaming Layer (LSL) endpoints discover and transport regular numeric
signals and irregular string markers without making Python or liblsl a
mandatory package dependency. Governed XDF import preserves raw and
corrected timestamps, clock observations, stream XML, and string markers
in a multi-rate container. The native XDF writer does not require
Python.

Recorded clock evidence can be converted into an explicit, serializable
model and applied without interpolating samples:

``` r

model <- clockOffset(clock_times, clock_offsets)
master_time <- driftCorrect(device_time, model)
regular_time <- dejitter(master_time, nominal_srate = 1000)
synced <- syncStreams(xdf_container)
syncDiagnostics(synced)
```

The sign convention is always
`master_time = device_time + offset(device_time)`. Reset segments are
fitted independently, and synchronized timestamps are appended beside
the original evidence rather than replacing it.

Stateful DSP processors accept sample-by-channel chunks and expose
portable, hashed numeric state:

``` r

adaptive <- rlsFilter(n_taps = 16, forgetting = 0.995)
filtered <- update(adaptive, signal_chunk, reference = noise_chunk)
processorState(adaptive)

spectrum <- welchOnline(1000, window_samples = 512, hop_samples = 256)
frame <- update(spectrum, signal_chunk)
```

Incremental PCA, online ICA, sliding STFT, and optional independent
[`PhysioPreprocess::StreamFilter`](https://x-biosignal.r-universe.dev/PhysioPreprocess/reference/StreamFilter.html)
instances use the same update/reset contract. Adaptive residuals and ICA
components are mathematical estimates, not claims of clean physiological
signals or identified biological sources.

Bounded pipelines schedule causal operations synchronously and expose
explicit loss and latency evidence:

``` r

pipeline <- streamPipeline(
  chunk_size = 32,
  queue_capacity = 8,
  backpressure = "error"
)
operation <- bandpassRmsOp(sos, window_samples = 128)
onChunk(pipeline, operation)
pipelineEnqueue(pipeline, signal_chunk, timestamps)
processed <- pipelineStep(pipeline)
pipelineState(pipeline)
```

[`measureLatency()`](https://x-biosignal.github.io/PhysioStream/reference/measureLatency.md)
reports monotonic ingest-to-emit p50/p95/p99 values and the recorded
hardware. The 50 ms target is an empirical reference budget, not a hard
real-time operating-system guarantee.

Stimulation-command outlets start closed and disarmed, and require
explicit arm and heartbeat state:

``` r

trigger <- loopbackTrigger(
  allowed_channels = c("left", "right"),
  max_intensity = c(left = 20, right = 20),
  intensity_unit = "mA",
  max_duration_ms = 500,
  refractory_ms = 1000,
  deadman_ms = 2000
)
triggerOpen(trigger)
armTrigger(trigger, session_id = "demo-session")
receipt <- sendStim(
  trigger, intensity = 5, channel = "left",
  duration_ms = 100, command_id = "demo-command-1"
)
triggerClose(trigger)
```

MQTT uses the configured reticulate environment containing pinned Paho
and publishes MQTT v5 commands with `retain = FALSE`. QoS 1 may
duplicate a command, so receivers must deduplicate exact command IDs.
TTL serial/callback adapters are generic integration boundaries, not
vendor stimulator protocols. Software maximum, dead-man, and refractory
gates are defense in depth only: independently fail-safe stimulation
hardware, limits, isolation, watchdog, and emergency stop remain
mandatory. A broker, serial, or adapter acknowledgement does not prove
physical stimulation.

LSL access uses an existing `reticulate` Python environment containing
`pylsl`. PhysioStream never installs or selects that environment:

``` r

lslAvailable(initialize = TRUE)
lslBackendInfo()
```

[`lslResolveStreams()`](https://x-biosignal.github.io/PhysioStream/reference/lslResolveStreams.md)
returns validated `StreamInfo` objects.
[`lslOutlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslOutlet.md)
and
[`lslInlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslInlet.md)
create side-effect-free endpoints; assign the result of
[`streamOpen()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
before calling
[`lslPush()`](https://x-biosignal.github.io/PhysioStream/reference/lslPush.md)
or
[`lslPull()`](https://x-biosignal.github.io/PhysioStream/reference/lslPull.md).
Numeric inlet chunks enter the bounded native ring and can be converted
with
[`streamSnapshot()`](https://x-biosignal.github.io/PhysioStream/reference/streamSnapshot.md).
Irregular string markers are retrieved as `PhysioEvents` with
[`lslMarkerEvents()`](https://x-biosignal.github.io/PhysioStream/reference/lslMarkerEvents.md).

XDF reading uses the same existing environment policy with `pyxdf`:

``` r

xdfAvailable(initialize = TRUE)
xdfBackendInfo()
x <- readXDF("recording.xdf")
writeXDF(x, "roundtrip.xdf")
```

`rowData(stream)$xdf_time` remains the authoritative timestamp vector
for jittered or irregular XDF streams; no import operation resamples
samples.

PhysioStream is research software infrastructure. It does not provide
real-time operating-system guarantees, network delivery guarantees,
clock-accuracy guarantees, medical-device certification, or
independently sufficient stimulation safety.

## Installation

``` r

install.packages(
  "PhysioStream",
  repos = c(
    "https://x-biosignal.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```
