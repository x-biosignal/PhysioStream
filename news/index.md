# Changelog

## PhysioStream 0.9.3

- XDF writing and stream synchronisation accept the canonical
  `MultiPhysioExperiment`. They previously tested for
  `MultiRatePhysioExperiment` only, which would have rejected a
  container built by the current constructor.

## PhysioStream 0.9.2

- Made two tests portable to the macOS/Windows r-universe binary
  builders (which the 0.9.1 pyxdf-skip fix exposed once those tests
  stopped being masked): (1) the bundled-JS syntax check now asserts
  `node --check` exits 0 rather than requiring silent output, since
  macOS/Windows node emits deprecation/experimental warnings orthogonal
  to syntax validity (and skips when node is absent); (2) the DSP
  reference-fixture covariance self- consistency check uses a numeric
  tolerance (1e-8) instead of tolerance = 0, because stats::cov() is
  BLAS/LAPACK-backed and drifts ~1 ULP across platforms.

## PhysioStream 0.9.1

- Fixed XDF/LSL capability probes triggering reticulate’s automatic
  ephemeral Python provisioning. reticulate (\>= 1.41) downloads uv,
  CPython and NumPy on first initialization when no interpreter is
  configured; on an offline build machine (e.g. r-universe binaries)
  that network work fails/hangs, so the pyxdf/pylsl skip guards surfaced
  as test ERRORs instead of clean SKIPs. The probes now gate on an
  already-configured, on-disk interpreter (`py_discover_config()` /
  `py_available(initialize = FALSE)`, neither of which provisions) and
  never start a download. Tests skip cleanly when no Python backend is
  present.

## PhysioStream 0.7.0

- Add governed loopback, MQTT v5, serial TTL, and callback TTL
  stimulation-command outlets with explicit side-effect-free
  construction and transport lifecycle.
- Add arm, heartbeat/dead-man, channel maximum, duration,
  duplicate-command, and per-channel refractory software gates with
  sealed bounded audit state.
- Record conservative unknown delivery after transport invocation, never
  retry stimulation commands automatically, and distinguish transport
  acknowledgement from physical stimulation.

## PhysioStream 0.6.0

- Add bounded synchronous stream pipelines with transactional per-chunk
  callback state and explicit error/drop-oldest/drop-newest
  backpressure.
- Add process-local monotonic ingest-to-emit latency records and
  p50/p95/p99 benchmark summaries.
- Add a compiled causal SOS-bandpass plus rolling-RMS operation for the
  documented 32-channel latency reference chain.

## PhysioStream 0.5.0

- Add governed LMS, normalized LMS, and recursive-least-squares adaptive
  filters with inspectable coefficient and delay state.
- Add constant-memory incremental PCA and adaptive-whitening online ICA.
- Add exponentially weighted online Welch spectra and sliding
  STFT/band-power emission with exact chunk-boundary continuity.
- Add portable hashed processor-state snapshots, transactional updates,
  and independent per-channel causal-filter integration.

## PhysioStream 0.4.0

- Add serializable robust clock-offset models with reset-segment drift
  correction and bounded extrapolation.
- Add explicit timestamp de-jittering that preserves sample values,
  counts, order, and reset boundaries.
- Add governed multi-stream synchronization and residual/jitter/event
  diagnostics for `MultiRatePhysioExperiment` containers.

## PhysioStream 0.3.0

- Add capability-gated XDF import through pinned pyxdf while preserving
  native file, stream-header, footer, clock, and timestamp evidence.
- Add an atomic native XDF 1.0 writer for governed numeric and string
  streams.
- Map every XDF stream to a `PhysioExperiment` inside a common-clock
  `MultiRatePhysioExperiment`, including lossless marker payloads and
  events.

## PhysioStream 0.2.0

- Add capability-gated pylsl discovery with governed LSL descriptor,
  channel, rate, dtype, source, and clock-domain mapping.
- Add reference-like `LSLInlet` and `LSLOutlet` endpoints with strict
  lifecycle, payload, timestamp, and local overwrite accounting.
- Add bounded irregular string-marker capture and deterministic
  conversion to `PhysioEvents`.
- Add offline metadata fixtures, independent validation, and optional
  real localhost loopback coverage for all governed LSL dtypes.

## PhysioStream 0.1.0

- Add validated `StreamInfo`, virtual source/sink contracts, and
  deterministic loopback streams.
- Add a bounded native SPSC ring buffer with explicit overwrite
  accounting.
- Add governed `PhysioExperiment` snapshots that preserve acquired
  timestamps and sequence identity.
