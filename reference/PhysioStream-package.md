# PhysioStream: governed physiological data streams

PhysioStream supplies transport-neutral stream contracts, a bounded
native SPSC buffer, deterministic loopback streams, capability-gated Lab
Streaming Layer inlets/outlets, marker-event capture, governed XDF
interchange, and conversion to
[PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html)
snapshots or multi-rate containers. Explicit clock models provide
segment-aware drift correction, timestamp de-jittering, and multi-stream
synchronization without sample interpolation. Live buffers and transport
endpoints are reference objects: copying an R wrapper aliases the same
runtime storage, and serialized wrappers cannot be resumed. Bounded
biofeedback frames and marker-anchored video-time mappings provide a
display-only live visualization boundary.

## See also

Useful links:

- <https://github.com/x-biosignal/PhysioStream>

- <https://x-biosignal.r-universe.dev/PhysioStream>

- <https://x-biosignal.github.io/PhysioStream/>

- Report bugs at <https://github.com/x-biosignal/PhysioStream/issues>

## Author

**Maintainer**: Yusuke Matsui <mail.to.matsui@gmail.com>

Authors:

- Yusuke Matsui <mail.to.matsui@gmail.com>
