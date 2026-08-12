# Package index

## All functions

- [`LSLInlet-class`](https://x-biosignal.github.io/PhysioStream/reference/LSLInlet-class.md)
  : Lab Streaming Layer inlet
- [`LSLOutlet-class`](https://x-biosignal.github.io/PhysioStream/reference/LSLOutlet-class.md)
  : Lab Streaming Layer outlet
- [`LoopbackSource-class`](https://x-biosignal.github.io/PhysioStream/reference/LoopbackSource-class.md)
  : Deterministic in-process stream source
- [`RingBuffer-class`](https://x-biosignal.github.io/PhysioStream/reference/RingBuffer-class.md)
  : Live bounded ring buffer
- [`StreamEndpoint-class`](https://x-biosignal.github.io/PhysioStream/reference/StreamEndpoint-class.md)
  : Virtual stream endpoint
- [`show(`*`<StreamInfo>`*`)`](https://x-biosignal.github.io/PhysioStream/reference/StreamInfo-class.md)
  : Validated stream metadata
- [`StreamSink-class`](https://x-biosignal.github.io/PhysioStream/reference/StreamSink-class.md)
  : Virtual stream sink
- [`StreamSource-class`](https://x-biosignal.github.io/PhysioStream/reference/StreamSource-class.md)
  : Virtual stream source
- [`armTrigger()`](https://x-biosignal.github.io/PhysioStream/reference/armTrigger.md)
  [`triggerHeartbeat()`](https://x-biosignal.github.io/PhysioStream/reference/armTrigger.md)
  [`disarmTrigger()`](https://x-biosignal.github.io/PhysioStream/reference/armTrigger.md)
  [`emergencyStop()`](https://x-biosignal.github.io/PhysioStream/reference/armTrigger.md)
  : Arm, heartbeat, disarm, or emergency-stop a trigger
- [`bandpassRmsOp()`](https://x-biosignal.github.io/PhysioStream/reference/bandpassRmsOp.md)
  : Construct the compiled causal SOS plus rolling-RMS operation
- [`biofeedbackStart()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-lifecycle.md)
  [`biofeedbackStep()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-lifecycle.md)
  [`biofeedbackStop()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-lifecycle.md)
  : Start, step, and stop a biofeedback runtime
- [`biofeedbackFrame()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-state.md)
  [`biofeedbackState()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedback-state.md)
  : Inspect a biofeedback frame or portable state
- [`biofeedbackScope()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedbackScope.md)
  : Construct and optionally launch a governed live biofeedback scope
- [`biofeedbackUpdate()`](https://x-biosignal.github.io/PhysioStream/reference/biofeedbackUpdate.md)
  : Publish a bounded external feedback update
- [`clockOffset()`](https://x-biosignal.github.io/PhysioStream/reference/clockOffset.md)
  : Estimate a governed stream-clock mapping
- [`closedLoopStart()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
  [`closedLoopStep()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
  [`closedLoopFlush()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
  [`closedLoopStop()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
  [`closedLoopEmergencyStop()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-lifecycle.md)
  : Run a governed closed-loop session
- [`closedLoopState()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-state.md)
  [`closedLoopLog()`](https://x-biosignal.github.io/PhysioStream/reference/closed-loop-state.md)
  : Inspect governed closed-loop state and audit
- [`closedLoop()`](https://x-biosignal.github.io/PhysioStream/reference/closedLoop.md)
  : Construct a governed detector-to-stimulation controller
- [`closedLoopProvenance()`](https://x-biosignal.github.io/PhysioStream/reference/closedLoopProvenance.md)
  : Append a stopped closed-loop session to PhysioExperiment provenance
- [`dejitter()`](https://x-biosignal.github.io/PhysioStream/reference/dejitter.md)
  : Regularize stream timestamps on a nominal grid
- [`driftCorrect()`](https://x-biosignal.github.io/PhysioStream/reference/driftCorrect.md)
  : Apply a governed stream-clock model
- [`emgOnsetOp()`](https://x-biosignal.github.io/PhysioStream/reference/emgOnsetOp.md)
  : Construct a causal streaming EMG-onset detector
- [`erdIntentOp()`](https://x-biosignal.github.io/PhysioStream/reference/erdIntentOp.md)
  : Construct a causal streaming EEG ERD detector
- [`incrementalPCA()`](https://x-biosignal.github.io/PhysioStream/reference/incrementalPCA.md)
  : Incremental principal-component analysis
- [`lmsFilter()`](https://x-biosignal.github.io/PhysioStream/reference/lmsFilter.md)
  : Stateful least-mean-squares adaptive filter
- [`loopbackFeed()`](https://x-biosignal.github.io/PhysioStream/reference/loopbackFeed.md)
  : Feed a loopback source
- [`loopbackSource()`](https://x-biosignal.github.io/PhysioStream/reference/loopbackSource.md)
  : Construct a loopback source
- [`loopbackTrigger()`](https://x-biosignal.github.io/PhysioStream/reference/loopbackTrigger.md)
  : Construct a deterministic governed loopback trigger
- [`lslAvailable()`](https://x-biosignal.github.io/PhysioStream/reference/lslAvailable.md)
  : Test Lab Streaming Layer backend availability
- [`lslBackendInfo()`](https://x-biosignal.github.io/PhysioStream/reference/lslBackendInfo.md)
  : Report the configured LSL backend
- [`lslInlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslInlet.md)
  : Construct an LSL inlet
- [`lslMarkerEvents()`](https://x-biosignal.github.io/PhysioStream/reference/lslMarkerEvents.md)
  : Convert buffered LSL markers to PhysioEvents
- [`lslOutlet()`](https://x-biosignal.github.io/PhysioStream/reference/lslOutlet.md)
  : Construct an LSL outlet
- [`lslPull()`](https://x-biosignal.github.io/PhysioStream/reference/lslPull.md)
  : Pull one LSL chunk into an inlet buffer
- [`lslPush()`](https://x-biosignal.github.io/PhysioStream/reference/lslPush.md)
  : Push samples or markers through an LSL outlet
- [`lslResolveStreams()`](https://x-biosignal.github.io/PhysioStream/reference/lslResolveStreams.md)
  : Resolve visible Lab Streaming Layer streams
- [`measureLatency()`](https://x-biosignal.github.io/PhysioStream/reference/measureLatency.md)
  : Measure governed ingest-to-emit pipeline latency
- [`mqttAvailable()`](https://x-biosignal.github.io/PhysioStream/reference/mqttAvailable.md)
  [`mqttBackendInfo()`](https://x-biosignal.github.io/PhysioStream/reference/mqttAvailable.md)
  : Test or describe the configured MQTT backend
- [`mqttTrigger()`](https://x-biosignal.github.io/PhysioStream/reference/mqttTrigger.md)
  : Construct a governed MQTT stimulation-command trigger
- [`nlmsFilter()`](https://x-biosignal.github.io/PhysioStream/reference/nlmsFilter.md)
  : Stateful normalized LMS adaptive filter
- [`onChunk()`](https://x-biosignal.github.io/PhysioStream/reference/onChunk.md)
  : Register a causal per-chunk operation
- [`onlineICA()`](https://x-biosignal.github.io/PhysioStream/reference/onlineICA.md)
  : Online recursive independent-component analysis
- [`phaseTargetOp()`](https://x-biosignal.github.io/PhysioStream/reference/phaseTargetOp.md)
  : Construct a causal phase-target detector
- [`pipelineState()`](https://x-biosignal.github.io/PhysioStream/reference/pipeline-state.md)
  [`pipelineReset()`](https://x-biosignal.github.io/PhysioStream/reference/pipeline-state.md)
  : Inspect or reset governed pipeline state
- [`pipelineEnqueue()`](https://x-biosignal.github.io/PhysioStream/reference/pipelineEnqueue.md)
  : Enqueue one bounded input chunk
- [`pipelineRun()`](https://x-biosignal.github.io/PhysioStream/reference/pipelineRun.md)
  : Pull from a configured source and process synchronously
- [`pipelineStep()`](https://x-biosignal.github.io/PhysioStream/reference/pipelineStep.md)
  : Process queued chunks synchronously
- [`processorState()`](https://x-biosignal.github.io/PhysioStream/reference/processorState.md)
  [`processorReset()`](https://x-biosignal.github.io/PhysioStream/reference/processorState.md)
  : Inspect or reset governed streaming processor state
- [`readXDF()`](https://x-biosignal.github.io/PhysioStream/reference/readXDF.md)
  : Read an Extensible Data Format file
- [`ringPull()`](https://x-biosignal.github.io/PhysioStream/reference/ring-read.md)
  [`ringPeek()`](https://x-biosignal.github.io/PhysioStream/reference/ring-read.md)
  : Pull or inspect buffered samples
- [`ringCapacity()`](https://x-biosignal.github.io/PhysioStream/reference/ring-state.md)
  [`ringFill()`](https://x-biosignal.github.io/PhysioStream/reference/ring-state.md)
  [`ringStats()`](https://x-biosignal.github.io/PhysioStream/reference/ring-state.md)
  : Ring-buffer capacity and occupancy
- [`ringBuffer()`](https://x-biosignal.github.io/PhysioStream/reference/ringBuffer.md)
  : Construct a native ring buffer
- [`ringPush()`](https://x-biosignal.github.io/PhysioStream/reference/ringPush.md)
  : Push samples into a ring buffer
- [`ringReset()`](https://x-biosignal.github.io/PhysioStream/reference/ringReset.md)
  : Reset unread ring-buffer state
- [`rlsFilter()`](https://x-biosignal.github.io/PhysioStream/reference/rlsFilter.md)
  : Stateful recursive-least-squares adaptive filter
- [`sendStim()`](https://x-biosignal.github.io/PhysioStream/reference/sendStim.md)
  : Send a governed stimulation command
- [`slidingSTFT()`](https://x-biosignal.github.io/PhysioStream/reference/slidingSTFT.md)
  : Sliding short-time Fourier transform
- [`streamName()`](https://x-biosignal.github.io/PhysioStream/reference/stream-metadata.md)
  [`streamType()`](https://x-biosignal.github.io/PhysioStream/reference/stream-metadata.md)
  [`streamChannels()`](https://x-biosignal.github.io/PhysioStream/reference/stream-metadata.md)
  [`streamRate()`](https://x-biosignal.github.io/PhysioStream/reference/stream-metadata.md)
  [`streamDtype()`](https://x-biosignal.github.io/PhysioStream/reference/stream-metadata.md)
  : Stream metadata accessors
- [`streamOpen()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
  [`streamClose()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
  [`streamPull()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
  [`streamPush()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
  [`streamState()`](https://x-biosignal.github.io/PhysioStream/reference/stream-operations.md)
  : Stream endpoint lifecycle and transport operations
- [`streamInfo()`](https://x-biosignal.github.io/PhysioStream/reference/streamInfo.md)
  : Construct or retrieve stream metadata
- [`streamPipeline()`](https://x-biosignal.github.io/PhysioStream/reference/streamPipeline.md)
  : Construct a governed synchronous stream-processing pipeline
- [`streamSnapshot()`](https://x-biosignal.github.io/PhysioStream/reference/streamSnapshot.md)
  : Snapshot an open numeric stream
- [`syncDiagnostics()`](https://x-biosignal.github.io/PhysioStream/reference/syncDiagnostics.md)
  : Summarize governed stream synchronization
- [`syncStreams()`](https://x-biosignal.github.io/PhysioStream/reference/syncStreams.md)
  : Synchronize a multi-rate stream container
- [`triggerOpen()`](https://x-biosignal.github.io/PhysioStream/reference/triggerOpen.md)
  [`triggerClose()`](https://x-biosignal.github.io/PhysioStream/reference/triggerOpen.md)
  : Open or close a governed stimulation trigger
- [`triggerState()`](https://x-biosignal.github.io/PhysioStream/reference/triggerState.md)
  : Inspect governed trigger state
- [`ttlTrigger()`](https://x-biosignal.github.io/PhysioStream/reference/ttlTrigger.md)
  : Construct a governed TTL stimulation-command trigger
- [`videoSyncTime()`](https://x-biosignal.github.io/PhysioStream/reference/video-sync-map.md)
  [`signalSyncTime()`](https://x-biosignal.github.io/PhysioStream/reference/video-sync-map.md)
  : Map between synchronized signal and video time
- [`videoSync()`](https://x-biosignal.github.io/PhysioStream/reference/videoSync.md)
  : Fit a governed marker-anchored video/signal time map
- [`videoSyncViewer()`](https://x-biosignal.github.io/PhysioStream/reference/videoSyncViewer.md)
  : Launch a synchronized video and signal viewer
- [`welchOnline()`](https://x-biosignal.github.io/PhysioStream/reference/welchOnline.md)
  : Exponentially weighted online Welch spectrum
- [`writeXDF()`](https://x-biosignal.github.io/PhysioStream/reference/writeXDF.md)
  : Write an Extensible Data Format file
- [`xdfAvailable()`](https://x-biosignal.github.io/PhysioStream/reference/xdfAvailable.md)
  : Test XDF backend availability
- [`xdfBackendInfo()`](https://x-biosignal.github.io/PhysioStream/reference/xdfBackendInfo.md)
  : Report the configured XDF backend
