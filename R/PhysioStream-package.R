#' PhysioStream: governed physiological data streams
#'
#' PhysioStream supplies transport-neutral stream contracts, a bounded native
#' SPSC buffer, deterministic loopback streams, capability-gated Lab Streaming
#' Layer inlets/outlets, marker-event capture, governed XDF interchange, and
#' conversion to [PhysioExperiment::PhysioExperiment] snapshots or multi-rate
#' containers. Explicit clock models provide segment-aware drift correction,
#' timestamp de-jittering, and multi-stream synchronization without sample
#' interpolation. Live buffers and transport endpoints are reference objects:
#' copying an R wrapper aliases the same runtime storage, and serialized
#' wrappers cannot be resumed. Bounded biofeedback frames and marker-anchored
#' video-time mappings provide a display-only live visualization boundary.
#'
#' @keywords internal
"_PACKAGE"

#' @import methods
#' @importFrom PhysioExperiment PhysioExperiment appendProvenance
#' @importFrom PhysioPreprocess StreamFilter
#' @importFrom Rcpp evalCpp
#' @importFrom SummarizedExperiment assay
#' @importFrom utils head tail
#' @useDynLib PhysioStream, .registration = TRUE
NULL
