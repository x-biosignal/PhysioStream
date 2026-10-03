.pipeline_normalize_sos <- function(sos) {
  if (!is.matrix(sos) || !is.numeric(sos) || is.object(sos) ||
      nrow(sos) < 1L || ncol(sos) != 6L || any(!is.finite(sos)) ||
      any(sos[, 4L] == 0)) {
    .pipeline_abort(
      "`sos` must be a finite numeric matrix with six columns and nonzero a0",
      "PhysioStream_pipeline_validation_error"
    )
  }
  sos <- matrix(
    as.numeric(sos), nrow = nrow(sos), ncol = 6L,
    dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2"))
  )
  for (i in seq_len(nrow(sos))) {
    a0 <- sos[i, 4L]
    sos[i, ] <- sos[i, ] / a0
    sos[i, 4L] <- 1
  }
  sos
}

.pipeline_hotpath_bytes <- function(n_sections, n_channels, window_samples,
                                    n_samples) {
  elements <- c(
    as.double(n_sections) * 2 * n_channels,
    as.double(window_samples) * n_channels,
    as.double(n_channels),
    as.double(n_samples) * n_channels
  )
  if (any(!is.finite(elements)) ||
      sum(elements) * 8 > .dsp_allocation_limit) {
    .pipeline_abort(
      "bandpass plus RMS state/output exceeds the 512 MiB allocation ceiling",
      "PhysioStream_pipeline_resource_error"
    )
  }
  invisible(TRUE)
}

.pipeline_bandpass_rms_callback <- function(chunk, state, context) {
  n_channels <- ncol(chunk$samples)
  n_sections <- nrow(state$sos)
  .pipeline_hotpath_bytes(
    n_sections, n_channels, state$window_samples, nrow(chunk$samples)
  )
  # A matrix with no column names is the most natural thing to hand a pipeline,
  # and it used to crash here: channel_names stayed NULL, so naming the output
  # below computed paste0(NULL, "_rms") -- length one -- and assigning that to a
  # multi-column matrix failed with "'dimnames' length not equal to array
  # extent". Worse, because NULL was also the "not yet initialised" signal, an
  # unnamed stream re-initialised its filter state on every chunk, silently
  # discarding the carried-over zi and RMS window. Names are synthesised once,
  # following the Ch1..Chn convention the containers use, and initialisation is
  # tracked explicitly.
  incoming <- colnames(chunk$samples)
  if (is.null(incoming)) incoming <- paste0("Ch", seq_len(n_channels))
  if (!isTRUE(state$initialised)) {
    state$channel_names <- incoming
    state$initialised <- TRUE
    state$zi <- array(
      0, dim = c(n_sections, 2L, n_channels),
      dimnames = list(NULL, c("z1", "z2"), state$channel_names)
    )
    state$rms_buffer <- matrix(
      0, nrow = state$window_samples, ncol = n_channels,
      dimnames = list(NULL, state$channel_names)
    )
    state$rms_sums <- stats::setNames(numeric(n_channels),
                                      state$channel_names)
  } else if (!identical(incoming, state$channel_names)) {
    .pipeline_abort(
      "bandpass RMS operation input channel identity changed",
      "PhysioStream_pipeline_channel_error"
    )
  }
  result <- cpp_pipeline_sos_rms(
    chunk$samples,
    state$sos,
    state$zi,
    state$rms_buffer,
    state$rms_sums,
    state$rms_cursor,
    state$rms_filled
  )
  state$zi <- result$zi
  state$rms_buffer <- result$rms_buffer
  state$rms_sums <- result$rms_sums
  state$rms_cursor <- result$rms_cursor
  state$rms_filled <- result$rms_filled
  state$n_samples <- state$n_samples + nrow(chunk$samples)
  keep <- if (identical(state$warmup, "complete")) {
    result$available
  } else {
    rep(TRUE, nrow(chunk$samples))
  }
  output <- chunk
  output$samples <- result$rms[keep, , drop = FALSE]
  colnames(output$samples) <- paste0(state$channel_names, "_rms")
  output$sequence <- chunk$sequence[keep]
  if (!is.null(chunk$timestamps)) {
    output$timestamps <- chunk$timestamps[keep]
  }
  list(
    output = output,
    state = state,
    events = list(),
    diagnostics = list(
      available = as.logical(result$available),
      warmup = state$warmup,
      window_samples = state$window_samples
    )
  )
}

#' Construct the compiled causal SOS plus rolling-RMS operation
#'
#' The operation uses direct-form-II-transposed second-order sections followed
#' by a per-channel rolling root-mean-square. It is a descriptor for
#' [onChunk()] and performs no processing until registered and stepped.
#'
#' @param sos Finite second-order-section matrix with columns
#'   `b0,b1,b2,a0,a1,a2`.
#' @param window_samples Exact positive RMS window length.
#' @param warmup Whether to emit partial-window RMS values or only rows after a
#'   complete window exists.
#' @param name Default operation name.
#' @return A `PipelineOperation` descriptor.
#' @examples
#' # A pass-through SOS keeps the example self-contained; use
#' # PhysioPreprocess::sosDesign() for a real band definition.
#' sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
#'               dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
#' op <- bandpassRmsOp(sos, window_samples = 4L)
#' pipeline <- streamPipeline(chunk_size = 8L)
#' onChunk(pipeline, op)
#' x <- matrix(sin(seq_len(16)), 8, 2, dimnames = list(NULL, c("C3", "C4")))
#' pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
#' pipelineStep(pipeline)$results[[1]]$output$samples
#' @export
bandpassRmsOp <- function(
    sos,
    window_samples,
    warmup = c("partial", "complete"),
    name = "bandpass_rms") {
  sos <- .pipeline_normalize_sos(sos)
  window_samples <- .dsp_scalar(
    window_samples, "window_samples", lower = 1, upper = 1048576,
    integer = TRUE
  )
  if (length(warmup) > 1L) {
    warmup <- warmup[[1L]]
  }
  warmup <- .dsp_enum(warmup, c("partial", "complete"), "warmup")
  name <- .pipeline_name(name, "name")
  .pipeline_hotpath_bytes(nrow(sos), 1L, window_samples, 0L)
  state <- list(
    sos = sos,
    window_samples = window_samples,
    warmup = warmup,
    initialised = FALSE,
    channel_names = NULL,
    zi = NULL,
    rms_buffer = NULL,
    rms_sums = NULL,
    rms_cursor = 0L,
    rms_filled = 0L,
    n_samples = 0
  )
  structure(
    list(
      callback = .pipeline_bandpass_rms_callback,
      state = state,
      name = name,
      kind = "feature",
      descriptor = list(
        type = "bandpass_rms",
        configuration = list(
          sos = sos,
          window_samples = window_samples,
          warmup = warmup
        )
      )
    ),
    class = "PipelineOperation"
  )
}
