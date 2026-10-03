.stream_source_buffer <- function(x) {
  if (methods::is(x, "LoopbackSource")) {
    return(x@buffer)
  }
  if (methods::is(x, "LSLInlet") &&
      !identical(streamInfo(x)@dtype, "string")) {
    return(x@buffer)
  }
  {
    .stream_abort(
      sprintf("streamSnapshot has no buffer adapter for `%s`", class(x)[[1L]]),
      "PhysioStream_unsupported_operation"
    )
  }
}

.stream_snapshot_integer <- function(x, name) {
  .ring_exact_integer(x, name, 0)
}

.stream_source_fingerprint <- function(info) {
  digest::digest(
    serialize(info, NULL, version = 3L),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Snapshot an open numeric stream
#'
#' Original timestamps and monotonic stream sequence identity are retained.
#' Selection is non-consuming unless `consume = TRUE`; consuming a newest
#' subset while older unread rows remain is rejected.
#'
#' @param x An open buffered `StreamSource`.
#' @param n Optional number of newest rows.
#' @param duration_seconds Optional duration of the newest closed timestamp
#'   interval.
#' @param consume Whether to consume the selected oldest contiguous unread rows.
#' @param assay_name Non-empty assay name.
#' @return A valid [PhysioExperiment::PhysioExperiment].
#' @examples
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 100)
#' src <- streamOpen(loopbackSource(info, capacity = 32L))
#' loopbackFeed(src, matrix(sin(seq_len(10)), 5, 2),
#'              c(0.01, 0.02, 0.03, 0.04, 0.05))
#' pe <- streamSnapshot(src)
#' dim(pe)
#' @export
streamSnapshot <- function(x, n = NULL, duration_seconds = NULL,
                           consume = FALSE, assay_name = "stream") {
  if (!methods::is(x, "StreamSource")) {
    .stream_abort("`x` must be a StreamSource",
                  "PhysioStream_validation_error")
  }
  .stream_require_open(x, "streamSnapshot")
  if (!xor(is.null(n), is.null(duration_seconds)) &&
      !(is.null(n) && is.null(duration_seconds))) {
    .stream_abort("supply at most one of `n` and `duration_seconds`",
                  "PhysioStream_validation_error")
  }
  if (!is.logical(consume) || length(consume) != 1L || is.na(consume)) {
    .stream_abort("`consume` must be one non-missing logical",
                  "PhysioStream_validation_error")
  }
  .stream_scalar_string(assay_name, "assay_name")
  info <- streamInfo(x)
  if (identical(info@dtype, "string") || info@nominal_srate <= 0) {
    .stream_abort("snapshots require a numeric stream with positive nominal rate",
                  "PhysioStream_validation_error")
  }
  buffer <- .stream_source_buffer(x)
  stats_before <- ringStats(buffer)
  fill <- as.integer(stats_before$fill)

  selection <- "all"
  selected <- if (!is.null(n)) {
    n <- .stream_snapshot_integer(n, "n")
    selection <- "newest_n"
    ringPeek(buffer, min(n, fill), "latest")
  } else if (!is.null(duration_seconds)) {
    if (!is.numeric(duration_seconds) || length(duration_seconds) != 1L ||
        !is.finite(duration_seconds) || duration_seconds < 0) {
      .stream_abort("`duration_seconds` must be one finite non-negative value",
                    "PhysioStream_validation_error")
    }
    selection <- "duration"
    all_rows <- ringPeek(buffer, fill, "oldest")
    if (!all_rows$count) {
      all_rows
    } else {
      keep <- all_rows$timestamps >=
        (all_rows$timestamps[[all_rows$count]] - duration_seconds)
      list(
        samples = all_rows$samples[keep, , drop = FALSE],
        timestamps = all_rows$timestamps[keep],
        sequence = all_rows$sequence[keep],
        count = as.integer(sum(keep)),
        stats_after = all_rows$stats_after
      )
    }
  } else {
    ringPeek(buffer, fill, "oldest")
  }

  if (selected$count < 2L) {
    .stream_abort("a stream snapshot requires at least two samples",
                  "PhysioStream_validation_error")
  }
  if (any(diff(selected$timestamps) <= 0)) {
    .stream_abort("snapshot timestamps are not strictly increasing",
                  "PhysioStream_validation_error")
  }
  if (ncol(selected$samples) != info@n_channels) {
    .stream_abort("snapshot channel count differs from StreamInfo",
                  "PhysioStream_validation_error")
  }
  selection_starts_oldest <- identical(
    selected$sequence[[1L]],
    stats_before$oldest_sequence
  )
  if (consume && (!selection_starts_oldest || selected$count != fill)) {
    .stream_abort(
      "`consume = TRUE` cannot skip earlier or later unread samples",
      "PhysioStream_state_error"
    )
  }
  if (consume) {
    selected <- ringPull(buffer, selected$count)
  }
  stats_after <- ringStats(buffer)

  sequence_labels <- paste0("stream_", format(
    selected$sequence,
    scientific = FALSE,
    trim = TRUE
  ))
  dimnames(selected$samples) <- list(sequence_labels, info@channel_names)
  units <- info@channel_units
  if (is.null(units)) {
    units <- rep(NA_character_, info@n_channels)
  }
  intervals <- diff(selected$timestamps)
  interval_median <- stats::median(intervals)
  interval_mad <- stats::median(abs(intervals - interval_median))
  stream_metadata <- list(
    info = info,
    source_class = class(x)[[1L]],
    selection = list(
      mode = selection,
      n = if (is.null(n)) NULL else as.integer(n),
      duration_seconds = duration_seconds,
      consume = consume
    ),
    stats_before = stats_before,
    stats_after = stats_after,
    observed = list(
      start = selected$timestamps[[1L]],
      end = selected$timestamps[[selected$count]],
      duration = selected$timestamps[[selected$count]] -
        selected$timestamps[[1L]],
      interval_median = interval_median,
      interval_mad = interval_mad,
      interval_max_deviation = max(abs(intervals - 1 / info@nominal_srate))
    ),
    loss_observed = isTRUE(stats_before$loss_observed),
    source_fingerprint = .stream_source_fingerprint(info)
  )
  if (methods::is(x, "LSLInlet")) {
    stream_metadata$transport <- .lsl_snapshot_metadata(x)
  }
  pe <- PhysioExperiment(
    assays = S4Vectors::SimpleList(
      stats::setNames(list(selected$samples), assay_name)
    ),
    rowData = S4Vectors::DataFrame(
      time_seconds = selected$timestamps,
      stream_sequence = selected$sequence,
      row.names = sequence_labels
    ),
    colData = S4Vectors::DataFrame(
      label = info@channel_names,
      stream_type = rep(info@type, info@n_channels),
      stream_dtype = rep(info@dtype, info@n_channels),
      unit = units,
      row.names = info@channel_names
    ),
    metadata = list(stream = stream_metadata),
    samplingRate = info@nominal_srate
  )
  if (nrow(assay(pe, assay_name)) != selected$count) {
    .stream_abort("constructed snapshot assay has an invalid row count",
                  "PhysioStream_buffer_error")
  }
  pe <- appendProvenance(
    pe,
    activity = "streamSnapshot",
    params = list(
      selection = selection,
      count = selected$count,
      consume = consume,
      source_fingerprint = stream_metadata$source_fingerprint
    ),
    input_assay = info@name,
    output_assay = assay_name,
    agent = "PhysioStream",
    package = "PhysioStream",
    software_version = as.character(utils::packageVersion("PhysioStream"))
  )
  methods::validObject(pe)
  pe
}
