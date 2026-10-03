.sync_streams <- function(x) {
  if (!methods::is(x, "MultiPhysioExperiment")) {
    .stream_abort(
      "`x` must be one MultiPhysioExperiment",
      "PhysioStream_sync_validation_error"
    )
  }
  streams <- as.list(PhysioExperiment::streams(x))
  keys <- names(streams)
  if (is.null(keys) || anyNA(keys) || any(!nzchar(keys)) ||
      anyDuplicated(keys)) {
    .stream_abort(
      "container stream keys must be unique non-empty strings",
      "PhysioStream_sync_validation_error"
    )
  }
  streams
}

.sync_row_data <- function(pe) {
  SummarizedExperiment::rowData(pe)
}

.sync_n_samples <- function(pe) {
  as.integer(nrow(SummarizedExperiment::assay(pe)))
}

.sync_regular_time <- function(pe, key, clock) {
  n <- .sync_n_samples(pe)
  if (!n) {
    return(numeric())
  }
  offset <- 0
  if (is.numeric(clock$offsets) && key %in% names(clock$offsets) &&
      is.finite(clock$offsets[[key]])) {
    offset <- as.numeric(clock$offsets[[key]])
  }
  rate <- PhysioExperiment::samplingRate(pe)
  if (n > 1L && (length(rate) != 1L || !is.finite(rate) || rate <= 0)) {
    .stream_abort(
      sprintf("stream `%s` has no explicit timestamps or positive rate", key),
      "PhysioStream_sync_timestamp_error"
    )
  }
  if (n == 1L) {
    return(offset)
  }
  offset + (seq_len(n) - 1) / rate
}

.sync_timestamp_source <- function(pe, key, clock, model, identity = FALSE) {
  row_data <- .sync_row_data(pe)
  columns <- names(row_data)
  n <- .sync_n_samples(pe)
  take <- function(name) {
    value <- .clock_numeric_vector(as.vector(row_data[[name]]), name)
    if (length(value) != n) {
      .stream_abort(
        sprintf("stream `%s` timestamp column `%s` has the wrong length",
                key, name),
        "PhysioStream_sync_timestamp_error"
      )
    }
    list(time = value, domain = name)
  }

  if (identity) {
    for (name in c("stream_time_master", "xdf_time", "time_seconds")) {
      if (name %in% columns) {
        return(take(name))
      }
    }
    return(list(
      time = .sync_regular_time(pe, key, clock),
      domain = "regular_time"
    ))
  }

  domain <- model$input_domain
  if (identical(domain, "device_time")) {
    for (name in c("xdf_time_raw", "xdf_time", "time_seconds")) {
      if (name %in% columns) {
        return(take(name))
      }
    }
    return(list(
      time = .sync_regular_time(pe, key, clock),
      domain = "regular_time"
    ))
  }
  if (identical(domain, "regular_time")) {
    return(list(
      time = .sync_regular_time(pe, key, clock),
      domain = "regular_time"
    ))
  }
  if (!(domain %in% columns)) {
    .stream_abort(
      sprintf("stream `%s` has no model input domain `%s`", key, domain),
      "PhysioStream_sync_timestamp_error"
    )
  }
  take(domain)
}

.sync_sample_segments <- function(pe, n) {
  row_data <- .sync_row_data(pe)
  if ("xdf_segment" %in% names(row_data)) {
    return(.clock_normalize_segments(
      as.vector(row_data$xdf_segment), n, "xdf_segment"
    ))
  }
  metadata <- S4Vectors::metadata(pe)$xdf
  if (is.list(metadata) && !is.null(metadata$segments)) {
    return(.clock_normalize_segments(metadata$segments, n, "xdf segments"))
  }
  .clock_normalize_segments(NULL, n, "segments")
}

.sync_identity_model <- function(timestamps, segments, input_domain) {
  normalized <- .clock_normalize_segments(
    segments, length(timestamps), "segments"
  )
  rows <- vector("list", nrow(normalized$ranges))
  origins <- numeric(length(rows))
  inliers <- vector("list", length(rows))
  for (segment in seq_along(rows)) {
    index <- which(normalized$labels == segment)
    time <- timestamps[index]
    origins[[segment]] <- if (length(time)) time[[1L]] else 0
    inliers[[segment]] <- logical(length(time))
    rows[[segment]] <- data.frame(
      segment = as.integer(segment),
      observation_start = if (length(index)) min(index) else 0,
      observation_end = if (length(index)) max(index) else 0,
      device_start = if (length(time)) min(time) else 0,
      device_end = if (length(time)) max(time) else 0,
      intercept = 0,
      drift = 0,
      drift_ppm = 0,
      rmse = 0,
      mad = 0,
      max_abs = 0,
      n_inlier = 0,
      n_total = 0,
      stringsAsFactors = FALSE
    )
  }
  table <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    empty <- lapply(.clock_segment_columns, function(name) {
      if (identical(name, "segment")) integer() else numeric()
    })
    names(empty) <- .clock_segment_columns
    as.data.frame(empty, stringsAsFactors = FALSE)
  }
  rownames(table) <- NULL
  .clock_finalize_model(list(
    schema = .clock_schema_version,
    method = "identity",
    origin = origins,
    origin_rule = "first",
    segments = table,
    sign_convention = "master = device + offset",
    input_domain = input_domain,
    quality = list(
      converged = TRUE,
      rank = rep.int(0L, nrow(table)),
      warnings = character()
    ),
    inliers = inliers,
    fit_details = vector("list", nrow(table)),
    parameters = list(),
    input_sha256 = digest::digest(
      list(timestamps = timestamps, segments = normalized$labels),
      algo = "sha256", serialize = TRUE
    ),
    evidence_sha256 = ""
  ))
}

.sync_set_model_domain <- function(model, domain) {
  .clock_validate_model(model)
  .stream_scalar_string(domain, "input_domain")
  model$input_domain <- domain
  .clock_finalize_model(model)
}

.sync_clock_observation_segments <- function(pe, clock_times) {
  n <- .sync_n_samples(pe)
  sample_segments <- .sync_sample_segments(pe, n)
  if (nrow(sample_segments$ranges) <= 1L) {
    return(NULL)
  }
  row_data <- .sync_row_data(pe)
  if (!("xdf_time_raw" %in% names(row_data))) {
    .stream_abort(
      "reset-segment clock fitting requires `xdf_time_raw` evidence",
      "PhysioStream_sync_clock_error"
    )
  }
  raw <- as.numeric(row_data$xdf_time_raw)
  bounds <- t(vapply(seq_len(nrow(sample_segments$ranges)), function(segment) {
    value <- raw[sample_segments$labels == segment]
    c(min(value), max(value))
  }, numeric(2)))
  labels <- vapply(clock_times, function(value) {
    distance <- pmax(bounds[, 1L] - value, value - bounds[, 2L], 0)
    which.min(distance)
  }, integer(1))
  runs <- rle(labels)$values
  if (!identical(runs, seq_len(nrow(bounds)))) {
    .stream_abort(
      "clock observations cannot be partitioned across XDF reset segments",
      "PhysioStream_sync_clock_error"
    )
  }
  labels
}

.sync_xdf_model <- function(pe) {
  metadata <- S4Vectors::metadata(pe)$xdf
  if (!is.list(metadata)) {
    return(NULL)
  }
  times <- metadata$clock_times
  values <- metadata$clock_values
  if (is.null(times) || is.null(values)) {
    return(NULL)
  }
  times <- .clock_numeric_vector(times, "clock_times")
  values <- .clock_numeric_vector(values, "clock_values")
  if (length(times) != length(values) || length(times) < 2L ||
      length(unique(times)) < 2L) {
    return(NULL)
  }
  segments <- .sync_clock_observation_segments(pe, times)
  .sync_set_model_domain(
    clockOffset(
      times, values, method = "huber", origin = "median",
      segments = segments
    ),
    "device_time"
  )
}

.sync_validate_models <- function(models, keys) {
  if (is.null(models)) {
    return(list())
  }
  if (!is.list(models) || is.data.frame(models) || !length(models) ||
      is.null(names(models)) || anyNA(names(models)) ||
      any(!nzchar(names(models))) || anyDuplicated(names(models)) ||
      any(!(names(models) %in% keys))) {
    .stream_abort(
      "`models` must be a uniquely named list keyed by container stream",
      "PhysioStream_sync_validation_error"
    )
  }
  for (model in models) {
    .clock_validate_model(model)
  }
  models
}

.sync_validate_events <- function(shared_events, keys) {
  if (is.null(shared_events)) {
    return(NULL)
  }
  if (!is.data.frame(shared_events) ||
      !identical(names(shared_events), c("event_id", "stream", "timestamp")) ||
      is.factor(shared_events$event_id) ||
      !is.character(shared_events$event_id) ||
      anyNA(shared_events$event_id) || any(!nzchar(shared_events$event_id)) ||
      is.factor(shared_events$stream) ||
      !is.character(shared_events$stream) ||
      anyNA(shared_events$stream) || any(!nzchar(shared_events$stream)) ||
      any(!(shared_events$stream %in% keys)) ||
      is.factor(shared_events$timestamp) ||
      !is.numeric(shared_events$timestamp) ||
      any(!is.finite(shared_events$timestamp)) ||
      anyDuplicated(paste(shared_events$stream, shared_events$event_id,
                          sep = "\r"))) {
    .stream_abort(
      "`shared_events` must have unique finite event_id/stream/timestamp rows",
      "PhysioStream_sync_event_error"
    )
  }
  data.frame(
    event_id = shared_events$event_id,
    stream = shared_events$stream,
    timestamp = as.numeric(shared_events$timestamp),
    stringsAsFactors = FALSE
  )
}

.sync_event_model <- function(events, key, master) {
  if (is.null(events)) {
    return(NULL)
  }
  device <- events[events$stream == key, , drop = FALSE]
  master_rows <- events[events$stream == master, , drop = FALSE]
  if (nrow(device) && any(!(device$event_id %in% master_rows$event_id))) {
    .stream_abort(
      sprintf("shared events for stream `%s` lack matching master ids", key),
      "PhysioStream_sync_event_error"
    )
  }
  ids <- intersect(device$event_id, master_rows$event_id)
  if (length(ids) < 2L) {
    return(NULL)
  }
  device <- device[match(ids, device$event_id), , drop = FALSE]
  master_rows <- master_rows[match(ids, master_rows$event_id), , drop = FALSE]
  order <- order(device$timestamp, device$event_id, method = "radix")
  device <- device[order, , drop = FALSE]
  master_rows <- master_rows[order, , drop = FALSE]
  model <- clockOffset(
    device$timestamp,
    master_rows$timestamp - device$timestamp,
    method = "huber",
    origin = "median"
  )
  model <- .sync_set_model_domain(model, "device_time")
  corrected <- driftCorrect(
    device$timestamp, model, extrapolate = "error"
  )
  list(
    model = model,
    count = length(ids),
    rmse = sqrt(mean((as.numeric(corrected) - master_rows$timestamp)^2))
  )
}

.sync_choose_master <- function(streams, clock) {
  keys <- names(streams)
  observations <- vapply(streams, function(pe) {
    metadata <- S4Vectors::metadata(pe)$xdf
    if (is.list(metadata) && is.numeric(metadata$clock_times)) {
      length(metadata$clock_times)
    } else {
      0L
    }
  }, integer(1))
  durations <- vapply(keys, function(key) {
    pe <- streams[[key]]
    source <- .sync_timestamp_source(
      pe, key, clock, model = NULL, identity = TRUE
    )$time
    if (length(source) < 2L) 0 else max(source) - min(source)
  }, numeric(1))
  order <- order(-observations, -durations, seq_along(keys), method = "radix")
  keys[[order[[1L]]]]
}

.sync_boolean <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .stream_abort(
      sprintf("`%s` must be TRUE or FALSE", name),
      "PhysioStream_sync_validation_error"
    )
  }
  x
}

#' Synchronize a multi-rate stream container
#'
#' Applies explicit clock models or recorded XDF/shared-event evidence without
#' interpolating sample values. Authoritative raw and master-domain timestamps
#' are appended to each stream's row data.
#'
#' @param x A `PhysioExperiment::MultiPhysioExperiment`.
#' @param master Exact master stream key, or `NULL` for deterministic choice.
#' @param models Optional uniquely named `StreamClockModel` list.
#' @param shared_events Optional exact `event_id`, `stream`, `timestamp` table.
#' @param dejitter Whether to append a nominal-grid timestamp column.
#' @param max_residual_seconds Optional non-negative residual gate.
#' @return A synchronized `PhysioExperiment::MultiPhysioExperiment`.
#' @examples
#' \dontrun{
#' # Requires a multi-stream container (MultiPhysioExperiment), e.g. from
#' # readXDF(); `master` names the reference stream key.
#' synced <- syncStreams(container, master = "eeg")
#' }
#' @export
syncStreams <- function(x, master = NULL, models = NULL,
                        shared_events = NULL, dejitter = FALSE,
                        max_residual_seconds = NULL) {
  streams <- .sync_streams(x)
  if (!length(streams)) {
    .stream_abort(
      "cannot synchronize a container with no streams",
      "PhysioStream_sync_validation_error"
    )
  }
  keys <- names(streams)
  clock <- PhysioExperiment::commonClock(x)
  models <- .sync_validate_models(models, keys)
  shared_events <- .sync_validate_events(shared_events, keys)
  dejitter <- .sync_boolean(dejitter, "dejitter")
  if (!is.null(max_residual_seconds) &&
      (!is.numeric(max_residual_seconds) ||
       length(max_residual_seconds) != 1L ||
       !is.finite(max_residual_seconds) || max_residual_seconds < 0)) {
    .stream_abort(
      "`max_residual_seconds` must be NULL or one non-negative number",
      "PhysioStream_sync_validation_error"
    )
  }
  residual_limit <- if (is.null(max_residual_seconds)) {
    Inf
  } else {
    as.numeric(max_residual_seconds)
  }
  if (is.null(master)) {
    master <- .sync_choose_master(streams, clock)
  } else if (is.factor(master) || !is.character(master) ||
             length(master) != 1L || is.na(master) ||
             !(master %in% keys)) {
    .stream_abort(
      "`master` must be NULL or one exact stream key",
      "PhysioStream_sync_validation_error"
    )
  }

  prepared <- vector("list", length(streams))
  names(prepared) <- keys
  event_evidence <- vector("list", length(streams))
  names(event_evidence) <- keys
  resolved_models <- vector("list", length(streams))
  names(resolved_models) <- keys

  for (key in keys) {
    pe <- streams[[key]]
    supplied <- models[[key]]
    if (.sync_n_samples(pe) == 0L && is.null(supplied)) {
      source <- list(time = numeric(), domain = "empty")
      segments <- .clock_normalize_segments(NULL, 0L, "segments")
      model <- .sync_identity_model(numeric(), NULL, "empty")
    } else if (identical(key, master) && is.null(supplied)) {
      source <- .sync_timestamp_source(
        pe, key, clock, model = NULL, identity = TRUE
      )
      segments <- .sync_sample_segments(pe, length(source$time))
      model <- .sync_identity_model(
        source$time, segments$labels, source$domain
      )
    } else {
      model <- supplied
      if (is.null(model)) {
        model <- .sync_xdf_model(pe)
      }
      if (is.null(model)) {
        event_fit <- .sync_event_model(shared_events, key, master)
        if (!is.null(event_fit)) {
          model <- event_fit$model
          event_evidence[[key]] <- event_fit
        }
      }
      if (is.null(model)) {
        .stream_abort(
          sprintf(
            "stream `%s` lacks a supplied model, clock evidence, or shared events",
            key
          ),
          "PhysioStream_sync_clock_error"
        )
      }
      source <- .sync_timestamp_source(pe, key, clock, model)
      segments <- .sync_sample_segments(pe, length(source$time))
    }
    .clock_validate_model(model)
    if (nrow(model$segments) != nrow(segments$ranges)) {
      .stream_abort(
        sprintf("stream `%s` reset segments do not match its clock model", key),
        "PhysioStream_sync_clock_error"
      )
    }
    if (length(source$time)) {
      corrected <- driftCorrect(
        source$time, model, segments = segments$labels,
        extrapolate = "bounded", max_extrapolation_seconds = 30
      )
    } else {
      corrected <- driftCorrect(source$time, model)
    }
    correction <- attr(corrected, "clock_correction")
    regular <- NULL
    jitter <- NULL
    if (dejitter && length(corrected)) {
      rate <- PhysioExperiment::samplingRate(pe)
      regular <- .clock_dejitter(
        as.numeric(corrected), rate, segments$labels,
        "least_squares", residual_limit
      )
      jitter <- attr(regular, "dejitter")
    }
    prepared[[key]] <- list(
      pe = pe,
      source = source,
      segments = segments,
      corrected = as.numeric(corrected),
      correction = correction,
      regular = if (is.null(regular)) NULL else as.numeric(regular),
      jitter = jitter
    )
    resolved_models[[key]] <- model
  }

  diagnostics <- .sync_build_diagnostics(
    prepared, resolved_models, event_evidence, residual_limit
  )
  all_times <- unlist(lapply(prepared, function(item) {
    item$corrected
  }), use.names = FALSE)
  t0 <- if (length(all_times)) min(all_times) else 0
  offsets <- stats::setNames(vapply(prepared, function(item) {
    if (length(item$corrected)) item$corrected[[1L]] - t0 else 0
  }, numeric(1)), keys)
  reference_rate <- clock$reference_rate
  if (length(reference_rate) != 1L || !is.finite(reference_rate) ||
      reference_rate <= 0) {
    rates <- vapply(streams, PhysioExperiment::samplingRate, numeric(1))
    rates <- rates[is.finite(rates) & rates > 0]
    reference_rate <- if (length(rates)) max(rates) else NA_real_
  }
  sync_metadata <- list(
    schema = .clock_schema_version,
    master = master,
    model_sha256 = vapply(
      resolved_models, function(model) model$evidence_sha256, character(1)
    ),
    input_domain = vapply(
      prepared, function(item) item$source$domain, character(1)
    ),
    sign_convention = "master = device + offset",
    dejitter = dejitter,
    max_residual_seconds = if (is.null(max_residual_seconds)) {
      NULL
    } else {
      residual_limit
    },
    diagnostics = diagnostics
  )
  payload <- serialize(sync_metadata, NULL, version = 3L)
  if (length(payload) > .stream_metadata_limit) {
    .stream_abort(
      "synchronization metadata exceeds the 1 MiB governed ceiling",
      "PhysioStream_sync_resource_error"
    )
  }

  output_streams <- lapply(keys, function(key) {
    item <- prepared[[key]]
    pe <- item$pe
    row_data <- SummarizedExperiment::rowData(pe)
    row_data$stream_time_raw <- item$source$time
    row_data$stream_time_master <- item$corrected
    if (!is.null(item$regular)) {
      row_data$stream_time_dejittered <- item$regular
    }
    SummarizedExperiment::rowData(pe) <- row_data
    pe <- PhysioExperiment::appendProvenance(
      pe,
      activity = "syncStreams",
      params = list(
        stream = key,
        master = master,
        model_sha256 = resolved_models[[key]]$evidence_sha256,
        input_domain = item$source$domain,
        dejitter = dejitter
      ),
      software_version = "0.4.0",
      package = "PhysioStream"
    )
    methods::validObject(pe)
    pe
  })
  names(output_streams) <- keys
  output_clock <- clock
  output_clock$t0 <- t0
  output_clock$reference_rate <- reference_rate
  output_clock$offsets <- offsets
  output_clock$sync <- sync_metadata
  result <- PhysioExperiment::MultiRatePhysioExperiment(
    streams = output_streams, clock = output_clock
  )
  methods::validObject(result)
  result
}
