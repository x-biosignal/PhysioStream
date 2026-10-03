.sync_quantile95 <- function(x) {
  if (!length(x)) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(abs(x), 0.95, names = FALSE, type = 7))
}

.sync_diagnostic_row <- function(key, segment, item, model,
                                 event_evidence, residual_limit) {
  if (is.na(segment)) {
    index <- integer()
    model_row <- NULL
  } else {
    index <- which(item$segments$labels == segment)
    model_row <- model$segments[segment, , drop = FALSE]
  }
  raw <- item$source$time[index]
  master <- item$corrected[index]
  correction <- item$correction
  extrapolation <- if (length(index)) {
    correction$extrapolation_seconds[index]
  } else {
    numeric()
  }
  jitter_residual <- if (!is.null(item$jitter) && length(index)) {
    item$jitter$residual[index]
  } else {
    numeric()
  }
  clock_rmse <- if (is.null(model_row)) 0 else model_row$rmse[[1L]]
  event_count <- if (is.null(event_evidence)) 0L else event_evidence$count
  event_rmse <- if (is.null(event_evidence)) NA_real_ else event_evidence$rmse
  jitter_rmse <- if (length(jitter_residual)) {
    sqrt(mean(jitter_residual^2))
  } else {
    NA_real_
  }
  jitter_max <- if (length(jitter_residual)) {
    max(abs(jitter_residual))
  } else {
    NA_real_
  }
  failure <- (is.finite(residual_limit) && clock_rmse > residual_limit) ||
    (is.finite(residual_limit) && is.finite(event_rmse) &&
     event_rmse > residual_limit) ||
    (is.finite(residual_limit) && is.finite(jitter_max) &&
     jitter_max > residual_limit)
  extrapolated_fraction <- if (length(extrapolation)) {
    mean(extrapolation > 0)
  } else {
    0
  }
  quality <- if (failure) {
    "fail"
  } else if (extrapolated_fraction > 0) {
    "warn"
  } else {
    "ok"
  }
  data.frame(
    stream = key,
    segment = if (is.na(segment)) NA_integer_ else as.integer(segment),
    n_samples = length(index),
    n_clock_observations = if (is.null(model_row)) {
      0L
    } else {
      as.integer(model_row$n_total[[1L]])
    },
    start_master = if (length(master)) master[[1L]] else NA_real_,
    end_master = if (length(master)) master[[length(master)]] else NA_real_,
    offset_start_seconds = if (length(master)) {
      master[[1L]] - raw[[1L]]
    } else {
      NA_real_
    },
    offset_end_seconds = if (length(master)) {
      master[[length(master)]] - raw[[length(raw)]]
    } else {
      NA_real_
    },
    drift_ppm = if (is.null(model_row)) 0 else model_row$drift_ppm[[1L]],
    clock_rmse_seconds = clock_rmse,
    jitter_rmse_seconds = jitter_rmse,
    jitter_p95_seconds = .sync_quantile95(jitter_residual),
    jitter_max_seconds = jitter_max,
    shared_event_count = as.integer(event_count),
    shared_event_rmse_seconds = event_rmse,
    extrapolated_fraction = extrapolated_fraction,
    quality = quality,
    stringsAsFactors = FALSE
  )
}

.sync_build_diagnostics <- function(prepared, models, event_evidence,
                                    residual_limit) {
  rows <- list()
  cursor <- 0L
  for (key in names(prepared)) {
    item <- prepared[[key]]
    model <- models[[key]]
    segments <- if (nrow(item$segments$ranges)) {
      seq_len(nrow(item$segments$ranges))
    } else {
      NA_integer_
    }
    for (segment in segments) {
      cursor <- cursor + 1L
      rows[[cursor]] <- .sync_diagnostic_row(
        key, segment, item, model, event_evidence[[key]], residual_limit
      )
    }
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  if (any(result$quality == "fail")) {
    failed <- unique(result$stream[result$quality == "fail"])
    .stream_abort(
      sprintf(
        "synchronization residual gate failed for stream(s): %s",
        paste(failed, collapse = ", ")
      ),
      "PhysioStream_sync_residual_error"
    )
  }
  result
}

#' Summarize governed stream synchronization
#'
#' Returns stored diagnostics for a synchronized container. For an
#' unsynchronized container, models or shared events are evaluated through
#' `syncStreams()` on a copy and only the diagnostics table is returned.
#'
#' @param x A `PhysioExperiment::MultiPhysioExperiment`.
#' @param models Optional uniquely named `StreamClockModel` list.
#' @param shared_events Optional exact `event_id`, `stream`, `timestamp` table.
#' @return One plain data-frame row per stream/reset segment.
#' @examples
#' \dontrun{
#' # Requires a multi-stream container (MultiPhysioExperiment), e.g. from
#' # readXDF().
#' diagnostics <- syncDiagnostics(container)
#' }
#' @export
syncDiagnostics <- function(x, models = NULL, shared_events = NULL) {
  .sync_streams(x)
  clock <- PhysioExperiment::commonClock(x)
  if (is.null(models) && is.null(shared_events) &&
      is.list(clock$sync) &&
      identical(clock$sync$schema, .clock_schema_version) &&
      is.data.frame(clock$sync$diagnostics)) {
    return(clock$sync$diagnostics)
  }
  master <- if (is.list(clock$sync) &&
                is.character(clock$sync$master) &&
                length(clock$sync$master) == 1L) {
    clock$sync$master
  } else {
    NULL
  }
  synchronized <- syncStreams(
    x, master = master, models = models, shared_events = shared_events,
    dejitter = FALSE
  )
  PhysioExperiment::commonClock(synchronized)$sync$diagnostics
}
