.xdf_scalar_path <- function(path, name = "path") {
  .stream_scalar_string(path, name)
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path, perl = TRUE)) {
    .stream_abort(
      sprintf("`%s` must be a local filesystem path, not a URL", name),
      "PhysioStream_validation_error"
    )
  }
  if (!identical(tolower(tools::file_ext(path)), "xdf")) {
    .stream_abort(
      sprintf("`%s` must have extension `.xdf`", name),
      "PhysioStream_validation_error"
    )
  }
  path
}

.xdf_read_path <- function(path, max_file_bytes) {
  path <- .xdf_scalar_path(path)
  max_file_bytes <- .xdf_exact_number(
    max_file_bytes, "max_file_bytes", 1, 2^53 - 1
  )
  if (!file.exists(path) || dir.exists(path)) {
    .stream_abort(
      "`path` must name an existing regular XDF file",
      "PhysioStream_validation_error"
    )
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(normalized)
  if (!isTRUE(utils::file_test("-f", normalized)) ||
      !isTRUE(info$isdir[[1L]] == FALSE) || !is.finite(info$size[[1L]]) ||
      info$size[[1L]] > max_file_bytes ||
      info$size[[1L]] > .xdf_allocation_limit) {
    .stream_abort(
      "XDF input is not a regular file within `max_file_bytes`",
      "PhysioStream_xdf_resource_error"
    )
  }
  list(
    path = normalized,
    size = unname(info$size[[1L]]),
    max_file_bytes = max_file_bytes,
    sha256 = digest::digest(
      file = normalized, algo = "sha256", serialize = FALSE
    )
  )
}

.xdf_backend_streams <- function(value) {
  if (!is.list(value)) {
    .stream_abort(
      "pyxdf returned an invalid stream collection",
      "PhysioStream_xdf_backend_error"
    )
  }
  if (!length(value)) {
    return(list())
  }
  ids <- vapply(value, function(stream) {
    if (!is.list(stream) || is.null(stream$stream_id)) {
      .stream_abort(
        "pyxdf returned a stream without an id",
        "PhysioStream_xdf_backend_error"
      )
    }
    id <- suppressWarnings(as.numeric(stream$stream_id))
    if (length(id) != 1L || !is.finite(id) || id != floor(id) ||
        id < 0 || id > 2^32 - 1) {
      .stream_abort(
        "pyxdf returned an invalid uint32 stream id",
        "PhysioStream_xdf_backend_error"
      )
    }
    .xdf_id_text(id)
  }, character(1))
  if (anyDuplicated(ids)) {
    .stream_abort(
      "pyxdf returned duplicate stream ids",
      "PhysioStream_xdf_backend_error"
    )
  }
  names(value) <- ids
  value
}

.xdf_backend_number <- function(value, name, minimum = -Inf,
                                integer = FALSE) {
  value <- suppressWarnings(as.numeric(value))
  invalid <- length(value) != 1L || !is.finite(value) || value < minimum
  if (integer) {
    invalid <- invalid || value != floor(value)
  }
  if (invalid) {
    .stream_abort(
      sprintf("pyxdf returned invalid `%s`", name),
      "PhysioStream_xdf_backend_error"
    )
  }
  value
}

.xdf_backend_matrix <- function(value, n_samples, n_channels, format) {
  if (!n_samples) {
    return(matrix(
      if (identical(format, "string")) character() else numeric(),
      nrow = 0L, ncol = n_channels
    ))
  }
  rows <- if (is.matrix(value)) {
    lapply(seq_len(nrow(value)), function(i) value[i, , drop = TRUE])
  } else {
    value
  }
  if (!is.list(rows) || length(rows) != n_samples) {
    .stream_abort(
      "pyxdf returned a ragged or wrong-length sample collection",
      "PhysioStream_xdf_backend_error"
    )
  }
  mode <- if (identical(format, "string")) "character" else "numeric"
  rows <- lapply(rows, function(row) {
    row <- unlist(row, recursive = TRUE, use.names = FALSE)
    if (length(row) != n_channels) {
      .stream_abort(
        "pyxdf returned a sample with the wrong channel count",
        "PhysioStream_xdf_backend_error"
      )
    }
    if (identical(mode, "character")) {
      if (!is.character(row) || anyNA(row)) {
        .stream_abort(
          "pyxdf returned invalid string samples",
          "PhysioStream_xdf_backend_error"
        )
      }
      enc2utf8(row)
    } else {
      row <- as.numeric(row)
      if (any(!is.finite(row))) {
        .stream_abort(
          "pyxdf returned non-finite numeric samples",
          "PhysioStream_xdf_backend_error"
        )
      }
      row
    }
  })
  matrix(
    unlist(rows, recursive = FALSE, use.names = FALSE),
    nrow = n_samples, ncol = n_channels, byrow = TRUE
  )
}

.xdf_backend_segments <- function(value, n_samples) {
  if (!n_samples) {
    if (length(value)) {
      .stream_abort(
        "pyxdf returned segments for an empty stream",
        "PhysioStream_xdf_backend_error"
      )
    }
    return(list(ranges = matrix(integer(), 0L, 2L), labels = integer()))
  }
  if (is.matrix(value)) {
    value <- lapply(seq_len(nrow(value)), function(i) value[i, ])
  }
  if (!is.list(value) || !length(value)) {
    .stream_abort(
      "pyxdf omitted segments for a non-empty stream",
      "PhysioStream_xdf_backend_error"
    )
  }
  ranges <- t(vapply(value, function(pair) {
    pair <- suppressWarnings(as.numeric(unlist(pair, use.names = FALSE)))
    if (length(pair) != 2L || any(!is.finite(pair)) ||
        any(pair != floor(pair)) || pair[[1L]] < 0 ||
        pair[[2L]] < pair[[1L]] || pair[[2L]] >= n_samples) {
      .stream_abort(
        "pyxdf returned invalid segment bounds",
        "PhysioStream_xdf_backend_error"
      )
    }
    as.integer(pair)
  }, integer(2)))
  if (ranges[[1L, 1L]] != 0L ||
      ranges[[nrow(ranges), 2L]] != n_samples - 1L ||
      (nrow(ranges) > 1L &&
       any(ranges[-1L, 1L] != ranges[-nrow(ranges), 2L] + 1L))) {
    .stream_abort(
      "pyxdf segments do not exactly partition the samples",
      "PhysioStream_xdf_backend_error"
    )
  }
  labels <- integer(n_samples)
  for (i in seq_len(nrow(ranges))) {
    labels[seq.int(ranges[i, 1L] + 1L, ranges[i, 2L] + 1L)] <- i
  }
  list(ranges = ranges, labels = labels)
}

.xdf_validate_backend_core <- function(stream, header) {
  exact <- c("name", "type", "source_id", "uid", "channel_format")
  for (name in exact) {
    backend_value <- stream[[name]]
    header_value <- header[[name]]
    if (is.null(backend_value)) {
      backend_value <- ""
    }
    if (!identical(as.character(backend_value), as.character(header_value))) {
      .stream_abort(
        sprintf("pyxdf `%s` differs from the native StreamHeader", name),
        "PhysioStream_xdf_backend_error"
      )
    }
  }
  count <- .xdf_backend_number(
    stream$channel_count, "channel_count", 1, integer = TRUE
  )
  rate <- .xdf_backend_number(
    stream$nominal_srate, "nominal_srate", 0
  )
  if (count != header$channel_count || rate != header$nominal_srate) {
    .stream_abort(
      "pyxdf numeric header fields differ from the native StreamHeader",
      "PhysioStream_xdf_backend_error"
    )
  }
  invisible(TRUE)
}

.xdf_prepare_backend_stream <- function(stream, native) {
  .xdf_validate_backend_core(stream, native$header)
  n_samples <- native$sample_count
  timestamps <- suppressWarnings(as.numeric(
    unlist(stream$time_stamps, recursive = TRUE, use.names = FALSE)
  ))
  if (length(timestamps) != n_samples || any(!is.finite(timestamps))) {
    .stream_abort(
      "pyxdf timestamps do not match the native sample count",
      "PhysioStream_xdf_backend_error"
    )
  }
  values <- .xdf_backend_matrix(
    stream$time_series, n_samples, native$header$channel_count,
    native$header$channel_format
  )
  segments <- .xdf_backend_segments(stream$segments, n_samples)
  clock_times <- suppressWarnings(as.numeric(
    unlist(stream$clock_times, recursive = TRUE, use.names = FALSE)
  ))
  clock_values <- suppressWarnings(as.numeric(
    unlist(stream$clock_values, recursive = TRUE, use.names = FALSE)
  ))
  if (length(clock_times) != length(clock_values) ||
      any(!is.finite(c(clock_times, clock_values)))) {
    .stream_abort(
      "pyxdf returned invalid clock observations",
      "PhysioStream_xdf_backend_error"
    )
  }
  effective_srate <- .xdf_backend_number(
    stream$effective_srate, "effective_srate"
  )
  list(
    values = values,
    timestamps = timestamps,
    segments = segments,
    clock_times = clock_times,
    clock_values = clock_values,
    effective_srate = effective_srate
  )
}

.xdf_select_native <- function(native, streams) {
  if (is.null(streams)) {
    return(native)
  }
  if (is.factor(streams) || anyNA(streams) || !length(streams)) {
    .stream_abort(
      "`streams` must be NULL or a non-empty unique id/name vector",
      "PhysioStream_validation_error"
    )
  }
  ids <- vapply(native, function(x) .xdf_id_text(x$id), character(1))
  names <- vapply(native, function(x) x$header$name, character(1))
  if (is.numeric(streams)) {
    selected <- vapply(streams, function(value) {
      value <- .xdf_exact_number(value, "streams", 0, 2^32 - 1)
      match(.xdf_id_text(value), ids)
    }, integer(1))
  } else if (is.character(streams) &&
             all(nzchar(streams)) && !anyDuplicated(streams)) {
    selected <- vapply(streams, function(value) {
      by_id <- match(value, ids)
      by_name <- which(names == value)
      candidates <- unique(c(by_id[!is.na(by_id)], by_name))
      if (length(candidates) != 1L) {
        .stream_abort(
          sprintf("XDF stream selector `%s` is missing or ambiguous", value),
          "PhysioStream_validation_error"
        )
      }
      candidates
    }, integer(1))
  } else {
    .stream_abort(
      "`streams` must be NULL or a non-empty unique id/name vector",
      "PhysioStream_validation_error"
    )
  }
  if (anyNA(selected) || anyDuplicated(selected)) {
    .stream_abort(
      "`streams` contains an unknown or duplicate stream id",
      "PhysioStream_validation_error"
    )
  }
  native[selected]
}

.xdf_marker_events <- function(values, timestamps, t0, header,
                               segments, raw_timestamps, descriptor_sha256,
                               synchronize, dejitter) {
  payload <- if (!nrow(values)) {
    character()
  } else if (ncol(values) == 1L) {
    values[, 1L]
  } else {
    vapply(seq_len(nrow(values)), function(i) {
      .lsl_marker_json(values[i, ], header$channel_names)
    }, character(1))
  }
  event_type <- if (nzchar(header$type)) header$type else "marker"
  events <- PhysioExperiment::PhysioEvents(
    onset = timestamps - t0,
    duration = rep(0, length(timestamps)),
    type = rep(event_type, length(timestamps)),
    value = payload
  )
  attr(events, "xdf") <- list(
    original_timestamps = timestamps,
    raw_timestamps = raw_timestamps,
    stream_id = header$stream_id,
    segment = segments,
    descriptor_sha256 = descriptor_sha256,
    synchronize = synchronize,
    dejitter = dejitter,
    mapping_schema = "1.0.0"
  )
  methods::validObject(events)
  events
}

.xdf_construct_stream <- function(native, output, raw, t0, source,
                                  backend_info, synchronize, dejitter,
                                  keep_raw_timestamps) {
  header <- native$header
  values <- output$values
  row_names <- if (nrow(values)) {
    paste0(
      "xdf_", .xdf_id_text(native$id), "_",
      sprintf("%010d", seq_len(nrow(values)))
    )
  } else {
    character()
  }
  dimnames(values) <- list(row_names, header$channel_names)
  units <- if (is.null(header$channel_units)) {
    rep(NA_character_, header$channel_count)
  } else {
    header$channel_units
  }
  row_data <- S4Vectors::DataFrame(
    xdf_time = output$timestamps,
    time_from_t0 = output$timestamps - t0,
    xdf_segment = output$segments$labels,
    row.names = row_names
  )
  if (keep_raw_timestamps) {
    row_data$xdf_time_raw <- raw$timestamps
  }
  metadata <- list(
    schema = "1.0.0",
    stream_id = native$id,
    name = header$name,
    type = header$type,
    source_id = header$source_id,
    uid = header$uid,
    hostname = header$hostname,
    session_id = header$session_id,
    created_at = header$created_at,
    nominal_srate = header$nominal_srate,
    channel_format = header$channel_format,
    stream_header_xml = native$header_xml,
    stream_header_sha256 = native$header_sha256,
    stream_footer_xml = native$footer_xml,
    stream_footer_sha256 = native$footer_sha256,
    clock_times = native$clock_times,
    clock_values = native$clock_values,
    segments = output$segments$ranges,
    effective_srate = output$effective_srate,
    synchronize = synchronize,
    dejitter = dejitter,
    source_file_sha256 = source$sha256,
    source_path = source$path,
    backend = backend_info,
    channel_metadata_synthesized = header$channel_metadata_synthesized
  )
  metadata_payload <- tryCatch(
    serialize(metadata, NULL, version = 3L),
    error = function(e) raw(.stream_metadata_limit + 1L)
  )
  if (length(metadata_payload) > .stream_metadata_limit) {
    .stream_abort(
      sprintf(
        "XDF stream `%s` metadata exceeds the 1 MiB governed ceiling",
        .xdf_id_text(native$id)
      ),
      "PhysioStream_xdf_resource_error"
    )
  }
  pe <- PhysioExperiment(
    assays = S4Vectors::SimpleList(xdf = values),
    rowData = row_data,
    colData = S4Vectors::DataFrame(
      label = header$channel_names,
      unit = units,
      type = header$channel_types,
      hardware_index = header$hardware_index,
      row.names = header$channel_names
    ),
    metadata = list(xdf = metadata),
    samplingRate = if (header$nominal_srate > 0) {
      header$nominal_srate
    } else {
      NA_real_
    },
    provenance = paste0(source$path, "#sha256=", source$sha256)
  )
  if (!nrow(values)) {
    SummarizedExperiment::rowData(pe) <- row_data
  }
  if (identical(header$channel_format, "string")) {
    events <- .xdf_marker_events(
      values, output$timestamps, t0, header,
      output$segments$labels,
      if (keep_raw_timestamps) raw$timestamps else NULL,
      native$header_sha256, synchronize, dejitter
    )
    pe <- PhysioExperiment::setEvents(pe, events)
  }
  methods::validObject(pe)
  pe
}

#' Read an Extensible Data Format file
#'
#' XDF timestamps are authoritative in `rowData(x)$xdf_time`; the regular
#' `streamTimeIndex()` grid of the returned multi-rate container is only a
#' nominal convenience for jittered or irregular streams. Files and converted
#' payloads are also bounded by a private 512 MiB in-memory ceiling even when
#' `max_file_bytes` is larger.
#'
#' @param path Existing local `.xdf` file.
#' @param streams Optional unique stream ids or unambiguous stream names.
#' @param synchronize Whether pyxdf clock synchronization is applied.
#' @param dejitter Whether pyxdf timestamp de-jittering is applied.
#' @param keep_raw_timestamps Whether a second uncorrected pass is retained.
#' @param max_file_bytes Exact positive file-size ceiling.
#' @param backend Exact configured backend.
#' @return A valid [PhysioExperiment::MultiPhysioExperiment].
#' @examples
#' \donttest{
#' # Reading XDF requires the pyxdf backend (via reticulate).
#' if (xdfAvailable()) {
#'   path <- system.file("extdata", "xdf-minimal.xdf",
#'                       package = "PhysioStream")
#'   container <- readXDF(path)
#' }
#' }
#' @export
readXDF <- function(path, streams = NULL, synchronize = TRUE, dejitter = TRUE,
                    keep_raw_timestamps = TRUE,
                    max_file_bytes = 2^31 - 1,
                    backend = getOption(
                      "PhysioStream.xdf_backend", "auto"
                    )) {
  synchronize <- .lsl_scalar_logical(synchronize, "synchronize")
  dejitter <- .lsl_scalar_logical(dejitter, "dejitter")
  keep_raw_timestamps <- .lsl_scalar_logical(
    keep_raw_timestamps, "keep_raw_timestamps"
  )
  backend <- .xdf_backend_arg(backend)
  source <- .xdf_read_path(path, max_file_bytes)
  scan <- .xdf_scan_file(
    source$path, min(source$max_file_bytes, .xdf_allocation_limit)
  )
  native <- .xdf_select_native(scan$streams, streams)
  converted_bytes <- 0
  for (stream in native) {
    cells <- stream$sample_count * stream$header$channel_count
    if (!is.finite(cells) || cells != floor(cells) ||
        cells > .Machine$integer.max) {
      .stream_abort(
        sprintf(
          "XDF stream `%s` dimensions exceed the governed matrix ceiling",
          .xdf_id_text(stream$id)
        ),
        "PhysioStream_xdf_resource_error"
      )
    }
    converted_bytes <- converted_bytes + cells * 8 +
      stream$sample_count * 4
    if (!is.finite(converted_bytes) ||
        converted_bytes > .xdf_allocation_limit) {
      .stream_abort(
        "converted XDF payload exceeds the internal allocation ceiling",
        "PhysioStream_xdf_resource_error"
      )
    }
  }
  unsupported <- vapply(
    native,
    function(x) identical(x$header$channel_format, "int64"),
    logical(1)
  )
  if (any(unsupported)) {
    stream <- native[[which(unsupported)[[1L]]]]
    .stream_abort(
      sprintf(
        "XDF stream `%s` uses unsupported format `int64`",
        .xdf_id_text(stream$id)
      ),
      "PhysioStream_xdf_unsupported_format"
    )
  }

  adapter <- .xdf_import_adapter(backend)
  backend_info <- adapter$backend_info()
  output_all <- .xdf_backend_streams(adapter$load(
    source$path, synchronize, dejitter
  ))
  raw_all <- if (keep_raw_timestamps &&
                 (synchronize || dejitter)) {
    .xdf_backend_streams(adapter$load(source$path, FALSE, FALSE))
  } else {
    output_all
  }
  native_ids <- vapply(
    scan$streams, function(x) .xdf_id_text(x$id), character(1)
  )
  if (!setequal(names(output_all), native_ids) ||
      !setequal(names(raw_all), native_ids)) {
    .stream_abort(
      "pyxdf omitted or added streams relative to native headers",
      "PhysioStream_xdf_backend_error"
    )
  }

  prepared <- lapply(native, function(stream) {
    id <- .xdf_id_text(stream$id)
    output <- .xdf_prepare_backend_stream(output_all[[id]], stream)
    raw <- .xdf_prepare_backend_stream(raw_all[[id]], stream)
    if (!identical(output$clock_times, stream$clock_times) ||
        !identical(output$clock_values, stream$clock_values) ||
        !identical(raw$clock_times, stream$clock_times) ||
        !identical(raw$clock_values, stream$clock_values)) {
      .stream_abort(
        sprintf("pyxdf clock observations differ for stream `%s`", id),
        "PhysioStream_xdf_backend_error"
      )
    }
    if (output$effective_srate < 0) {
      .stream_abort(
        sprintf("pyxdf returned a negative processed rate for stream `%s`", id),
        "PhysioStream_xdf_backend_error"
      )
    }
    same_values <- identical(output$values, raw$values)
    if (!same_values) {
      .stream_abort(
        sprintf(
          "pyxdf timestamp processing changed samples for stream `%s`", id
        ),
        "PhysioStream_xdf_backend_error"
      )
    }
    if (synchronize && length(output$timestamps)) {
      for (segment in unique(output$segments$labels)) {
        times <- output$timestamps[output$segments$labels == segment]
        if (length(times) > 1L && any(diff(times) < 0)) {
          .stream_abort(
            sprintf(
              "synchronized timestamps decrease within stream `%s` segment",
              id
            ),
            "PhysioStream_xdf_backend_error"
          )
        }
      }
    }
    list(native = stream, output = output, raw = raw)
  })
  nonempty_times <- unlist(lapply(
    prepared, function(x) x$output$timestamps
  ), use.names = FALSE)
  t0 <- if (length(nonempty_times)) min(nonempty_times) else 0
  keys <- vapply(native, function(x) {
    paste0("xdf_stream_", .xdf_id_text(x$id))
  }, character(1))
  experiments <- lapply(prepared, function(item) {
    .xdf_construct_stream(
      item$native, item$output, item$raw, t0, source,
      backend_info, synchronize, dejitter, keep_raw_timestamps
    )
  })
  names(experiments) <- keys
  rates <- vapply(
    native, function(x) x$header$nominal_srate, numeric(1)
  )
  positive_rates <- rates[is.finite(rates) & rates > 0]
  reference_rate <- if (length(positive_rates)) max(positive_rates) else NA_real_
  offsets <- stats::setNames(vapply(prepared, function(item) {
    if (length(item$output$timestamps)) {
      item$output$timestamps[[1L]] - t0
    } else {
      0
    }
  }, numeric(1)), keys)
  clock <- list(
    t0 = t0,
    reference_rate = reference_rate,
    offsets = offsets,
    xdf = list(
      schema = "1.0.0",
      file_header_xml = scan$file_header_xml,
      file_header_sha256 = scan$file_header_sha256,
      source_path = source$path,
      source_size = source$size,
      source_sha256 = source$sha256,
      synchronize = synchronize,
      dejitter = dejitter,
      backend = backend_info
    )
  )
  result <- PhysioExperiment::MultiRatePhysioExperiment(
    streams = experiments, clock = clock
  )
  methods::validObject(result)

  after <- file.info(source$path)
  after_hash <- digest::digest(
    file = source$path, algo = "sha256", serialize = FALSE
  )
  if (!is.finite(after$size[[1L]]) ||
      unname(after$size[[1L]]) != source$size ||
      !identical(after_hash, source$sha256)) {
    .stream_abort(
      "XDF file changed while it was being parsed",
      "PhysioStream_xdf_concurrent_change"
    )
  }
  result
}

.xdf_col_character <- function(col_data, candidates, n, default = "") {
  found <- candidates[candidates %in% names(col_data)]
  if (!length(found)) {
    if (length(default) == 1L) {
      return(rep(default, n))
    }
    if (length(default) != n) {
      .stream_abort(
        "default channel metadata has the wrong length",
        "PhysioStream_xdf_validation_error"
      )
    }
    return(as.character(default))
  }
  value <- as.character(col_data[[found[[1L]]]])
  if (length(value) != n) {
    .stream_abort(
      sprintf("channel metadata `%s` has the wrong length", found[[1L]]),
      "PhysioStream_xdf_validation_error"
    )
  }
  value
}

.xdf_export_format <- function(metadata, values) {
  format <- metadata$xdf$channel_format
  if (is.null(format)) {
    info <- metadata$stream$info
    dtype <- if (methods::is(info, "StreamInfo")) {
      info@dtype
    } else {
      metadata$stream$dtype
    }
    if (!is.null(dtype) && length(dtype) == 1L) {
      format <- switch(
        dtype,
        float64 = "double64",
        float32 = "float32",
        int32 = "int32",
        int16 = "int16",
        int8 = "int8",
        string = "string",
        NULL
      )
    }
  }
  if (!is.character(format) || length(format) != 1L || is.na(format) ||
      !(format %in% setdiff(names(.xdf_wire_formats), "int64"))) {
    .stream_abort(
      "each stream requires an explicit supported XDF channel format",
      "PhysioStream_xdf_unsupported_format"
    )
  }
  if (identical(format, "string") && !is.character(values)) {
    .stream_abort(
      "XDF string streams require a character assay",
      "PhysioStream_xdf_validation_error"
    )
  }
  if (!identical(format, "string") && !is.numeric(values)) {
    .stream_abort(
      "XDF numeric streams require a numeric assay",
      "PhysioStream_xdf_validation_error"
    )
  }
  if (identical(format, "string")) {
    encoded <- enc2utf8(values)
    valid <- iconv(encoded, from = "UTF-8", to = "UTF-8", sub = NA)
    if (anyNA(valid)) {
      .stream_abort(
        "XDF string streams require valid UTF-8 values",
        "PhysioStream_xdf_validation_error"
      )
    }
  }
  format
}

.xdf_export_record <- function(pe, key, output_id, clock, timestamps) {
  assays <- SummarizedExperiment::assays(pe)
  if (!length(assays)) {
    .stream_abort(
      sprintf("stream `%s` has no assay", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  assay_name <- if ("xdf" %in% names(assays)) {
    "xdf"
  } else {
    PhysioExperiment::defaultAssay(pe)
  }
  values <- SummarizedExperiment::assay(pe, assay_name)
  if (length(dim(values)) != 2L) {
    .stream_abort(
      sprintf("stream `%s` assay must be two-dimensional", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  values <- as.matrix(values)
  if (anyNA(values) ||
      (is.numeric(values) && any(!is.finite(values)))) {
    .stream_abort(
      sprintf("stream `%s` assay contains missing/non-finite values", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  metadata <- S4Vectors::metadata(pe)
  if (is.null(metadata$xdf)) {
    metadata$xdf <- list()
  }
  format <- .xdf_export_format(metadata, values)
  n_channels <- ncol(values)
  if (n_channels < 1L) {
    .stream_abort(
      sprintf("stream `%s` must have at least one channel", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  col_data <- SummarizedExperiment::colData(pe)
  channel_names <- .xdf_col_character(
    col_data, c("label"), n_channels,
    default = if (!is.null(colnames(values))) colnames(values) else ""
  )
  if (length(channel_names) != n_channels || anyNA(channel_names) ||
      any(!nzchar(channel_names)) || anyDuplicated(channel_names)) {
    .stream_abort(
      sprintf("stream `%s` requires unique non-empty channel labels", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  units <- .xdf_col_character(col_data, c("unit"), n_channels, NA_character_)
  if (all(is.na(units) | !nzchar(units))) {
    units <- NULL
  } else if (any(is.na(units) | !nzchar(units))) {
    .stream_abort(
      sprintf("stream `%s` has partial channel units", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  channel_types <- .xdf_col_character(
    col_data, c("type", "stream_type"), n_channels
  )
  hardware <- .xdf_col_character(
    col_data, c("hardware_index"), n_channels
  )

  row_data <- SummarizedExperiment::rowData(pe)
  time_column <- if (identical(timestamps, "raw")) {
    "xdf_time_raw"
  } else {
    "xdf_time"
  }
  time_values <- if (time_column %in% names(row_data)) {
    as.numeric(row_data[[time_column]])
  } else if (!nrow(values)) {
    numeric()
  } else if (identical(timestamps, "output")) {
    rate <- PhysioExperiment::samplingRate(pe)
    offset <- if (key %in% names(clock$offsets)) clock$offsets[[key]] else 0
    if (length(rate) != 1L || !is.finite(rate) || rate <= 0 ||
        !is.finite(clock$t0) || !is.finite(offset)) {
      .stream_abort(
        sprintf("stream `%s` requires explicit output timestamps", key),
        "PhysioStream_xdf_validation_error"
      )
    }
    clock$t0 + offset + (seq_len(nrow(values)) - 1) / rate
  } else {
    .stream_abort(
      sprintf("stream `%s` has no complete raw timestamps", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  if (length(time_values) != nrow(values) ||
      any(!is.finite(time_values))) {
    .stream_abort(
      sprintf("stream `%s` timestamps are missing or invalid", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  rate <- metadata$xdf$nominal_srate
  if (is.null(rate)) {
    rate <- PhysioExperiment::samplingRate(pe)
    if (length(rate) != 1L || is.na(rate)) {
      rate <- 0
    }
  }
  if (!is.numeric(rate) || length(rate) != 1L || !is.finite(rate) ||
      rate < 0) {
    .stream_abort(
      sprintf("stream `%s` has invalid nominal rate", key),
      "PhysioStream_xdf_validation_error"
    )
  }
  stream_info <- metadata$stream$info
  info_name <- if (methods::is(stream_info, "StreamInfo")) {
    stream_info@name
  } else {
    NULL
  }
  info_type <- if (methods::is(stream_info, "StreamInfo")) {
    stream_info@type
  } else {
    NULL
  }
  info_source <- if (methods::is(stream_info, "StreamInfo")) {
    stream_info@source_id
  } else {
    NULL
  }
  name <- metadata$xdf$name
  if (is.null(name)) name <- info_name
  if (is.null(name)) name <- key
  type <- metadata$xdf$type
  if (is.null(type)) type <- info_type
  if (is.null(type)) type <- ""
  source_id <- metadata$xdf$source_id
  if (is.null(source_id)) source_id <- info_source
  if (is.null(source_id)) source_id <- ""
  .stream_scalar_string(name, "XDF stream name")
  .stream_scalar_string(type, "XDF stream type", allow_empty = TRUE)
  .stream_scalar_string(
    source_id, "XDF stream source_id", allow_empty = TRUE
  )
  list(
    key = key,
    output_id = output_id,
    values = values,
    timestamps = time_values,
    n_channels = n_channels,
    channel_names = channel_names,
    channel_units = units,
    channel_types = channel_types,
    hardware_index = hardware,
    nominal_srate = as.numeric(rate),
    format = format,
    name = name,
    type = type,
    source_id = source_id,
    metadata = metadata$xdf
  )
}

.xdf_output_path <- function(path, overwrite) {
  path <- .xdf_scalar_path(path)
  overwrite <- .lsl_scalar_logical(overwrite, "overwrite")
  parent_input <- dirname(path)
  if (!dir.exists(parent_input)) {
    .stream_abort(
      "XDF output parent directory must already exist",
      "PhysioStream_validation_error"
    )
  }
  parent_link <- Sys.readlink(parent_input)
  if (length(parent_link) == 1L && !is.na(parent_link) &&
      nzchar(parent_link)) {
    .stream_abort(
      "XDF output parent must not be a symbolic link",
      "PhysioStream_xdf_path_error"
    )
  }
  parent <- normalizePath(parent_input, winslash = "/", mustWork = TRUE)
  target <- file.path(parent, basename(path))
  target_link <- Sys.readlink(target)
  if (length(target_link) == 1L && !is.na(target_link) &&
      nzchar(target_link)) {
    .stream_abort(
      "XDF output target must not be a symbolic link",
      "PhysioStream_xdf_path_error"
    )
  }
  if (dir.exists(target)) {
    .stream_abort(
      "XDF output target is a directory",
      "PhysioStream_validation_error"
    )
  }
  existed <- file.exists(target)
  if (existed && !overwrite) {
    .stream_abort(
      "XDF output already exists; set `overwrite = TRUE` explicitly",
      "PhysioStream_xdf_path_error"
    )
  }
  list(
    path = target,
    overwrite = overwrite,
    existed = existed,
    initial_size = if (existed) unname(file.info(target)$size[[1L]]) else 0,
    initial_sha256 = if (existed) {
      digest::digest(file = target, algo = "sha256", serialize = FALSE)
    } else {
      ""
    }
  )
}

#' Write an Extensible Data Format file
#'
#' The writer implements the governed XDF 1.0 subset natively and does not
#' require Python. Output is scanned before an atomic same-directory rename.
#'
#' @param x One [PhysioExperiment::MultiPhysioExperiment] or
#'   [PhysioExperiment::PhysioExperiment].
#' @param path Destination `.xdf` path in an existing directory.
#' @param overwrite Whether an existing regular destination may be replaced.
#' @param chunk_samples Exact positive samples per XDF Samples chunk.
#' @param timestamps Exact timestamp domain, `"output"` or `"raw"`.
#' @return The normalized output path, invisibly.
#' @examples
#' \dontrun{
#' # Writing XDF requires the pyxdf backend and a multi-stream container,
#' # e.g. one returned by readXDF().
#' writeXDF(container, tempfile(fileext = ".xdf"))
#' }
#' @export
writeXDF <- function(x, path, overwrite = FALSE, chunk_samples = 256L,
                     timestamps = c("output", "raw")) {
  if (missing(timestamps)) {
    timestamps <- "output"
  }
  if (!is.character(timestamps) || length(timestamps) != 1L ||
      is.na(timestamps) || !(timestamps %in% c("output", "raw"))) {
    .stream_abort(
      "`timestamps` must be exactly 'output' or 'raw'",
      "PhysioStream_validation_error"
    )
  }
  chunk_samples <- .xdf_exact_number(
    chunk_samples, "chunk_samples", 1, .Machine$integer.max
  )
  output <- .xdf_output_path(path, overwrite)
  if (methods::is(x, "MultiPhysioExperiment")) {
    streams <- as.list(PhysioExperiment::streams(x))
    clock <- PhysioExperiment::commonClock(x)
  } else if (methods::is(x, "PhysioExperiment")) {
    streams <- list(stream = x)
    clock <- list(
      t0 = 0,
      reference_rate = PhysioExperiment::samplingRate(x),
      offsets = c(stream = 0)
    )
  } else {
    .stream_abort(
      "`x` must be a PhysioExperiment or MultiPhysioExperiment",
      "PhysioStream_validation_error"
    )
  }
  if (!length(streams) || is.null(names(streams)) ||
      any(!nzchar(names(streams))) || anyDuplicated(names(streams))) {
    .stream_abort(
      "XDF output requires uniquely named streams",
      "PhysioStream_xdf_validation_error"
    )
  }
  records <- lapply(seq_along(streams), function(i) {
    .xdf_export_record(
      streams[[i]], names(streams)[[i]], i, clock, timestamps
    )
  })
  bytes <- .xdf_assemble(records, as.integer(chunk_samples))
  temporary <- tempfile(
    pattern = paste0(".", basename(output$path), "."),
    tmpdir = dirname(output$path)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  .xdf_write_raw_file(temporary, bytes)
  scan <- .xdf_scan_file(temporary, .xdf_allocation_limit)
  if (length(scan$streams) != length(records) ||
      !identical(
        vapply(scan$streams, `[[`, numeric(1), "sample_count"),
        vapply(records, function(record) nrow(record$values), numeric(1))
      )) {
    .stream_abort(
      "post-write XDF structural validation failed",
      "PhysioStream_xdf_write_error"
    )
  }
  target_link <- Sys.readlink(output$path)
  target_is_link <- length(target_link) == 1L && !is.na(target_link) &&
    nzchar(target_link)
  current_exists <- file.exists(output$path)
  current_regular <- current_exists &&
    isTRUE(utils::file_test("-f", output$path))
  target_changed <- if (output$existed) {
    !current_regular ||
      unname(file.info(output$path)$size[[1L]]) != output$initial_size ||
      !identical(
        digest::digest(
          file = output$path, algo = "sha256", serialize = FALSE
        ),
        output$initial_sha256
      )
  } else {
    current_exists
  }
  parent_link <- Sys.readlink(dirname(output$path))
  parent_is_link <- length(parent_link) == 1L && !is.na(parent_link) &&
    nzchar(parent_link)
  if (target_is_link || parent_is_link || target_changed) {
    .stream_abort(
      "XDF destination changed before atomic landing",
      "PhysioStream_xdf_concurrent_change"
    )
  }
  if (!file.rename(temporary, output$path)) {
    .stream_abort(
      "failed to atomically land the XDF output",
      "PhysioStream_xdf_write_error"
    )
  }
  invisible(output$path)
}
