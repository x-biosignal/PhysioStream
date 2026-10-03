.lsl_mapping_schema <- "1.0.0"
.lsl_properties <- c("name", "type", "source_id", "uid", "hostname")
.lsl_formats <- c(
  float32 = "float32",
  double64 = "float64",
  int32 = "int32",
  int16 = "int16",
  int8 = "int8",
  string = "string"
)

.lsl_xml_one <- function(doc, path, required = FALSE) {
  nodes <- xml2::xml_find_all(doc, path)
  if (length(nodes) > 1L) {
    .stream_abort(
      sprintf("LSL descriptor has repeated field `%s`", path),
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (!length(nodes)) {
    if (required) {
      .stream_abort(
        sprintf("LSL descriptor is missing `%s`", path),
        "PhysioStream_lsl_metadata_error"
      )
    }
    return("")
  }
  xml2::xml_text(nodes[[1L]])
}

.lsl_parse_integer <- function(x, name, minimum = 0L) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value) || value != floor(value) ||
      value < minimum || value > .Machine$integer.max) {
    .stream_abort(
      sprintf("LSL descriptor `%s` is not an exact integer", name),
      "PhysioStream_lsl_metadata_error"
    )
  }
  as.integer(value)
}

.lsl_parse_number <- function(x, name, minimum = 0) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value) || value < minimum) {
    .stream_abort(
      sprintf("LSL descriptor `%s` is not a finite value >= %s",
              name, minimum),
      "PhysioStream_lsl_metadata_error"
    )
  }
  value
}

.lsl_canonical_xml <- function(xml) {
  if (!is.character(xml) || length(xml) != 1L || is.na(xml) || !nzchar(xml)) {
    .stream_abort(
      "LSL descriptor XML must be one non-empty string",
      "PhysioStream_lsl_metadata_error"
    )
  }
  doc <- tryCatch(
    xml2::read_xml(xml, options = c("NONET", "NOBLANKS")),
    error = function(e) {
      .stream_abort(
        paste0("invalid LSL descriptor XML: ", conditionMessage(e)),
        "PhysioStream_lsl_metadata_error"
      )
    }
  )
  if (!identical(xml2::xml_name(xml2::xml_root(doc)), "info")) {
    .stream_abort(
      "LSL descriptor root must be exactly `info`",
      "PhysioStream_lsl_metadata_error"
    )
  }
  list(doc = doc, text = as.character(doc))
}

.lsl_descriptor_record <- function(xml, library_version = "") {
  canonical <- .lsl_canonical_xml(xml)
  doc <- canonical$doc
  name <- .lsl_xml_one(doc, "/info/name", required = TRUE)
  type <- .lsl_xml_one(doc, "/info/type", required = TRUE)
  source_id <- .lsl_xml_one(doc, "/info/source_id")
  n_channels <- .lsl_parse_integer(
    .lsl_xml_one(doc, "/info/channel_count", required = TRUE),
    "channel_count", 1L
  )
  nominal_srate <- .lsl_parse_number(
    .lsl_xml_one(doc, "/info/nominal_srate", required = TRUE),
    "nominal_srate", 0
  )
  format <- .lsl_xml_one(doc, "/info/channel_format", required = TRUE)
  if (!(format %in% names(.lsl_formats))) {
    .stream_abort(
      sprintf("unsupported LSL channel format `%s`", format),
      "PhysioStream_lsl_metadata_error"
    )
  }
  dtype <- unname(.lsl_formats[[format]])
  if (identical(dtype, "string") && nominal_srate != 0) {
    .stream_abort(
      "string LSL streams are supported only at irregular rate zero",
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (!identical(dtype, "string") && nominal_srate <= 0) {
    .stream_abort(
      "numeric LSL streams require a positive nominal rate",
      "PhysioStream_lsl_metadata_error"
    )
  }

  channel_nodes <- xml2::xml_find_all(
    doc, "/info/desc/channels/channel"
  )
  if (length(channel_nodes) != n_channels) {
    .stream_abort(
      "LSL channel metadata count differs from channel_count",
      "PhysioStream_lsl_metadata_error"
    )
  }
  field <- function(node, field_name) {
    found <- xml2::xml_find_all(node, paste0("./", field_name))
    if (length(found) > 1L) {
      .stream_abort(
        sprintf("LSL channel repeats `%s`", field_name),
        "PhysioStream_lsl_metadata_error"
      )
    }
    if (!length(found)) "" else xml2::xml_text(found[[1L]])
  }
  channel_metadata <- lapply(channel_nodes, function(node) {
    list(
      label = field(node, "label"),
      unit = field(node, "unit"),
      type = field(node, "type"),
      hardware_index = field(node, "hardware_index")
    )
  })
  labels <- vapply(channel_metadata, `[[`, character(1), "label")
  units <- vapply(channel_metadata, `[[`, character(1), "unit")
  if (all(!nzchar(labels))) {
    labels <- sprintf("channel_%03d", seq_len(n_channels))
  } else if (any(!nzchar(labels))) {
    .stream_abort(
      "LSL channel labels must be all present or all absent",
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (anyDuplicated(labels)) {
    .stream_abort(
      "LSL channel labels must be unique",
      "PhysioStream_lsl_metadata_error"
    )
  }
  names(channel_metadata) <- labels
  channel_units <- if (all(!nzchar(units))) {
    NULL
  } else {
    if (any(!nzchar(units))) {
      .stream_abort(
        "LSL channel units must be all present or all absent",
        "PhysioStream_lsl_metadata_error"
      )
    }
    units
  }

  created_at_text <- .lsl_xml_one(doc, "/info/created_at")
  created_at <- if (nzchar(created_at_text)) {
    .lsl_parse_number(created_at_text, "created_at", 0)
  } else {
    0
  }
  list(
    name = name,
    type = type,
    channel_names = labels,
    channel_units = channel_units,
    nominal_srate = nominal_srate,
    dtype = dtype,
    source_id = source_id,
    lsl = list(
      uid = .lsl_xml_one(doc, "/info/uid"),
      hostname = .lsl_xml_one(doc, "/info/hostname"),
      session_id = .lsl_xml_one(doc, "/info/session_id"),
      created_at = created_at,
      protocol_version = .lsl_xml_one(doc, "/info/version"),
      library_version = as.character(library_version),
      manufacturer = .lsl_xml_one(doc, "/info/desc/manufacturer"),
      channel_metadata = channel_metadata,
      descriptor_sha256 = digest::digest(
        charToRaw(enc2utf8(canonical$text)),
        algo = "sha256", serialize = FALSE
      ),
      mapping_schema = .lsl_mapping_schema
    )
  )
}

.lsl_record_to_stream_info <- function(record) {
  streamInfo(
    name = record$name,
    type = record$type,
    channel_names = record$channel_names,
    nominal_srate = record$nominal_srate,
    dtype = record$dtype,
    source_id = record$source_id,
    clock_domain = "lsl",
    channel_units = record$channel_units,
    metadata = list(lsl = record$lsl)
  )
}

.lsl_info_to_stream_info <- function(info, adapter) {
  backend_info <- adapter$backend_info()
  record <- .lsl_descriptor_record(
    adapter$info_xml(info),
    library_version = backend_info$liblsl_version
  )
  .lsl_record_to_stream_info(record)
}

.lsl_outlet_metadata <- function(info) {
  metadata <- info@metadata$lsl
  if (is.null(metadata)) {
    return(NULL)
  }
  if (!is.list(metadata)) {
    .stream_abort(
      "`metadata$lsl` must be a named list",
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (length(metadata) &&
      (is.null(names(metadata)) || anyNA(names(metadata)) ||
       any(!nzchar(names(metadata))) || anyDuplicated(names(metadata)))) {
    .stream_abort(
      "`metadata$lsl` fields must have unique non-empty names",
      "PhysioStream_lsl_metadata_error"
    )
  }
  governed <- c(
    "uid", "hostname", "session_id", "created_at", "protocol_version",
    "library_version", "manufacturer", "channel_metadata",
    "descriptor_sha256", "mapping_schema"
  )
  unknown <- setdiff(names(metadata), governed)
  if (length(unknown)) {
    .stream_abort(
      sprintf("unrecognized governed LSL metadata field `%s`", unknown[[1L]]),
      "PhysioStream_lsl_metadata_error"
    )
  }
  for (field in c(
    "uid", "hostname", "session_id", "protocol_version", "library_version",
    "manufacturer"
  )) {
    value <- metadata[[field]]
    if (!is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))) {
      .stream_abort(
        sprintf("`metadata$lsl$%s` must be one non-missing string", field),
        "PhysioStream_lsl_metadata_error"
      )
    }
  }
  if (!is.null(metadata$created_at) &&
      (!is.numeric(metadata$created_at) ||
       length(metadata$created_at) != 1L ||
       !is.finite(metadata$created_at) ||
       metadata$created_at < 0)) {
    .stream_abort(
      "`metadata$lsl$created_at` must be one finite non-negative value",
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (!is.null(metadata$mapping_schema) &&
      !identical(metadata$mapping_schema, .lsl_mapping_schema)) {
    .stream_abort(
      "`metadata$lsl$mapping_schema` is unsupported",
      "PhysioStream_lsl_metadata_error"
    )
  }
  if (!is.null(metadata$descriptor_sha256) &&
      (!is.character(metadata$descriptor_sha256) ||
       length(metadata$descriptor_sha256) != 1L ||
       !grepl("^[0-9a-f]{64}$", metadata$descriptor_sha256))) {
    .stream_abort(
      "`metadata$lsl$descriptor_sha256` must be a lowercase SHA-256",
      "PhysioStream_lsl_metadata_error"
    )
  }

  channels <- metadata$channel_metadata
  if (is.null(channels)) {
    return(metadata)
  }
  if (!is.list(channels) || length(channels) != info@n_channels ||
      !identical(names(channels), info@channel_names)) {
    .stream_abort(
      "`metadata$lsl$channel_metadata` must match channel order and names",
      "PhysioStream_lsl_metadata_error"
    )
  }
  expected_units <- if (is.null(info@channel_units)) {
    rep("", info@n_channels)
  } else {
    info@channel_units
  }
  for (i in seq_len(info@n_channels)) {
    channel <- channels[[i]]
    if (!is.list(channel)) {
      .stream_abort(
        "each LSL channel metadata entry must be a named list",
        "PhysioStream_lsl_metadata_error"
      )
    }
    if (length(channel) &&
        (is.null(names(channel)) || anyNA(names(channel)) ||
         any(!nzchar(names(channel))) || anyDuplicated(names(channel)))) {
      .stream_abort(
        "LSL channel metadata fields must have unique non-empty names",
        "PhysioStream_lsl_metadata_error"
      )
    }
    unknown <- setdiff(
      names(channel), c("label", "unit", "type", "hardware_index")
    )
    if (length(unknown)) {
      .stream_abort(
        sprintf("unrecognized LSL channel metadata field `%s`", unknown[[1L]]),
        "PhysioStream_lsl_metadata_error"
      )
    }
    for (field in names(channel)) {
      value <- channel[[field]]
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        .stream_abort(
          sprintf("LSL channel metadata `%s` must be one non-missing string",
                  field),
          "PhysioStream_lsl_metadata_error"
        )
      }
    }
    if (!is.null(channel$label) &&
        !identical(channel$label, info@channel_names[[i]])) {
      .stream_abort(
        "LSL channel metadata labels differ from StreamInfo",
        "PhysioStream_lsl_metadata_error"
      )
    }
    if (!is.null(channel$unit) &&
        !identical(channel$unit, expected_units[[i]])) {
      .stream_abort(
        "LSL channel metadata units differ from StreamInfo",
        "PhysioStream_lsl_metadata_error"
      )
    }
  }
  metadata
}

.lsl_append_descriptor <- function(py_info, info, adapter,
                                   lsl_metadata = NULL) {
  desc <- py_info$desc()
  channels <- adapter$append_child(desc, "channels")
  units <- info@channel_units
  channel_metadata <- if (is.list(lsl_metadata)) {
    lsl_metadata$channel_metadata
  } else {
    NULL
  }
  for (i in seq_len(info@n_channels)) {
    channel <- adapter$append_child(channels, "channel")
    adapter$append_child_value(channel, "label", info@channel_names[[i]])
    if (!is.null(units)) {
      adapter$append_child_value(channel, "unit", units[[i]])
    }
    if (is.list(channel_metadata) && length(channel_metadata) == info@n_channels) {
      for (field in c("type", "hardware_index")) {
        value <- channel_metadata[[i]][[field]]
        if (is.character(value) && length(value) == 1L && nzchar(value)) {
          adapter$append_child_value(channel, field, value)
        }
      }
    }
  }
  if (is.list(lsl_metadata)) {
    manufacturer <- lsl_metadata$manufacturer
    if (is.character(manufacturer) && length(manufacturer) == 1L &&
        nzchar(manufacturer)) {
      adapter$append_child_value(desc, "manufacturer", manufacturer)
    }
  }
  py_info
}

.stream_info_to_lsl_info <- function(info, adapter) {
  if (!methods::is(info, "StreamInfo")) {
    .stream_abort(
      "`info` must be a StreamInfo",
      "PhysioStream_validation_error"
    )
  }
  methods::validObject(info)
  if (identical(info@dtype, "string") && info@nominal_srate != 0) {
    .stream_abort(
      "string LSL outlets require nominal_srate = 0",
      "PhysioStream_validation_error"
    )
  }
  if (!identical(info@dtype, "string") && info@nominal_srate <= 0) {
    .stream_abort(
      "numeric LSL outlets require a positive nominal rate",
      "PhysioStream_validation_error"
    )
  }
  lsl_metadata <- .lsl_outlet_metadata(info)
  py_info <- adapter$make_info(info)
  .lsl_append_descriptor(py_info, info, adapter, lsl_metadata)
}

.lsl_info_public_equal <- function(observed, expected) {
  identical(observed@name, expected@name) &&
    identical(observed@type, expected@type) &&
    identical(observed@n_channels, expected@n_channels) &&
    identical(observed@channel_names, expected@channel_names) &&
    identical(observed@channel_units, expected@channel_units) &&
    identical(observed@nominal_srate, expected@nominal_srate) &&
    identical(observed@dtype, expected@dtype) &&
    identical(observed@source_id, expected@source_id)
}

#' Resolve visible Lab Streaming Layer streams
#'
#' pylsl resolution returns short descriptors. PhysioStream therefore opens a
#' bounded, non-recovering metadata inlet for each result, retrieves its full
#' descriptor, and closes the temporary inlet before returning. No samples are
#' pulled.
#'
#' @param property,value Either both `NULL`, or an exact core LSL property and
#'   one non-empty value.
#' @param minimum Exact minimum number of descriptors required.
#' @param timeout Finite non-negative resolver timeout in seconds.
#' @inheritParams lslAvailable
#' @return A list of validated `StreamInfo` objects in backend order.
#' @examples
#' \donttest{
#' # Requires a running LSL network with at least one stream.
#' if (lslAvailable()) {
#'   streams <- lslResolveStreams(timeout = 0.2)
#' }
#' }
#' @export
lslResolveStreams <- function(property = NULL, value = NULL, minimum = 0L,
                              timeout = 1,
                              backend = getOption(
                                "PhysioStream.lsl_backend", "auto"
                              )) {
  backend <- .lsl_backend_arg(backend)
  minimum <- .ring_exact_integer(minimum, "minimum", 0)
  timeout <- .lsl_scalar_timeout(timeout)
  both_null <- is.null(property) && is.null(value)
  both_value <- is.character(property) && length(property) == 1L &&
    !is.na(property) && property %in% .lsl_properties &&
    is.character(value) && length(value) == 1L && !is.na(value) &&
    nzchar(value)
  if (!both_null && !both_value) {
    .stream_abort(
      "`property` and `value` must both be NULL or an exact property and non-empty value",
      "PhysioStream_validation_error"
    )
  }
  adapter <- .lsl_import_adapter(backend)
  descriptors <- if (both_null) {
    adapter$resolve_all(timeout)
  } else {
    adapter$resolve_byprop(property, value, minimum, timeout)
  }
  mapped <- lapply(descriptors, function(descriptor) {
    full <- adapter$full_info(descriptor, timeout)
    .lsl_info_to_stream_info(full, adapter)
  })
  if (length(mapped) < minimum) {
    .stream_abort(
      sprintf("LSL resolution returned %d stream(s); minimum is %d",
              length(mapped), minimum),
      "PhysioStream_lsl_resolution_error"
    )
  }
  mapped
}
