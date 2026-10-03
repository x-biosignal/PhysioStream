.xdf_wire_formats <- c(
  float32 = 4L,
  double64 = 8L,
  int64 = 8L,
  int32 = 4L,
  int16 = 2L,
  int8 = 1L,
  string = NA_integer_
)
.xdf_allocation_limit <- 512 * 1024^2
.xdf_xml_limit <- 8 * 1024^2
.xdf_boundary <- as.raw(c(
  0x43, 0xa5, 0x46, 0xdc, 0xcb, 0xf5, 0x41, 0x0f,
  0xb3, 0x0e, 0xd5, 0x46, 0x73, 0x83, 0xcb, 0xe4
))

.xdf_exact_number <- function(x, name, minimum = 0, maximum = 2^53 - 1) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x != floor(x) || x < minimum || x > maximum) {
    .stream_abort(
      sprintf(
        "`%s` must be one exact integer between %s and %s",
        name, format(minimum, scientific = FALSE),
        format(maximum, scientific = FALSE)
      ),
      "PhysioStream_validation_error"
    )
  }
  as.numeric(x)
}

.xdf_id_text <- function(x) {
  format(x, scientific = FALSE, trim = TRUE)
}

.xdf_raw_uint <- function(value, width) {
  value <- .xdf_exact_number(
    value, "wire integer", 0, if (width == 8L) 2^53 - 1 else 2^(8 * width) - 1
  )
  out <- raw(width)
  remaining <- value
  for (i in seq_len(width)) {
    byte <- remaining %% 256
    out[[i]] <- as.raw(byte)
    remaining <- floor(remaining / 256)
  }
  out
}

.xdf_read_uint <- function(bytes, pos, width, limit = length(bytes),
                           context = "integer") {
  if (pos < 1 || width < 1L || pos + width - 1 > limit) {
    .stream_abort(
      sprintf("truncated XDF %s", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  values <- as.integer(bytes[pos:(pos + width - 1L)])
  value <- sum(values * 256^(seq_len(width) - 1L))
  if (!is.finite(value) || value > 2^53 - 1) {
    .stream_abort(
      sprintf("XDF %s exceeds exact R numeric range", context),
      "PhysioStream_xdf_resource_error"
    )
  }
  as.numeric(value)
}

.xdf_varint_raw <- function(value) {
  value <- .xdf_exact_number(value, "variable integer")
  if (value < 256) {
    return(c(as.raw(1L), .xdf_raw_uint(value, 1L)))
  }
  if (value <= 2^32 - 1) {
    return(c(as.raw(4L), .xdf_raw_uint(value, 4L)))
  }
  c(as.raw(8L), .xdf_raw_uint(value, 8L))
}

.xdf_read_varint <- function(bytes, pos, limit = length(bytes),
                             context = "variable integer") {
  if (pos < 1L || pos > limit) {
    .stream_abort(
      sprintf("truncated XDF %s", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  width <- as.integer(bytes[[pos]])
  if (!(width %in% c(1L, 4L, 8L))) {
    .stream_abort(
      sprintf("invalid XDF %s width `%s`", context, width),
      "PhysioStream_xdf_parse_error"
    )
  }
  value <- .xdf_read_uint(
    bytes, pos + 1L, width, limit, context
  )
  list(value = value, next_pos = pos + 1L + width, width = width)
}

.xdf_double_raw <- function(value) {
  con <- rawConnection(raw(), "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.double(value), con, size = 8L, endian = "little")
  rawConnectionValue(con)
}

.xdf_float_raw <- function(value, size) {
  con <- rawConnection(raw(), "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.double(value), con, size = size, endian = "little")
  rawConnectionValue(con)
}

.xdf_signed_raw <- function(value, size) {
  bits <- 8 * size
  unsigned <- if (value < 0) value + 2^bits else value
  .xdf_raw_uint(unsigned, size)
}

.xdf_read_double <- function(bytes, pos, limit = length(bytes),
                             context = "double") {
  if (pos < 1L || pos + 7L > limit) {
    .stream_abort(
      sprintf("truncated XDF %s", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  con <- rawConnection(bytes[pos:(pos + 7L)], "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, "double", n = 1L, size = 8L, endian = "little")
}

.xdf_xml_record <- function(xml, context = "XML") {
  if (!is.character(xml) || length(xml) != 1L || is.na(xml) || !nzchar(xml)) {
    .stream_abort(
      sprintf("XDF %s must be one non-empty string", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  xml <- enc2utf8(xml)
  if (nchar(xml, type = "bytes") > .xdf_xml_limit) {
    .stream_abort(
      sprintf("XDF %s exceeds the 8 MiB XML ceiling", context),
      "PhysioStream_xdf_resource_error"
    )
  }
  if (grepl("<!DOCTYPE|<!ENTITY", xml, ignore.case = TRUE, perl = TRUE)) {
    .stream_abort(
      sprintf("XDF %s must not contain a DTD or entity declaration", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  doc <- tryCatch(
    xml2::read_xml(xml, options = c("NONET", "NOBLANKS")),
    error = function(e) {
      .stream_abort(
        paste0("invalid XDF ", context, ": ", conditionMessage(e)),
        "PhysioStream_xdf_parse_error"
      )
    }
  )
  if (!identical(xml2::xml_name(xml2::xml_root(doc)), "info")) {
    .stream_abort(
      sprintf("XDF %s root must be exactly `info`", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  canonical <- as.character(doc)
  list(
    doc = doc,
    text = canonical,
    sha256 = digest::digest(
      charToRaw(enc2utf8(canonical)), algo = "sha256", serialize = FALSE
    )
  )
}

.xdf_xml_one <- function(doc, path, required = FALSE,
                         context = "stream header") {
  nodes <- xml2::xml_find_all(doc, path)
  if (length(nodes) > 1L) {
    .stream_abort(
      sprintf("XDF %s repeats `%s`", context, path),
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (!length(nodes)) {
    if (required) {
      .stream_abort(
        sprintf("XDF %s is missing `%s`", context, path),
        "PhysioStream_xdf_metadata_error"
      )
    }
    return("")
  }
  xml2::xml_text(nodes[[1L]])
}

.xdf_header_number <- function(x, name, integer = FALSE, minimum = 0,
                               maximum = 2^53 - 1) {
  value <- suppressWarnings(as.numeric(x))
  invalid <- length(value) != 1L || !is.finite(value) ||
    value < minimum || value > maximum
  if (integer) {
    invalid <- invalid || value != floor(value)
  }
  if (invalid) {
    .stream_abort(
      sprintf("XDF stream header `%s` is invalid", name),
      "PhysioStream_xdf_metadata_error"
    )
  }
  value
}

.xdf_channel_field <- function(node, name) {
  found <- xml2::xml_find_all(node, paste0("./", name))
  if (length(found) > 1L) {
    .stream_abort(
      sprintf("XDF channel metadata repeats `%s`", name),
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (!length(found)) "" else xml2::xml_text(found[[1L]])
}

.xdf_parse_stream_header <- function(record, stream_id) {
  doc <- record$doc
  channel_count <- .xdf_header_number(
    .xdf_xml_one(doc, "/info/channel_count", TRUE),
    "channel_count", integer = TRUE, minimum = 1,
    maximum = .Machine$integer.max
  )
  nominal_srate <- .xdf_header_number(
    .xdf_xml_one(doc, "/info/nominal_srate", TRUE),
    "nominal_srate", minimum = 0
  )
  channel_format <- .xdf_xml_one(
    doc, "/info/channel_format", TRUE
  )
  if (!(channel_format %in% names(.xdf_wire_formats))) {
    .stream_abort(
      sprintf("unsupported XDF channel format `%s`", channel_format),
      "PhysioStream_xdf_unsupported_format"
    )
  }
  if (identical(channel_format, "int64")) {
    .stream_abort(
      sprintf(
        "XDF stream `%s` uses unsupported format `int64`",
        .xdf_id_text(stream_id)
      ),
      "PhysioStream_xdf_unsupported_format"
    )
  }

  channel_tree <- xml2::xml_find_all(doc, "/info/desc/channels")
  if (length(channel_tree) > 1L) {
    .stream_abort(
      "XDF stream header repeats the channel tree",
      "PhysioStream_xdf_metadata_error"
    )
  }
  nodes <- xml2::xml_find_all(doc, "/info/desc/channels/channel")
  synthesized <- !length(channel_tree)
  if (!synthesized && length(nodes) != channel_count) {
    .stream_abort(
      "XDF channel metadata count differs from channel_count",
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (synthesized) {
    labels <- sprintf("channel_%03d", seq_len(channel_count))
    units <- types <- hardware <- rep("", channel_count)
  } else {
    labels <- vapply(nodes, .xdf_channel_field, character(1), "label")
    units <- vapply(nodes, .xdf_channel_field, character(1), "unit")
    types <- vapply(nodes, .xdf_channel_field, character(1), "type")
    hardware <- vapply(
      nodes, .xdf_channel_field, character(1), "hardware_index"
    )
    if (any(!nzchar(labels)) || anyDuplicated(labels)) {
      .stream_abort(
        "XDF channel labels must be unique non-empty strings",
        "PhysioStream_xdf_metadata_error"
      )
    }
    if (any(nzchar(units)) && any(!nzchar(units))) {
      .stream_abort(
        "XDF channel units must be all present or all absent",
        "PhysioStream_xdf_metadata_error"
      )
    }
  }

  created_text <- .xdf_xml_one(doc, "/info/created_at")
  created_at <- if (nzchar(created_text)) {
    .xdf_header_number(created_text, "created_at", minimum = 0)
  } else {
    0
  }
  name <- .xdf_xml_one(doc, "/info/name", TRUE)
  if (!nzchar(name)) {
    .stream_abort(
      "XDF stream header `name` must be non-empty",
      "PhysioStream_xdf_metadata_error"
    )
  }
  list(
    stream_id = stream_id,
    name = name,
    type = .xdf_xml_one(doc, "/info/type"),
    source_id = .xdf_xml_one(doc, "/info/source_id"),
    uid = .xdf_xml_one(doc, "/info/uid"),
    hostname = .xdf_xml_one(doc, "/info/hostname"),
    session_id = .xdf_xml_one(doc, "/info/session_id"),
    created_at = created_at,
    channel_count = as.integer(channel_count),
    nominal_srate = nominal_srate,
    channel_format = channel_format,
    channel_names = labels,
    channel_units = if (all(!nzchar(units))) NULL else units,
    channel_types = types,
    hardware_index = hardware,
    channel_metadata_synthesized = synthesized
  )
}

.xdf_raw_text <- function(bytes, start, end, context) {
  if (end < start) {
    .stream_abort(
      sprintf("empty XDF %s", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  value <- tryCatch(
    rawToChar(bytes[start:end]),
    error = function(e) {
      .stream_abort(
        sprintf("invalid byte sequence in XDF %s", context),
        "PhysioStream_xdf_parse_error"
      )
    }
  )
  if (is.na(iconv(value, from = "UTF-8", to = "UTF-8", sub = NA))) {
    .stream_abort(
      sprintf("XDF %s is not valid UTF-8", context),
      "PhysioStream_xdf_parse_error"
    )
  }
  enc2utf8(value)
}

.xdf_find_stream <- function(streams, stream_id) {
  if (!length(streams)) {
    return(NA_integer_)
  }
  match(.xdf_id_text(stream_id), vapply(
    streams, function(x) .xdf_id_text(x$id), character(1)
  ))
}

.xdf_scan_samples <- function(bytes, pos, end, stream) {
  count_record <- .xdf_read_varint(
    bytes, pos, end, "sample count"
  )
  count <- count_record$value
  if (count > .Machine$integer.max ||
      stream$sample_count + count > .Machine$integer.max) {
    .stream_abort(
      "XDF sample count exceeds the governed matrix dimension ceiling",
      "PhysioStream_xdf_resource_error"
    )
  }
  pos <- count_record$next_pos
  width <- .xdf_wire_formats[[stream$header$channel_format]]
  for (sample_index in seq_len(count)) {
    if (pos > end) {
      .stream_abort(
        "truncated XDF sample timestamp marker",
        "PhysioStream_xdf_parse_error"
      )
    }
    marker <- as.integer(bytes[[pos]])
    pos <- pos + 1L
    if (marker == 8L) {
      timestamp <- .xdf_read_double(bytes, pos, end, "sample timestamp")
      if (!is.finite(timestamp)) {
        .stream_abort(
          "XDF sample timestamp must be finite",
          "PhysioStream_xdf_parse_error"
        )
      }
      pos <- pos + 8L
    } else if (marker != 0L) {
      .stream_abort(
        "XDF timestamp marker must be zero or eight",
        "PhysioStream_xdf_parse_error"
      )
    }
    if (identical(stream$header$channel_format, "string")) {
      for (channel_index in seq_len(stream$header$channel_count)) {
        string_size <- .xdf_read_varint(
          bytes, pos, end, "string byte count"
        )
        pos <- string_size$next_pos
        if (string_size$value > .xdf_allocation_limit ||
            pos + string_size$value - 1 > end) {
          .stream_abort(
            "truncated or oversized XDF string sample",
            "PhysioStream_xdf_parse_error"
          )
        }
        if (string_size$value) {
          .xdf_raw_text(
            bytes, pos, pos + string_size$value - 1L, "string sample"
          )
          pos <- pos + string_size$value
        }
      }
    } else {
      sample_bytes <- stream$header$channel_count * width
      if (pos + sample_bytes - 1 > end) {
        .stream_abort(
          "truncated XDF numeric sample",
          "PhysioStream_xdf_parse_error"
        )
      }
      pos <- pos + sample_bytes
    }
  }
  if (pos != end + 1L) {
    .stream_abort(
      "XDF Samples chunk length does not match its payload",
      "PhysioStream_xdf_parse_error"
    )
  }
  count
}

.xdf_scan_bytes <- function(bytes) {
  if (!is.raw(bytes) || length(bytes) < 4L ||
      !identical(bytes[1:4], charToRaw("XDF:"))) {
    .stream_abort(
      "XDF file has invalid or truncated magic",
      "PhysioStream_xdf_parse_error"
    )
  }
  pos <- 5L
  file_header <- NULL
  streams <- list()
  chunk_count <- 0L
  while (pos <= length(bytes)) {
    length_record <- .xdf_read_varint(
      bytes, pos, length(bytes), "chunk length"
    )
    chunk_length <- length_record$value
    if (chunk_length < 2 || chunk_length > .xdf_allocation_limit) {
      .stream_abort(
        "XDF chunk length is invalid or exceeds the allocation ceiling",
        "PhysioStream_xdf_resource_error"
      )
    }
    start <- length_record$next_pos
    end <- start + chunk_length - 1
    if (end > length(bytes)) {
      .stream_abort(
        "truncated XDF chunk",
        "PhysioStream_xdf_parse_error"
      )
    }
    tag <- .xdf_read_uint(bytes, start, 2L, end, "chunk tag")
    payload <- start + 2L
    chunk_count <- chunk_count + 1L

    if (tag == 1) {
      if (!is.null(file_header) || length(streams)) {
        .stream_abort(
          "XDF FileHeader must occur exactly once before streams",
          "PhysioStream_xdf_parse_error"
        )
      }
      file_header <- .xdf_xml_record(
        .xdf_raw_text(bytes, payload, end, "FileHeader"),
        "FileHeader"
      )
      version <- .xdf_xml_one(
        file_header$doc, "/info/version", TRUE, "FileHeader"
      )
      if (!identical(version, "1.0")) {
        .stream_abort(
          sprintf("unsupported XDF file version `%s`", version),
          "PhysioStream_xdf_unsupported_format"
        )
      }
    } else if (tag %in% c(2, 3, 4, 6)) {
      if (payload + 3L > end) {
        .stream_abort(
          "truncated XDF stream chunk id",
          "PhysioStream_xdf_parse_error"
        )
      }
      stream_id <- .xdf_read_uint(
        bytes, payload, 4L, end, "stream id"
      )
      data_pos <- payload + 4L
      index <- .xdf_find_stream(streams, stream_id)
      if (tag == 2) {
        if (!is.na(index)) {
          .stream_abort(
            sprintf("duplicate XDF stream id `%s`", .xdf_id_text(stream_id)),
            "PhysioStream_xdf_parse_error"
          )
        }
        header_record <- .xdf_xml_record(
          .xdf_raw_text(bytes, data_pos, end, "StreamHeader"),
          "StreamHeader"
        )
        streams[[length(streams) + 1L]] <- list(
          id = stream_id,
          header_xml = header_record$text,
          header_sha256 = header_record$sha256,
          header = .xdf_parse_stream_header(header_record, stream_id),
          footer_xml = "",
          footer_sha256 = "",
          clock_times = numeric(),
          clock_values = numeric(),
          sample_count = 0
        )
      } else {
        if (is.na(index)) {
          .stream_abort(
            sprintf(
              "XDF tag %s refers to unknown stream id `%s`",
              tag, .xdf_id_text(stream_id)
            ),
            "PhysioStream_xdf_parse_error"
          )
        }
        if (tag == 3) {
          count <- .xdf_scan_samples(
            bytes, data_pos, end, streams[[index]]
          )
          streams[[index]]$sample_count <-
            streams[[index]]$sample_count + count
        } else if (tag == 4) {
          if (end - data_pos + 1L != 16L) {
            .stream_abort(
              "XDF ClockOffset chunk must contain exactly two doubles",
              "PhysioStream_xdf_parse_error"
            )
          }
          clock_time <- .xdf_read_double(
            bytes, data_pos, end, "clock collection time"
          )
          clock_value <- .xdf_read_double(
            bytes, data_pos + 8L, end, "clock offset"
          )
          if (!all(is.finite(c(clock_time, clock_value)))) {
            .stream_abort(
              "XDF clock observations must be finite",
              "PhysioStream_xdf_parse_error"
            )
          }
          streams[[index]]$clock_times <- c(
            streams[[index]]$clock_times, clock_time
          )
          streams[[index]]$clock_values <- c(
            streams[[index]]$clock_values, clock_value
          )
        } else {
          if (nzchar(streams[[index]]$footer_xml)) {
            .stream_abort(
              sprintf(
                "duplicate XDF StreamFooter for id `%s`",
                .xdf_id_text(stream_id)
              ),
              "PhysioStream_xdf_parse_error"
            )
          }
          footer_record <- .xdf_xml_record(
            .xdf_raw_text(bytes, data_pos, end, "StreamFooter"),
            "StreamFooter"
          )
          streams[[index]]$footer_xml <- footer_record$text
          streams[[index]]$footer_sha256 <- footer_record$sha256
        }
      }
    } else if (tag == 5) {
      if (end - payload + 1L != length(.xdf_boundary) ||
          !identical(bytes[payload:end], .xdf_boundary)) {
        .stream_abort(
          "invalid XDF boundary chunk",
          "PhysioStream_xdf_parse_error"
        )
      }
    } else {
      .stream_abort(
        sprintf("unsupported XDF chunk tag `%s`", tag),
        "PhysioStream_xdf_unsupported_format"
      )
    }
    pos <- end + 1L
  }
  if (is.null(file_header)) {
    .stream_abort(
      "XDF file is missing FileHeader",
      "PhysioStream_xdf_parse_error"
    )
  }
  if (!length(streams)) {
    .stream_abort(
      "XDF file contains no StreamHeader",
      "PhysioStream_xdf_parse_error"
    )
  }
  list(
    file_header_xml = file_header$text,
    file_header_sha256 = file_header$sha256,
    streams = streams,
    chunk_count = chunk_count
  )
}

.xdf_scan_file <- function(path, max_bytes = .xdf_allocation_limit) {
  size <- unname(file.info(path)$size)
  if (!is.finite(size) || size > max_bytes || size > .xdf_allocation_limit) {
    .stream_abort(
      "XDF file exceeds the configured or internal allocation ceiling",
      "PhysioStream_xdf_resource_error"
    )
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, "raw", n = size)
  if (length(bytes) != size) {
    .stream_abort(
      "XDF file changed or was truncated while reading",
      "PhysioStream_xdf_parse_error"
    )
  }
  .xdf_scan_bytes(bytes)
}

.xdf_chunk <- function(tag, payload) {
  payload <- c(.xdf_raw_uint(tag, 2L), payload)
  c(.xdf_varint_raw(length(payload)), payload)
}

.xdf_xml_set <- function(root, name, value, omit_empty = FALSE) {
  nodes <- xml2::xml_find_all(root, paste0("./", name))
  if (length(nodes) > 1L) {
    .stream_abort(
      sprintf("preserved XDF XML repeats `%s`", name),
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (omit_empty && !nzchar(value)) {
    if (length(nodes)) {
      xml2::xml_remove(nodes[[1L]])
    }
    return(invisible(root))
  }
  node <- if (length(nodes)) nodes[[1L]] else xml2::xml_add_child(root, name)
  xml2::xml_set_text(node, enc2utf8(as.character(value)))
  invisible(root)
}

.xdf_decimal <- function(value) {
  format(value, digits = 17L, scientific = FALSE, trim = TRUE)
}

.xdf_build_header <- function(record) {
  preserved <- record$metadata$stream_header_xml
  doc <- if (is.character(preserved) && length(preserved) == 1L &&
             !is.na(preserved) && nzchar(preserved)) {
    .xdf_xml_record(preserved, "preserved StreamHeader")$doc
  } else {
    xml2::xml_new_root("info")
  }
  root <- xml2::xml_root(doc)
  .xdf_xml_set(root, "name", record$name)
  .xdf_xml_set(root, "type", record$type, omit_empty = TRUE)
  .xdf_xml_set(root, "channel_count", as.character(record$n_channels))
  .xdf_xml_set(root, "nominal_srate", .xdf_decimal(record$nominal_srate))
  .xdf_xml_set(root, "channel_format", record$format)
  .xdf_xml_set(root, "source_id", record$source_id, omit_empty = TRUE)

  desc <- xml2::xml_find_all(root, "./desc")
  if (length(desc) > 1L) {
    .stream_abort(
      "preserved XDF header repeats `desc`",
      "PhysioStream_xdf_metadata_error"
    )
  }
  desc <- if (length(desc)) desc[[1L]] else xml2::xml_add_child(root, "desc")
  old_channels <- xml2::xml_find_all(desc, "./channels")
  if (length(old_channels) > 1L) {
    .stream_abort(
      "preserved XDF header repeats `desc/channels`",
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (length(old_channels)) {
    xml2::xml_remove(old_channels[[1L]])
  }
  channels <- xml2::xml_add_child(desc, "channels")
  for (i in seq_len(record$n_channels)) {
    channel <- xml2::xml_add_child(channels, "channel")
    xml2::xml_add_child(channel, "label", record$channel_names[[i]])
    if (!is.null(record$channel_units)) {
      xml2::xml_add_child(channel, "unit", record$channel_units[[i]])
    }
    if (nzchar(record$channel_types[[i]])) {
      xml2::xml_add_child(channel, "type", record$channel_types[[i]])
    }
    if (nzchar(record$hardware_index[[i]])) {
      xml2::xml_add_child(
        channel, "hardware_index", record$hardware_index[[i]]
      )
    }
  }
  .xdf_xml_record(as.character(doc), "output StreamHeader")$text
}

.xdf_build_footer <- function(record) {
  preserved <- record$metadata$stream_footer_xml
  doc <- if (is.character(preserved) && length(preserved) == 1L &&
             !is.na(preserved) && nzchar(preserved)) {
    .xdf_xml_record(preserved, "preserved StreamFooter")$doc
  } else {
    xml2::xml_new_root("info")
  }
  root <- xml2::xml_root(doc)
  timestamps <- record$timestamps
  first <- if (length(timestamps)) timestamps[[1L]] else 0
  last <- if (length(timestamps)) timestamps[[length(timestamps)]] else 0
  .xdf_xml_set(root, "first_timestamp", .xdf_decimal(first))
  .xdf_xml_set(root, "last_timestamp", .xdf_decimal(last))
  .xdf_xml_set(root, "sample_count", as.character(nrow(record$values)))
  old_offsets <- xml2::xml_find_all(root, "./clock_offsets")
  if (length(old_offsets) > 1L) {
    .stream_abort(
      "preserved XDF footer repeats `clock_offsets`",
      "PhysioStream_xdf_metadata_error"
    )
  }
  if (length(old_offsets)) {
    xml2::xml_remove(old_offsets[[1L]])
  }
  .xdf_xml_record(as.character(doc), "output StreamFooter")$text
}

.xdf_value_bytes <- function(value, format) {
  if (identical(format, "string")) {
    encoded <- charToRaw(enc2utf8(value))
    return(c(.xdf_varint_raw(length(encoded)), encoded))
  }
  if (identical(format, "float32")) {
    raw <- .xdf_float_raw(value, 4L)
    con <- rawConnection(raw, "rb")
    on.exit(close(con), add = TRUE)
    roundtrip <- readBin(con, "double", n = 1L, size = 4L, endian = "little")
    if (!identical(as.double(value), roundtrip)) {
      .stream_abort(
        "float32 XDF values must be exactly representable",
        "PhysioStream_xdf_validation_error"
      )
    }
    return(raw)
  }
  if (identical(format, "double64")) {
    return(.xdf_float_raw(value, 8L))
  }
  bounds <- list(
    int8 = c(-2^7, 2^7 - 1),
    int16 = c(-2^15, 2^15 - 1),
    int32 = c(-2^31, 2^31 - 1)
  )[[format]]
  if (is.null(bounds) || value != floor(value) ||
      value < bounds[[1L]] || value > bounds[[2L]]) {
    .stream_abort(
      sprintf("value is not exactly representable as XDF %s", format),
      "PhysioStream_xdf_validation_error"
    )
  }
  .xdf_signed_raw(value, .xdf_wire_formats[[format]])
}

.xdf_sample_chunk <- function(record, from, to) {
  payload <- c(
    .xdf_raw_uint(record$output_id, 4L),
    .xdf_varint_raw(to - from + 1L)
  )
  for (i in seq.int(from, to)) {
    payload <- c(payload, as.raw(8L), .xdf_double_raw(record$timestamps[[i]]))
    for (j in seq_len(record$n_channels)) {
      payload <- c(
        payload,
        .xdf_value_bytes(record$values[i, j], record$format)
      )
    }
  }
  .xdf_chunk(3L, payload)
}

.xdf_write_raw_file <- function(path, bytes) {
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  flush(con)
  invisible(path)
}

.xdf_assemble <- function(records, chunk_samples) {
  estimated_bytes <- 128
  for (record in records) {
    value_bytes <- if (identical(record$format, "string")) {
      sum(nchar(enc2utf8(record$values), type = "bytes")) +
        length(record$values) * 9
    } else {
      length(record$values) * .xdf_wire_formats[[record$format]]
    }
    preserved_header <- record$metadata$stream_header_xml
    preserved_footer <- record$metadata$stream_footer_xml
    xml_bytes <- sum(nchar(
      enc2utf8(c(
        if (is.character(preserved_header)) preserved_header else "",
        if (is.character(preserved_footer)) preserved_footer else "",
        record$name, record$type, record$source_id,
        record$channel_names, record$channel_units,
        record$channel_types, record$hardware_index
      )),
      type = "bytes"
    )) + 4096
    estimated_bytes <- estimated_bytes + value_bytes +
      nrow(record$values) * 10 + xml_bytes
    if (!is.finite(estimated_bytes) ||
        estimated_bytes > .xdf_allocation_limit) {
      .stream_abort(
        "estimated XDF output exceeds the internal allocation ceiling",
        "PhysioStream_xdf_resource_error"
      )
    }
  }
  chunks <- list(
    charToRaw("XDF:"),
    .xdf_chunk(
      1L, charToRaw("<?xml version=\"1.0\"?><info><version>1.0</version></info>")
    )
  )
  for (record in records) {
    chunks[[length(chunks) + 1L]] <- .xdf_chunk(
      2L,
      c(
        .xdf_raw_uint(record$output_id, 4L),
        charToRaw(enc2utf8(.xdf_build_header(record)))
      )
    )
  }
  sample_chunks <- list()
  for (record in records) {
    n <- nrow(record$values)
    if (!n) {
      next
    }
    starts <- seq.int(1L, n, by = chunk_samples)
    for (from in starts) {
      to <- min(n, from + chunk_samples - 1L)
      sample_chunks[[length(sample_chunks) + 1L]] <- list(
        first_timestamp = record$timestamps[[from]],
        stream_id = record$output_id,
        bytes = .xdf_sample_chunk(record, from, to)
      )
    }
  }
  if (length(sample_chunks)) {
    queues <- split(
      sample_chunks,
      vapply(sample_chunks, function(chunk) {
        .xdf_id_text(chunk$stream_id)
      }, character(1))
    )
    ordered <- list()
    while (length(queues)) {
      heads <- lapply(queues, `[[`, 1L)
      choice <- order(
        vapply(heads, `[[`, numeric(1), "first_timestamp"),
        vapply(heads, `[[`, numeric(1), "stream_id"),
        method = "radix"
      )[[1L]]
      ordered[[length(ordered) + 1L]] <- heads[[choice]]$bytes
      queues[[choice]] <- queues[[choice]][-1L]
      if (!length(queues[[choice]])) {
        queues[[choice]] <- NULL
      }
    }
    chunks <- c(chunks, ordered)
  }
  for (record in records) {
    chunks[[length(chunks) + 1L]] <- .xdf_chunk(
      6L,
      c(
        .xdf_raw_uint(record$output_id, 4L),
        charToRaw(enc2utf8(.xdf_build_footer(record)))
      )
    )
  }
  bytes <- do.call(c, chunks)
  if (length(bytes) > .xdf_allocation_limit) {
    .stream_abort(
      "serialized XDF exceeds the internal allocation ceiling",
      "PhysioStream_xdf_resource_error"
    )
  }
  bytes
}
