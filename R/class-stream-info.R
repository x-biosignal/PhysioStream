.stream_schema_version <- "1.0.0"
.stream_dtypes <- c("float64", "float32", "int32", "int16", "int8", "string")
.stream_metadata_limit <- 1024L * 1024L

.stream_abort <- function(message, class = "PhysioStream_error") {
  cond <- structure(
    list(message = message, call = NULL),
    class = c(class, "PhysioStream_error", "error", "condition")
  )
  stop(cond)
}

.stream_scalar_string <- function(x, name, allow_empty = FALSE) {
  ok <- is.character(x) && length(x) == 1L && !is.na(x)
  if (!allow_empty) {
    ok <- ok && nzchar(x)
  }
  if (!ok) {
    .stream_abort(
      sprintf("`%s` must be a %s scalar string", name,
              if (allow_empty) "non-missing" else "non-empty"),
      "PhysioStream_validation_error"
    )
  }
  x
}

.stream_metadata_valid <- function(x, path = "metadata") {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is.list(x)) {
    if (length(x)) {
      nm <- names(x)
      if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
        return(sprintf("`%s` lists must have unique, non-empty names", path))
      }
      for (i in seq_along(x)) {
        valid <- .stream_metadata_valid(x[[i]], paste0(path, "$", nm[[i]]))
        if (!isTRUE(valid)) {
          return(valid)
        }
      }
    }
    return(TRUE)
  }
  if (!is.atomic(x) || methods::is(x, "externalptr") || isS4(x)) {
    return(sprintf("`%s` must contain only finite atomic values or named lists",
                   path))
  }
  if (is.numeric(x) || is.complex(x)) {
    if (any(!is.finite(x))) {
      return(sprintf("`%s` contains non-finite values", path))
    }
  } else if (!is.raw(x) && anyNA(x)) {
    return(sprintf("`%s` contains missing values", path))
  }
  TRUE
}

.stream_info_validity <- function(object) {
  for (nm in c("name", "type", "clock_domain")) {
    value <- methods::slot(object, nm)
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(value)) {
      return(sprintf("`%s` must be a non-empty scalar string", nm))
    }
  }
  if (!is.character(object@source_id) || length(object@source_id) != 1L ||
      is.na(object@source_id)) {
    return("`source_id` must be a non-missing scalar string")
  }
  if (!is.integer(object@n_channels) || length(object@n_channels) != 1L ||
      is.na(object@n_channels) || object@n_channels < 1L) {
    return("`n_channels` must be one positive integer")
  }
  channels <- object@channel_names
  if (!is.character(channels) || length(channels) != object@n_channels ||
      anyNA(channels) || any(!nzchar(channels)) || anyDuplicated(channels)) {
    return("`channel_names` must be unique non-empty strings matching `n_channels`")
  }
  units <- object@channel_units
  if (!is.null(units) &&
      (!is.character(units) || length(units) != object@n_channels ||
       anyNA(units))) {
    return("`channel_units` must be NULL or non-missing strings matching channels")
  }
  rate <- object@nominal_srate
  if (!is.numeric(rate) || length(rate) != 1L || !is.finite(rate) || rate < 0) {
    return("`nominal_srate` must be one finite value greater than or equal to zero")
  }
  if (!is.character(object@dtype) || length(object@dtype) != 1L ||
      is.na(object@dtype) || !(object@dtype %in% .stream_dtypes)) {
    return("`dtype` is not an exact supported value")
  }
  valid_metadata <- .stream_metadata_valid(object@metadata)
  if (!isTRUE(valid_metadata)) {
    return(valid_metadata)
  }
  payload <- tryCatch(
    serialize(object@metadata, NULL, version = 3L),
    error = function(e) NULL
  )
  if (is.null(payload) || length(payload) > .stream_metadata_limit) {
    return("`metadata` must serialize to at most 1 MiB")
  }
  if (!identical(object@schema_version, .stream_schema_version)) {
    return("`schema_version` must be exactly '1.0.0'")
  }
  TRUE
}

#' Validated stream metadata
#'
#' @slot name,type,source_id,clock_domain Scalar stream identity strings.
#' @slot n_channels Exact positive channel count.
#' @slot channel_names,channel_units Channel labels and optional units.
#' @slot nominal_srate Nominal samples per second; zero denotes an irregular
#'   stream reserved for later transports.
#' @slot dtype Exact storage type.
#' @slot metadata Named recursively serializable metadata.
#' @slot schema_version Metadata schema identifier.
#' @examples
#' info <- streamInfo("eeg-demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 250)
#' info
#' @exportClass StreamInfo
methods::setClass(
  "StreamInfo",
  slots = c(
    name = "character",
    type = "character",
    n_channels = "integer",
    channel_names = "character",
    channel_units = "ANY",
    nominal_srate = "numeric",
    dtype = "character",
    source_id = "character",
    clock_domain = "character",
    metadata = "list",
    schema_version = "character"
  ),
  validity = .stream_info_validity
)

#' Construct or retrieve stream metadata
#'
#' With a character first argument, constructs validated stream metadata. With
#' a stream endpoint or ring buffer, retrieves its immutable `StreamInfo`.
#'
#' @param name A non-empty stream name, or a stream object when used as an
#'   accessor.
#' @param type A non-empty stream type.
#' @param channel_names Unique non-empty channel labels.
#' @param nominal_srate A finite nominal sampling rate greater than or equal to
#'   zero.
#' @param dtype Exact storage type.
#' @param source_id Optional stable source identifier.
#' @param clock_domain Non-empty clock-domain label.
#' @param channel_units Optional units matching `channel_names`.
#' @param metadata Named recursively serializable metadata, at most 1 MiB.
#' @param ... Reserved for methods.
#' @return A `StreamInfo` object, or an endpoint's `StreamInfo`.
#' @examples
#' info <- streamInfo("eeg-demo", type = "EEG",
#'                    channel_names = c("C3", "Cz", "C4"),
#'                    nominal_srate = 250, channel_units = rep("uV", 3))
#' info
#' streamChannels(info)
#' @export
methods::setGeneric("streamInfo", function(name, ...) {
  standardGeneric("streamInfo")
})

#' @rdname streamInfo
#' @export
methods::setMethod(
  "streamInfo",
  "character",
  function(name, type, channel_names, nominal_srate,
           dtype = c("float64", "float32", "int32", "int16", "int8", "string"),
           source_id = "", clock_domain = "local", channel_units = NULL,
           metadata = list(), ...) {
    .stream_scalar_string(name, "name")
    .stream_scalar_string(type, "type")
    .stream_scalar_string(source_id, "source_id", allow_empty = TRUE)
    .stream_scalar_string(clock_domain, "clock_domain")
    if (!is.character(channel_names) || !length(channel_names) ||
        anyNA(channel_names) || any(!nzchar(channel_names)) ||
        anyDuplicated(channel_names)) {
      .stream_abort("`channel_names` must be unique non-empty strings",
                    "PhysioStream_validation_error")
    }
    if (length(channel_names) > .Machine$integer.max) {
      .stream_abort("too many channels", "PhysioStream_validation_error")
    }
    if (!is.null(channel_units) &&
        (!is.character(channel_units) ||
         length(channel_units) != length(channel_names) ||
         anyNA(channel_units))) {
      .stream_abort("`channel_units` must be NULL or match `channel_names`",
                    "PhysioStream_validation_error")
    }
    if (!is.numeric(nominal_srate) || length(nominal_srate) != 1L ||
        !is.finite(nominal_srate) || nominal_srate < 0) {
      .stream_abort("`nominal_srate` must be finite and >= 0",
                    "PhysioStream_validation_error")
    }
    if (missing(dtype)) {
      dtype <- "float64"
    }
    if (!is.character(dtype) || length(dtype) != 1L || is.na(dtype) ||
        !(dtype %in% .stream_dtypes)) {
      .stream_abort("`dtype` must be one exact supported value",
                    "PhysioStream_validation_error")
    }
    if (!is.list(metadata)) {
      .stream_abort("`metadata` must be a list",
                    "PhysioStream_validation_error")
    }
    object <- methods::new(
      "StreamInfo",
      name = name,
      type = type,
      n_channels = as.integer(length(channel_names)),
      channel_names = channel_names,
      channel_units = channel_units,
      nominal_srate = as.numeric(nominal_srate),
      dtype = dtype,
      source_id = source_id,
      clock_domain = clock_domain,
      metadata = metadata,
      schema_version = .stream_schema_version
    )
    methods::validObject(object)
    object
  }
)

#' @rdname streamInfo
#' @export
methods::setMethod("streamInfo", "StreamInfo", function(name, ...) name)

#' @param object A `StreamInfo` object to display.
#' @rdname StreamInfo-class
#' @export
methods::setMethod("show", "StreamInfo", function(object) {
  cat("StreamInfo<", object@schema_version, ">: ", object@name, "\n", sep = "")
  cat("  type: ", object@type, "; channels: ", object@n_channels,
      "; rate: ", format(object@nominal_srate), " Hz; dtype: ",
      object@dtype, "\n", sep = "")
})

.stream_info_accessor <- function(slot_name) {
  force(slot_name)
  function(x) methods::slot(x, slot_name)
}

#' Stream metadata accessors
#'
#' @param x A `StreamInfo`, endpoint, or ring buffer.
#' @return The requested metadata field.
#' @examples
#' info <- streamInfo("eeg-demo", type = "EEG",
#'                    channel_names = c("C3", "C4"), nominal_srate = 250,
#'                    dtype = "float32")
#' streamName(info)
#' streamType(info)
#' streamChannels(info)
#' streamRate(info)
#' streamDtype(info)
#' @name stream-metadata
NULL

#' @rdname stream-metadata
#' @export
streamName <- function(x) methods::slot(streamInfo(x), "name")

#' @rdname stream-metadata
#' @export
streamType <- function(x) methods::slot(streamInfo(x), "type")

#' @rdname stream-metadata
#' @export
streamChannels <- function(x) methods::slot(streamInfo(x), "channel_names")

#' @rdname stream-metadata
#' @export
streamRate <- function(x) methods::slot(streamInfo(x), "nominal_srate")

#' @rdname stream-metadata
#' @export
streamDtype <- function(x) methods::slot(streamInfo(x), "dtype")
