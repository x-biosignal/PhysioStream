.trigger_schema_version <- "1.0.0"
.trigger_command_schema <- "physiostream.stim-command/1.0.0"
.trigger_state_limit <- 16 * 1024^2
.trigger_allocation_limit <- 512 * 1024^2
.trigger_max_exact_ns <- 2^53

.trigger_abort <- function(message,
                           class = "PhysioStream_trigger_error") {
  .stream_abort(message, class)
}

.trigger_scalar <- function(x, name, lower = -Inf, upper = Inf,
                            integer = FALSE, lower_open = FALSE,
                            upper_open = FALSE) {
  valid <- !is.factor(x) && !is.object(x) && is.numeric(x) &&
    is.null(dim(x)) && length(x) == 1L && is.finite(x)
  valid <- valid && if (lower_open) x > lower else x >= lower
  valid <- valid && if (upper_open) x < upper else x <= upper
  if (integer) {
    valid <- valid && x == floor(x) && x <= .Machine$integer.max
  }
  if (!valid) {
    .trigger_abort(
      sprintf("`%s` has an invalid value", name),
      "PhysioStream_trigger_validation_error"
    )
  }
  if (integer) as.integer(x) else as.numeric(x)
}

.trigger_logical <- function(x, name) {
  if (!is.logical(x) || is.object(x) || !is.null(dim(x)) ||
      length(x) != 1L || is.na(x)) {
    .trigger_abort(
      sprintf("`%s` must be one plain non-missing logical value", name),
      "PhysioStream_trigger_validation_error"
    )
  }
  x
}

.trigger_string <- function(x, name, max_bytes = 4096L,
                            pattern = NULL) {
  valid <- !is.factor(x) && !is.object(x) && is.character(x) &&
    is.null(dim(x)) && length(x) == 1L &&
    !is.na(x) && nzchar(x) &&
    nchar(x, type = "bytes") <= max_bytes
  if (valid && !is.null(pattern)) {
    valid <- grepl(pattern, x, perl = TRUE)
  }
  if (!valid) {
    .trigger_abort(
      sprintf("`%s` must be one valid non-empty string", name),
      "PhysioStream_trigger_validation_error"
    )
  }
  enc2utf8(x)
}

.trigger_id <- function(x, name) {
  .trigger_string(
    x, name, max_bytes = 128L,
    pattern = "\\A[A-Za-z0-9][A-Za-z0-9._:-]{0,127}\\z"
  )
}

.trigger_enum <- function(x, choices, name) {
  valid <- !is.factor(x) && !is.object(x) && is.character(x) &&
    is.null(dim(x)) && length(x) == 1L && !is.na(x) &&
    x %in% choices
  if (!valid) {
    .trigger_abort(
      sprintf(
        "`%s` must be exactly one of: %s",
        name, paste(sprintf("'%s'", choices), collapse = ", ")
      ),
      "PhysioStream_trigger_validation_error"
    )
  }
  enc2utf8(x)
}

.trigger_channels <- function(x) {
  if (is.factor(x) || is.object(x) || !is.character(x) ||
      !is.null(dim(x)) || !length(x) ||
      anyNA(x) || any(!nzchar(x)) || anyDuplicated(x) ||
      any(nchar(x, type = "bytes") > 256L)) {
    .trigger_abort(
      "`allowed_channels` must contain unique non-empty strings",
      "PhysioStream_trigger_validation_error"
    )
  }
  enc2utf8(x)
}

.trigger_channel_values <- function(x, channels, name,
                                    lower = 0, lower_open = FALSE) {
  valid <- !is.factor(x) && !is.object(x) && is.numeric(x) &&
    is.null(dim(x)) &&
    length(x) %in% c(1L, length(channels)) && all(is.finite(x))
  valid <- valid && if (lower_open) all(x > lower) else all(x >= lower)
  if (valid && length(x) > 1L) {
    valid <- !is.null(names(x)) && !anyNA(names(x)) &&
      identical(sort(names(x)), sort(channels)) &&
      !anyDuplicated(names(x))
  }
  if (!valid) {
    .trigger_abort(
      sprintf(
        "`%s` must be a finite scalar or an exactly channel-named vector",
        name
      ),
      "PhysioStream_trigger_validation_error"
    )
  }
  if (length(x) == 1L) {
    x <- rep(as.numeric(x), length(channels))
    names(x) <- channels
  } else {
    x <- as.numeric(x[channels])
    names(x) <- channels
  }
  x
}

.trigger_ms_to_ns <- function(x, name, allow_zero = FALSE) {
  x <- .trigger_scalar(
    x, name, lower = 0, lower_open = !allow_zero,
    upper = .trigger_max_exact_ns / 1e6
  )
  ns <- x * 1e6
  if (!is.finite(ns) || ns != floor(ns) || ns > .trigger_max_exact_ns) {
    .trigger_abort(
      sprintf("`%s` cannot be represented as exact monotonic nanoseconds",
              name),
      "PhysioStream_trigger_validation_error"
    )
  }
  as.numeric(ns)
}

.trigger_channel_ms <- function(x, channels, name, allow_zero = FALSE) {
  x <- .trigger_channel_values(
    x, channels, name, lower = 0, lower_open = !allow_zero
  )
  out <- vapply(
    seq_along(x),
    function(i) .trigger_ms_to_ns(x[[i]], name, allow_zero = allow_zero),
    numeric(1)
  )
  names(out) <- channels
  list(ms = x, ns = out)
}

.trigger_state_hash <- function(state) {
  payload <- state
  payload$state_sha256 <- NULL
  digest::digest(
    serialize(payload, NULL, version = 3L),
    algo = "sha256", serialize = FALSE
  )
}

.trigger_nonplain_path <- function(x, path) {
  if (is.object(x) || !is.null(dim(x))) {
    return(path)
  }
  attribute_names <- names(attributes(x))
  if (length(setdiff(attribute_names, "names"))) {
    return(path)
  }
  if (is.list(x)) {
    names_x <- names(x)
    for (i in seq_along(x)) {
      label <- if (!is.null(names_x) && nzchar(names_x[[i]])) {
        names_x[[i]]
      } else {
        as.character(i)
      }
      bad <- .trigger_nonplain_path(
        x[[i]], paste0(path, "$", label)
      )
      if (!is.null(bad)) {
        return(bad)
      }
    }
  }
  NULL
}

.trigger_plain_bytes <- function(x, label,
                                 limit = .trigger_allocation_limit) {
  bad <- .dsp_runtime_path(x, label)
  if (!is.null(bad)) {
    .trigger_abort(
      sprintf("unsupported runtime value at `%s`", bad),
      "PhysioStream_trigger_state_error"
    )
  }
  object_path <- .trigger_nonplain_path(x, label)
  if (!is.null(object_path)) {
    .trigger_abort(
      sprintf("non-plain object at `%s`", object_path),
      "PhysioStream_trigger_state_error"
    )
  }
  bytes <- tryCatch(
    serialize(x, NULL, version = 3L),
    error = function(e) NULL
  )
  if (is.null(bytes) || length(bytes) > limit) {
    .trigger_abort(
      sprintf("`%s` exceeds the governed serialization ceiling", label),
      "PhysioStream_trigger_resource_error"
    )
  }
  invisible(length(bytes))
}

.trigger_seal_state <- function(state) {
  state$schema <- .trigger_schema_version
  state$state_sha256 <- NULL
  .trigger_plain_bytes(state, "trigger state", .trigger_state_limit)
  state$state_sha256 <- .trigger_state_hash(state)
  .trigger_plain_bytes(state, "trigger state", .trigger_state_limit)
  state
}

.trigger_validate_state <- function(state) {
  valid <- is.list(state) && !is.object(state) &&
    identical(state$schema, .trigger_schema_version) &&
    is.character(state$state_sha256) &&
    length(state$state_sha256) == 1L &&
    !is.na(state$state_sha256) && nzchar(state$state_sha256)
  if (!valid) {
    .trigger_abort(
      "trigger state has an invalid schema",
      "PhysioStream_trigger_state_error"
    )
  }
  .trigger_plain_bytes(state, "trigger state", .trigger_state_limit)
  if (!identical(state$state_sha256, .trigger_state_hash(state))) {
    .trigger_abort(
      "trigger state hash does not match its payload",
      "PhysioStream_trigger_state_error"
    )
  }
  invisible(TRUE)
}

.trigger_configuration <- function(
    backend, transport, allowed_channels, max_intensity, intensity_unit,
    max_duration_ms, refractory_ms, deadman_ms, audit_capacity,
    endpoint = list()) {
  channels <- .trigger_channels(allowed_channels)
  max_intensity <- .trigger_channel_values(
    max_intensity, channels, "max_intensity", lower = 0, lower_open = TRUE
  )
  max_duration <- .trigger_channel_ms(
    max_duration_ms, channels, "max_duration_ms"
  )
  refractory <- .trigger_channel_ms(
    refractory_ms, channels, "refractory_ms", allow_zero = TRUE
  )
  deadman_ns <- .trigger_ms_to_ns(deadman_ms, "deadman_ms")
  audit_capacity <- .trigger_scalar(
    audit_capacity, "audit_capacity", lower = 1, upper = 16384,
    integer = TRUE
  )
  intensity_unit <- .trigger_string(
    intensity_unit, "intensity_unit", max_bytes = 64L
  )
  .trigger_plain_bytes(endpoint, "trigger endpoint", 1024^2)
  list(
    backend = backend,
    transport = transport,
    allowed_channels = channels,
    max_intensity = max_intensity,
    intensity_unit = intensity_unit,
    max_duration_ms = max_duration$ms,
    max_duration_ns = max_duration$ns,
    refractory_ms = refractory$ms,
    refractory_ns = refractory$ns,
    deadman_ms = as.numeric(deadman_ms),
    deadman_ns = deadman_ns,
    audit_capacity = audit_capacity,
    endpoint = endpoint
  )
}

.trigger_empty_counters <- function() {
  list(
    opens = 0,
    closes = 0,
    arms = 0,
    heartbeats = 0,
    disarms = 0,
    emergency_stops = 0,
    attempted = 0,
    acknowledged = 0,
    unknown = 0,
    audit_truncated = 0
  )
}

.trigger_empty_state <- function(configuration) {
  channels <- configuration$allowed_channels
  last_attempt <- rep(list(NULL), length(channels))
  names(last_attempt) <- channels
  list(
    backend = configuration$backend,
    configuration = configuration,
    lifecycle = list(
      status = "closed",
      generation = 0,
      stopped = FALSE,
      last_error_code = NULL
    ),
    arm = list(
      status = "disarmed",
      generation = 0,
      session_id = NULL,
      armed_at_ns = NULL,
      heartbeat_at_ns = NULL,
      disarm_reason = "initial"
    ),
    clock = list(mode = NULL, last_ns = NULL),
    counters = .trigger_empty_counters(),
    last_attempt_by_channel = last_attempt,
    recent_command_ids = character(),
    audit = list(),
    audit_truncated = 0
  )
}

.trigger_clock_value <- function(trigger, state, now_ns) {
  mode <- if (is.null(now_ns)) {
    if (is.function(trigger$clock)) "injected" else "internal"
  } else {
    "explicit"
  }
  if (!is.null(state$clock$mode) && !identical(state$clock$mode, mode)) {
    .trigger_abort(
      "monotonic clock domains cannot be mixed",
      "PhysioStream_trigger_timing_error"
    )
  }
  if (is.null(now_ns)) {
    now_ns <- if (is.function(trigger$clock)) {
      trigger$clock()
    } else {
      cpp_monotonic_ns()
    }
  }
  valid <- !is.factor(now_ns) && !is.object(now_ns) &&
    is.numeric(now_ns) && is.null(dim(now_ns)) &&
    length(now_ns) == 1L && is.finite(now_ns) && now_ns >= 0 &&
    now_ns == floor(now_ns) && now_ns <= .trigger_max_exact_ns
  if (!valid) {
    .trigger_abort(
      "`now_ns` must be one exact non-negative monotonic nanosecond value",
      "PhysioStream_trigger_timing_error"
    )
  }
  now_ns <- as.numeric(now_ns)
  if (!is.null(state$clock$last_ns) && now_ns < state$clock$last_ns) {
    .trigger_abort(
      "`now_ns` decreased within the bound monotonic clock domain",
      "PhysioStream_trigger_timing_error"
    )
  }
  list(value = now_ns, mode = mode)
}

.trigger_apply_clock <- function(state, clock) {
  state$clock$mode <- clock$mode
  state$clock$last_ns <- clock$value
  state
}

.trigger_canonical_command <- function(
    command_id, session_id, channel, intensity, intensity_unit,
    duration_ms, issued_monotonic_ns, backend) {
  command <- list(
    schema = .trigger_command_schema,
    command_id = command_id,
    session_id = session_id,
    channel = channel,
    intensity = as.numeric(intensity),
    intensity_unit = intensity_unit,
    duration_ms = as.numeric(duration_ms),
    issued_monotonic_ns = as.numeric(issued_monotonic_ns),
    backend = backend
  )
  payload_without_hash <- unclass(jsonlite::toJSON(
    command, auto_unbox = TRUE, digits = NA, null = "null",
    pretty = FALSE
  ))
  command$payload_sha256 <- digest::digest(
    charToRaw(enc2utf8(payload_without_hash)),
    algo = "sha256", serialize = FALSE
  )
  payload <- unclass(jsonlite::toJSON(
    command, auto_unbox = TRUE, digits = NA, null = "null",
    pretty = FALSE
  ))
  .trigger_plain_bytes(command, "stimulation command", 1024^2)
  if (nchar(payload, type = "bytes") > 1024^2) {
    .trigger_abort(
      "stimulation command payload exceeds 1 MiB",
      "PhysioStream_trigger_resource_error"
    )
  }
  list(command = command, payload = payload)
}

.trigger_serial_token <- function(x, name) {
  x <- .trigger_string(x, name, max_bytes = 256L)
  if (grepl("[,\r\n]", x, perl = TRUE) ||
      !identical(iconv(x, from = "UTF-8", to = "ASCII", sub = NA), x)) {
    .trigger_abort(
      sprintf("`%s` is not a safe ASCII serial token", name),
      "PhysioStream_trigger_validation_error"
    )
  }
  x
}

.trigger_number_text <- function(x) {
  sprintf("%.17g", as.numeric(x))
}

.trigger_ttl_frame <- function(command, line, pulse_width_ms) {
  fields <- c(
    "STIM",
    .trigger_serial_token(command$schema, "schema"),
    .trigger_serial_token(command$command_id, "command_id"),
    .trigger_serial_token(command$session_id, "session_id"),
    as.character(line),
    .trigger_serial_token(command$channel, "channel"),
    .trigger_number_text(command$intensity),
    .trigger_serial_token(command$intensity_unit, "intensity_unit"),
    .trigger_number_text(command$duration_ms),
    .trigger_number_text(pulse_width_ms)
  )
  body <- paste(fields, collapse = ",")
  crc <- digest::digest(
    charToRaw(body), algo = "crc32", serialize = FALSE
  )
  paste0(body, ",", crc, "\n")
}

.trigger_stop_frame <- function(line) {
  body <- paste(c("STOP", as.character(line)), collapse = ",")
  crc <- digest::digest(
    charToRaw(body), algo = "crc32", serialize = FALSE
  )
  paste0(body, ",", crc, "\n")
}

.trigger_append_audit <- function(state, record) {
  capacity <- state$configuration$audit_capacity
  if (length(state$audit) >= capacity) {
    state$audit <- state$audit[-1L]
    state$audit_truncated <- state$audit_truncated + 1
    state$counters$audit_truncated <-
      state$counters$audit_truncated + 1
  }
  state$audit[[length(state$audit) + 1L]] <- record
  state
}

.trigger_replace_last_audit <- function(state, receipt) {
  if (!length(state$audit)) {
    .trigger_abort(
      "trigger attempt audit is unexpectedly empty",
      "PhysioStream_trigger_state_error"
    )
  }
  state$audit[[length(state$audit)]] <- receipt
  state
}
