.trigger_runtime_fields <- c(
  "adapter", "config", "credentials", "transport", "writer"
)

.trigger_new <- function(class, configuration, runtime_config = list(),
                         credentials = list(), writer = NULL,
                         clock = NULL) {
  if (!is.null(clock) && !is.function(clock)) {
    .trigger_abort(
      "`clock` must be NULL or a function",
      "PhysioStream_trigger_validation_error"
    )
  }
  runtime <- new.env(parent = emptyenv())
  runtime$adapter <- NULL
  runtime$config <- runtime_config
  runtime$credentials <- credentials
  runtime$transport <- configuration$transport
  runtime$writer <- writer

  object <- new.env(parent = emptyenv())
  object$busy <- FALSE
  object$clock <- clock
  object$runtime <- runtime
  object$state <- .trigger_seal_state(
    .trigger_empty_state(configuration)
  )
  class(object) <- c(class, "TriggerBackend", "TriggerRuntime")
  object
}

.trigger_assert <- function(trigger) {
  if (!inherits(trigger, "TriggerBackend") || !is.environment(trigger) ||
      !identical(sort(ls(trigger, all.names = TRUE)),
                 sort(c("busy", "clock", "runtime", "state")))) {
    .trigger_abort(
      "`trigger` must be an intact TriggerBackend",
      "PhysioStream_trigger_validation_error"
    )
  }
  .trigger_validate_state(trigger$state)
  if (!is.logical(trigger$busy) || length(trigger$busy) != 1L ||
      is.na(trigger$busy) ||
      (!is.null(trigger$clock) && !is.function(trigger$clock)) ||
      !is.environment(trigger$runtime) ||
      !identical(sort(ls(trigger$runtime, all.names = TRUE)),
                 sort(.trigger_runtime_fields)) ||
      !identical(trigger$runtime$transport,
                 trigger$state$configuration$transport) ||
      (!is.null(trigger$runtime$writer) &&
       !is.function(trigger$runtime$writer))) {
    .trigger_abort(
      "trigger runtime registry does not match governed state",
      "PhysioStream_trigger_state_error"
    )
  }
  invisible(TRUE)
}

.trigger_require_idle <- function(trigger) {
  if (isTRUE(trigger$busy)) {
    .trigger_abort(
      "trigger mutation is not reentrant",
      "PhysioStream_trigger_state_error"
    )
  }
  invisible(TRUE)
}

.trigger_runtime_snapshot <- function(trigger) {
  list(
    adapter = trigger$runtime$adapter,
    config = .dsp_deep_copy(trigger$runtime$config),
    credentials = .dsp_deep_copy(trigger$runtime$credentials),
    transport = trigger$runtime$transport,
    writer = trigger$runtime$writer,
    clock = trigger$clock
  )
}

.trigger_runtime_unchanged <- function(trigger, snapshot) {
  identical(trigger$runtime$adapter, snapshot$adapter) &&
    identical(trigger$runtime$config, snapshot$config) &&
    identical(trigger$runtime$credentials, snapshot$credentials) &&
    identical(trigger$runtime$transport, snapshot$transport) &&
    identical(trigger$runtime$writer, snapshot$writer) &&
    identical(trigger$clock, snapshot$clock)
}

.trigger_restore_runtime <- function(trigger, snapshot) {
  trigger$runtime$adapter <- snapshot$adapter
  trigger$runtime$config <- snapshot$config
  trigger$runtime$credentials <- snapshot$credentials
  trigger$runtime$transport <- snapshot$transport
  trigger$runtime$writer <- snapshot$writer
  trigger$clock <- snapshot$clock
  invisible(trigger)
}

.trigger_state_unchanged <- function(trigger, state) {
  isTRUE(tryCatch(
    identical(
      serialize(trigger$state, NULL, version = 3L),
      serialize(state, NULL, version = 3L)
    ),
    error = function(e) FALSE
  ))
}

.trigger_commit <- function(trigger, old, candidate) {
  if (!.trigger_state_unchanged(trigger, old)) {
    .trigger_abort(
      "trigger state changed during a transactional update",
      "PhysioStream_trigger_state_error"
    )
  }
  trigger$state <- .trigger_seal_state(candidate)
  invisible(trigger)
}

.trigger_writer_result <- function(result, action) {
  valid <- is.list(result) && !is.object(result) &&
    is.logical(result$ok) && !is.object(result$ok) &&
    is.null(dim(result$ok)) && length(result$ok) == 1L &&
    !is.na(result$ok) &&
    is.character(result$ack_code) && !is.object(result$ack_code) &&
    is.null(dim(result$ack_code)) &&
    length(result$ack_code) == 1L &&
    !is.na(result$ack_code) && nzchar(result$ack_code) &&
    nchar(result$ack_code, type = "bytes") <= 128L
  if (!valid) {
    .trigger_abort(
      sprintf("TTL callback returned an invalid `%s` result", action),
      "PhysioStream_trigger_transport_error"
    )
  }
  plain <- tryCatch(
    {
      .trigger_plain_bytes(result, "TTL callback result", 1024^2)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(plain, "condition")) {
    .trigger_abort(
      "TTL callback returned a non-plain or oversized result",
      "PhysioStream_trigger_transport_error"
    )
  }
  if (identical(action, "pulse") && isTRUE(result$ok)) {
    times <- unlist(
      result[c("assert_time_ns", "deassert_time_ns", "ack_time_ns")],
      use.names = FALSE
    )
    if (length(times) != 3L || !is.numeric(times) ||
        any(!is.finite(times)) || any(times < 0) ||
        any(times != floor(times)) || any(times > .trigger_max_exact_ns) ||
        times[[1L]] > times[[2L]] || times[[2L]] > times[[3L]]) {
      .trigger_abort(
        "TTL callback did not report ordered exact pulse timestamps",
        "PhysioStream_trigger_transport_error"
      )
    }
  }
  result
}

.trigger_stop_payload <- function(state, reason, now_ns) {
  payload <- list(
    schema = "physiostream.stop/1.0.0",
    session_id = state$arm$session_id,
    reason = reason,
    issued_monotonic_ns = now_ns,
    backend = state$backend
  )
  unclass(jsonlite::toJSON(
    payload, auto_unbox = TRUE, digits = NA, null = "null",
    pretty = FALSE
  ))
}

.trigger_transport_open <- function(trigger) {
  transport <- trigger$runtime$transport
  if (transport %in% c("loopback", "ttl_loopback")) {
    return(NULL)
  }
  if (identical(transport, "mqtt")) {
    return(.mqtt_open_adapter(
      trigger$runtime$config, trigger$runtime$credentials
    ))
  }
  if (identical(transport, "serial")) {
    return(.ttl_open_serial_adapter(trigger$runtime$config))
  }
  if (identical(transport, "callback")) {
    result <- trigger$runtime$writer(list(
      action = "stop",
      line = trigger$runtime$config$line,
      reason = "open_fail_safe",
      payload_raw = charToRaw(.trigger_stop_frame(
        trigger$runtime$config$line
      ))
    ))
    result <- .trigger_writer_result(result, "stop")
    if (!isTRUE(result$ok)) {
      .trigger_abort(
        "TTL callback could not establish fail-safe low on open",
        "PhysioStream_trigger_transport_error"
      )
    }
    return(NULL)
  }
  .trigger_abort(
    "unsupported trigger transport",
    "PhysioStream_trigger_state_error"
  )
}

.trigger_transport_close <- function(trigger) {
  adapter <- trigger$runtime$adapter
  transport <- trigger$runtime$transport
  if (identical(transport, "mqtt") && !is.null(adapter)) {
    adapter$close()
  } else if (identical(transport, "serial") && !is.null(adapter)) {
    adapter$close()
  }
  invisible(TRUE)
}

.trigger_transport_stop <- function(trigger, reason, now_ns) {
  transport <- trigger$runtime$transport
  adapter <- trigger$runtime$adapter
  if (transport %in% c("loopback", "ttl_loopback")) {
    return(list(ok = TRUE, ack_code = "loopback_stop"))
  }
  if (identical(transport, "mqtt")) {
    if (is.null(adapter)) {
      .trigger_abort(
        "MQTT adapter is unavailable",
        "PhysioStream_trigger_transport_error"
      )
    }
    return(adapter$publish(
      .trigger_stop_payload(trigger$state, reason, now_ns)
    ))
  }
  if (identical(transport, "serial")) {
    if (is.null(adapter)) {
      .trigger_abort(
        "serial adapter is unavailable",
        "PhysioStream_trigger_transport_error"
      )
    }
    return(adapter$stop())
  }
  if (identical(transport, "callback")) {
    result <- trigger$runtime$writer(list(
      action = "stop",
      line = trigger$runtime$config$line,
      reason = reason,
      payload_raw = charToRaw(.trigger_stop_frame(
        trigger$runtime$config$line
      ))
    ))
    return(.trigger_writer_result(result, "stop"))
  }
  .trigger_abort(
    "unsupported trigger transport",
    "PhysioStream_trigger_state_error"
  )
}

.trigger_transport_send <- function(trigger, canonical) {
  transport <- trigger$runtime$transport
  adapter <- trigger$runtime$adapter
  if (identical(transport, "loopback")) {
    return(list(ok = TRUE, ack_code = "loopback_ack"))
  }
  if (identical(transport, "mqtt")) {
    if (is.null(adapter)) {
      .trigger_abort(
        "MQTT adapter is unavailable",
        "PhysioStream_trigger_transport_error"
      )
    }
    return(adapter$publish(canonical$payload))
  }
  frame <- .trigger_ttl_frame(
    canonical$command,
    trigger$runtime$config$line,
    trigger$runtime$config$pulse_width_ms
  )
  if (identical(transport, "ttl_loopback")) {
    return(list(
      ok = TRUE, ack_code = "ttl_loopback_ack",
      frame = frame,
      assert_time_ns = canonical$command$issued_monotonic_ns,
      deassert_time_ns = canonical$command$issued_monotonic_ns,
      ack_time_ns = canonical$command$issued_monotonic_ns
    ))
  }
  if (identical(transport, "serial")) {
    if (is.null(adapter)) {
      .trigger_abort(
        "serial adapter is unavailable",
        "PhysioStream_trigger_transport_error"
      )
    }
    result <- adapter$send(frame)
    result$frame <- frame
    return(result)
  }
  if (identical(transport, "callback")) {
    result <- trigger$runtime$writer(list(
      action = "pulse",
      line = trigger$runtime$config$line,
      pulse_width_ms = trigger$runtime$config$pulse_width_ms,
      command = .dsp_deep_copy(canonical$command),
      payload_raw = charToRaw(frame)
    ))
    result <- .trigger_writer_result(result, "pulse")
    if (isTRUE(result$ok) &&
        (result$assert_time_ns <
           canonical$command$issued_monotonic_ns ||
         result$deassert_time_ns <= result$assert_time_ns)) {
      .trigger_abort(
        "TTL callback reported an invalid observed pulse interval",
        "PhysioStream_trigger_transport_error"
      )
    }
    result$frame <- frame
    return(result)
  }
  .trigger_abort(
    "unsupported trigger transport",
    "PhysioStream_trigger_state_error"
  )
}

#' Open or close a governed stimulation trigger
#'
#' Trigger constructors are side-effect free. `triggerOpen()` explicitly opens
#' the configured transport and leaves it disarmed. `triggerClose()` performs a
#' bounded best-effort stop/deassert before closing. Software interlocks are
#' additional safeguards only; independently fail-safe stimulation hardware is
#' required.
#'
#' @param trigger A `TriggerBackend`.
#' @return The trigger, invisibly.
#' @examples
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' triggerOpen(trigger)
#' triggerState(trigger)$lifecycle$status
#' triggerClose(trigger)
#' @export
triggerOpen <- function(trigger) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  old <- .dsp_deep_copy(trigger$state)
  if (!identical(old$lifecycle$status, "closed")) {
    .trigger_abort(
      "trigger is already open",
      "PhysioStream_trigger_lifecycle_error"
    )
  }
  runtime <- .trigger_runtime_snapshot(trigger)
  trigger$busy <- TRUE
  on.exit({
    trigger$busy <- FALSE
  }, add = TRUE)
  opened <- tryCatch(
    .trigger_transport_open(trigger),
    error = function(e) e,
    interrupt = function(e) e
  )
  if (!.trigger_state_unchanged(trigger, old) ||
      !.trigger_runtime_unchanged(trigger, runtime)) {
    trigger$state <- old
    .trigger_restore_runtime(trigger, runtime)
    .trigger_abort(
      "trigger runtime changed during transport open",
      "PhysioStream_trigger_state_error"
    )
  }
  if (inherits(opened, "condition")) {
    .trigger_restore_runtime(trigger, runtime)
    .trigger_abort(
      "trigger transport open failed",
      if (inherits(opened, "PhysioStream_trigger_unavailable")) {
        "PhysioStream_trigger_unavailable"
      } else {
        "PhysioStream_trigger_transport_error"
      }
    )
  }
  candidate <- old
  candidate$lifecycle$status <- "open"
  candidate$lifecycle$generation <- candidate$lifecycle$generation + 1
  candidate$lifecycle$stopped <- FALSE
  candidate$lifecycle$last_error_code <- NULL
  candidate$arm <- list(
    status = "disarmed", generation = old$arm$generation,
    session_id = NULL, armed_at_ns = NULL,
    heartbeat_at_ns = NULL, disarm_reason = "open"
  )
  candidate$counters$opens <- candidate$counters$opens + 1
  trigger$runtime$adapter <- opened
  trigger$state <- .trigger_seal_state(candidate)
  invisible(trigger)
}

#' @rdname triggerOpen
#' @export
triggerClose <- function(trigger) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  old <- .dsp_deep_copy(trigger$state)
  if (identical(old$lifecycle$status, "closed")) {
    return(invisible(trigger))
  }
  runtime <- .trigger_runtime_snapshot(trigger)
  trigger$busy <- TRUE
  on.exit({
    trigger$busy <- FALSE
  }, add = TRUE)
  now_ns <- old$clock$last_ns
  if (is.null(now_ns)) {
    now_ns <- 0
  }
  stop_result <- tryCatch(
    .trigger_transport_stop(trigger, "close", now_ns),
    error = function(e) e,
    interrupt = function(e) e
  )
  close_result <- tryCatch(
    .trigger_transport_close(trigger),
    error = function(e) e,
    interrupt = function(e) e
  )
  runtime_changed <- !.trigger_runtime_unchanged(trigger, runtime)
  state_changed <- !.trigger_state_unchanged(trigger, old)
  trigger$state <- old
  .trigger_restore_runtime(trigger, runtime)
  trigger$runtime$adapter <- NULL

  failed <- runtime_changed || state_changed ||
    inherits(stop_result, "condition") ||
    inherits(close_result, "condition") ||
    (is.list(stop_result) && !isTRUE(stop_result$ok))
  candidate <- old
  candidate$lifecycle$status <- "closed"
  candidate$lifecycle$stopped <- isTRUE(failed)
  candidate$lifecycle$last_error_code <- if (failed) {
    "transport_close_failed"
  } else {
    NULL
  }
  candidate$arm$status <- "disarmed"
  candidate$arm$session_id <- NULL
  candidate$arm$armed_at_ns <- NULL
  candidate$arm$heartbeat_at_ns <- NULL
  candidate$arm$disarm_reason <- "close"
  candidate$counters$closes <- candidate$counters$closes + 1
  trigger$state <- .trigger_seal_state(candidate)
  if (failed) {
    .trigger_abort(
      "trigger closed with a stop/deassert failure",
      "PhysioStream_trigger_transport_error"
    )
  }
  invisible(trigger)
}

#' Inspect governed trigger state
#'
#' The returned deep plain-list snapshot contains no connection, Python object,
#' callback, or credential. Runtime environments are not portable.
#'
#' @param trigger A `TriggerBackend`.
#' @return A sealed plain list.
#' @examples
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' triggerOpen(trigger)
#' triggerState(trigger)$arm$status
#' @export
triggerState <- function(trigger) {
  .trigger_assert(trigger)
  .dsp_deep_copy(trigger$state)
}

#' Arm, heartbeat, disarm, or emergency-stop a trigger
#'
#' Every trigger opens disarmed. Dead-man expiry disarms locally and requires a
#' new explicit arm with a different session ID within the open lifecycle.
#' Emergency stop latches locally even if the transport stop attempt fails.
#' These software gates do not replace device hardware limits or an independent
#' emergency stop.
#'
#' @param trigger A `TriggerBackend`.
#' @param session_id Unique non-empty session identifier.
#' @param now_ns Optional exact process-local monotonic nanosecond value.
#' @param reason Bounded non-sensitive reason code.
#' @return The trigger, invisibly.
#' @examples
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' triggerOpen(trigger)
#' armTrigger(trigger, "session-1", now_ns = 0)
#' triggerState(trigger)$arm$status
#' @export
armTrigger <- function(trigger, session_id, now_ns = NULL) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  old <- .dsp_deep_copy(trigger$state)
  session_id <- .trigger_id(session_id, "session_id")
  if (!identical(old$lifecycle$status, "open") ||
      isTRUE(old$lifecycle$stopped) ||
      !identical(old$arm$status, "disarmed")) {
    .trigger_abort(
      "trigger must be open, disarmed, and not stopped before arming",
      "PhysioStream_trigger_lifecycle_error"
    )
  }
  if (!is.null(old$arm$session_id) &&
      identical(old$arm$session_id, session_id)) {
    .trigger_abort(
      "arming requires a new session id",
      "PhysioStream_trigger_lifecycle_error"
    )
  }
  clock <- .trigger_clock_value(trigger, old, now_ns)
  candidate <- .trigger_apply_clock(old, clock)
  candidate$arm <- list(
    status = "armed",
    generation = old$arm$generation + 1,
    session_id = session_id,
    armed_at_ns = clock$value,
    heartbeat_at_ns = clock$value,
    disarm_reason = NULL
  )
  candidate$counters$arms <- candidate$counters$arms + 1
  .trigger_commit(trigger, old, candidate)
}

#' @rdname armTrigger
#' @export
triggerHeartbeat <- function(trigger, now_ns = NULL) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  old <- .dsp_deep_copy(trigger$state)
  if (!identical(old$lifecycle$status, "open") ||
      !identical(old$arm$status, "armed") ||
      isTRUE(old$lifecycle$stopped)) {
    .trigger_abort(
      "heartbeat requires an open armed trigger",
      "PhysioStream_trigger_lifecycle_error"
    )
  }
  clock <- .trigger_clock_value(trigger, old, now_ns)
  age <- clock$value - old$arm$heartbeat_at_ns
  if (age > old$configuration$deadman_ns) {
    candidate <- .trigger_apply_clock(old, clock)
    candidate$arm$status <- "disarmed"
    candidate$arm$disarm_reason <- "deadman_expired"
    candidate$counters$disarms <- candidate$counters$disarms + 1
    .trigger_commit(trigger, old, candidate)
    .trigger_abort(
      "dead-man interval expired; explicit re-arm is required",
      "PhysioStream_trigger_deadman_error"
    )
  }
  candidate <- .trigger_apply_clock(old, clock)
  candidate$arm$heartbeat_at_ns <- clock$value
  candidate$counters$heartbeats <- candidate$counters$heartbeats + 1
  .trigger_commit(trigger, old, candidate)
}

#' @rdname armTrigger
#' @export
disarmTrigger <- function(trigger, reason = "caller", now_ns = NULL) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  reason <- .trigger_id(reason, "reason")
  old <- .dsp_deep_copy(trigger$state)
  if (identical(old$arm$status, "disarmed")) {
    return(invisible(trigger))
  }
  clock <- .trigger_clock_value(trigger, old, now_ns)
  candidate <- .trigger_apply_clock(old, clock)
  candidate$arm$status <- "disarmed"
  candidate$arm$disarm_reason <- reason
  candidate$counters$disarms <- candidate$counters$disarms + 1
  .trigger_commit(trigger, old, candidate)
}

#' @rdname armTrigger
#' @export
emergencyStop <- function(trigger, reason = "caller", now_ns = NULL) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  reason <- .trigger_id(reason, "reason")
  old <- .dsp_deep_copy(trigger$state)
  clock <- .trigger_clock_value(trigger, old, now_ns)
  candidate <- .trigger_apply_clock(old, clock)
  candidate$lifecycle$stopped <- TRUE
  candidate$arm$status <- "disarmed"
  candidate$arm$disarm_reason <- paste0("emergency_stop:", reason)
  candidate$counters$emergency_stops <-
    candidate$counters$emergency_stops + 1
  candidate <- .trigger_seal_state(candidate)
  trigger$state <- candidate

  if (!identical(old$lifecycle$status, "open")) {
    return(invisible(trigger))
  }
  runtime <- .trigger_runtime_snapshot(trigger)
  trigger$busy <- TRUE
  on.exit({
    trigger$busy <- FALSE
  }, add = TRUE)
  result <- tryCatch(
    .trigger_transport_stop(trigger, reason, clock$value),
    error = function(e) e,
    interrupt = function(e) e
  )
  runtime_changed <- !.trigger_runtime_unchanged(trigger, runtime)
  state_changed <- !.trigger_state_unchanged(trigger, candidate)
  if (runtime_changed || state_changed) {
    trigger$state <- .trigger_seal_state(candidate)
    .trigger_restore_runtime(trigger, runtime)
    result <- structure(
      list(message = "runtime mutation"),
      class = c("PhysioStream_trigger_state_error", "error", "condition")
    )
  }
  if (inherits(result, "condition") ||
      (is.list(result) && !isTRUE(result$ok))) {
    candidate$lifecycle$last_error_code <- "emergency_stop_transport_failed"
    trigger$state <- .trigger_seal_state(candidate)
    .trigger_abort(
      "emergency stop latched locally but transport stop failed",
      "PhysioStream_trigger_transport_error"
    )
  }
  invisible(trigger)
}

.trigger_validate_send <- function(trigger, old, intensity, channel,
                                   duration_ms, command_id, now_ns) {
  if (!identical(old$lifecycle$status, "open") ||
      isTRUE(old$lifecycle$stopped) ||
      !identical(old$arm$status, "armed")) {
    .trigger_abort(
      "sendStim requires an open armed trigger",
      "PhysioStream_trigger_lifecycle_error"
    )
  }
  clock <- .trigger_clock_value(trigger, old, now_ns)
  if (clock$value - old$arm$heartbeat_at_ns >
      old$configuration$deadman_ns) {
    candidate <- .trigger_apply_clock(old, clock)
    candidate$arm$status <- "disarmed"
    candidate$arm$disarm_reason <- "deadman_expired"
    candidate$counters$disarms <- candidate$counters$disarms + 1
    .trigger_commit(trigger, old, candidate)
    .trigger_abort(
      "dead-man interval expired; explicit re-arm is required",
      "PhysioStream_trigger_deadman_error"
    )
  }
  command_id <- .trigger_id(command_id, "command_id")
  if (command_id %in% old$recent_command_ids) {
    .trigger_abort(
      "duplicate stimulation command id",
      "PhysioStream_trigger_duplicate_error"
    )
  }
  channel <- .trigger_string(channel, "channel", max_bytes = 256L)
  if (!(channel %in% old$configuration$allowed_channels)) {
    .trigger_abort(
      "stimulation channel is not allowed",
      "PhysioStream_trigger_interlock_error"
    )
  }
  intensity <- .trigger_scalar(intensity, "intensity", lower = 0)
  if (intensity > old$configuration$max_intensity[[channel]]) {
    .trigger_abort(
      "stimulation intensity exceeds the configured channel maximum",
      "PhysioStream_trigger_interlock_error"
    )
  }
  duration_ns <- .trigger_ms_to_ns(duration_ms, "duration_ms")
  duration_ms <- as.numeric(duration_ms)
  if (duration_ns > old$configuration$max_duration_ns[[channel]]) {
    .trigger_abort(
      "stimulation duration exceeds the configured channel maximum",
      "PhysioStream_trigger_interlock_error"
    )
  }
  last_attempt <- old$last_attempt_by_channel[[channel]]
  if (!is.null(last_attempt) &&
      clock$value - last_attempt <
      old$configuration$refractory_ns[[channel]]) {
    .trigger_abort(
      "stimulation command violates the configured refractory interval",
      "PhysioStream_trigger_refractory_error"
    )
  }
  canonical <- .trigger_canonical_command(
    command_id = command_id,
    session_id = old$arm$session_id,
    channel = channel,
    intensity = intensity,
    intensity_unit = old$configuration$intensity_unit,
    duration_ms = duration_ms,
    issued_monotonic_ns = clock$value,
    backend = old$backend
  )
  list(
    clock = clock, command_id = command_id, channel = channel,
    canonical = canonical
  )
}

#' Send a governed stimulation command
#'
#' Validation, arm/dead-man, channel maximum, duration, duplicate-ID, and
#' refractory gates run before transport invocation. Once invocation begins,
#' command identity and refractory time are conservatively reserved even when
#' acknowledgement is unknown. Commands are never retried automatically.
#'
#' MQTT acknowledgement means broker/client acceptance; TTL acknowledgement
#' means adapter completion. Neither status proves physical stimulation.
#' Receipts and audit records bind lifecycle and arm generations, validation,
#' attempt, and acknowledgement times, and bounded error class/code. A caller
#' interrupt after transport invocation first finalizes an `unknown` audit
#' record and then re-signals the interrupt.
#'
#' @param trigger A `TriggerBackend`.
#' @param intensity Finite non-negative intensity in the configured unit.
#' @param channel Exact configured channel identifier.
#' @param duration_ms Finite positive command duration in milliseconds.
#' @param command_id Unique bounded command identifier.
#' @param now_ns Optional exact process-local monotonic nanosecond value.
#' @return A deep plain-list command receipt.
#' @examples
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' triggerOpen(trigger)
#' armTrigger(trigger, "session-1", now_ns = 0)
#' receipt <- sendStim(trigger, intensity = 2.5, channel = "left",
#'                     duration_ms = 125, command_id = "cmd-1", now_ns = 10)
#' receipt$status
#' @export
sendStim <- function(trigger, intensity, channel, duration_ms, command_id,
                     now_ns = NULL) {
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  old <- .dsp_deep_copy(trigger$state)
  validated <- .trigger_validate_send(
    trigger, old, intensity, channel, duration_ms, command_id, now_ns
  )
  canonical <- validated$canonical
  clock <- validated$clock

  receipt <- list(
    schema = "physiostream.stim-receipt/1.0.0",
    command = .dsp_deep_copy(canonical$command),
    lifecycle_generation = old$lifecycle$generation,
    arm_generation = old$arm$generation,
    status = "attempted",
    validated_at_ns = clock$value,
    attempted_at_ns = clock$value,
    acknowledged_at_ns = NULL,
    transport = trigger$runtime$transport,
    ack_code = NULL,
    error_class = NULL,
    error_code = NULL,
    payload = canonical$payload
  )
  .trigger_plain_bytes(receipt, "stimulation receipt", 1024^2)

  attempted <- .trigger_apply_clock(old, clock)
  attempted$last_attempt_by_channel[[validated$channel]] <- clock$value
  attempted$recent_command_ids <- c(
    attempted$recent_command_ids, validated$command_id
  )
  capacity <- attempted$configuration$audit_capacity
  if (length(attempted$recent_command_ids) > capacity) {
    attempted$recent_command_ids <- tail(
      attempted$recent_command_ids, capacity
    )
  }
  attempted$counters$attempted <- attempted$counters$attempted + 1
  attempted <- .trigger_append_audit(attempted, receipt)
  attempted <- .trigger_seal_state(attempted)

  runtime <- .trigger_runtime_snapshot(trigger)
  trigger$state <- attempted
  trigger$busy <- TRUE
  on.exit({
    trigger$busy <- FALSE
  }, add = TRUE)
  result <- tryCatch(
    .trigger_transport_send(trigger, canonical),
    error = function(e) e,
    interrupt = function(e) e
  )

  runtime_changed <- !.trigger_runtime_unchanged(trigger, runtime)
  state_changed <- !.trigger_state_unchanged(trigger, attempted)
  if (runtime_changed || state_changed) {
    trigger$state <- attempted
    .trigger_restore_runtime(trigger, runtime)
    result <- structure(
      list(message = "runtime mutation"),
      class = c("PhysioStream_trigger_state_error", "error", "condition")
    )
  }

  ack_code_valid <- is.list(result) && !inherits(result, "condition") &&
    is.character(result$ack_code) && length(result$ack_code) == 1L &&
    !is.na(result$ack_code) && nzchar(result$ack_code) &&
    nchar(result$ack_code, type = "bytes") <= 128L
  acknowledged <- is.list(result) && !inherits(result, "condition") &&
    isTRUE(result$ok) && ack_code_valid
  receipt$status <- if (acknowledged) "acknowledged" else "unknown"
  receipt$ack_code <- if (acknowledged) {
    enc2utf8(result$ack_code)
  } else {
    NULL
  }
  receipt$error_code <- if (!acknowledged) {
    if (inherits(result, "PhysioStream_trigger_state_error")) {
      "runtime_mutation"
    } else if (inherits(result, "condition")) {
      "transport_error"
    } else if (is.list(result) && isTRUE(result$ok)) {
      "invalid_transport_receipt"
    } else {
      "negative_acknowledgement"
    }
  } else {
    NULL
  }
  receipt$error_class <- if (acknowledged) {
    NULL
  } else if (inherits(result, "PhysioStream_trigger_state_error")) {
    "PhysioStream_trigger_state_error"
  } else {
    "PhysioStream_trigger_transport_error"
  }
  if (acknowledged) {
    receipt$acknowledged_at_ns <- if (
        identical(trigger$runtime$transport, "callback")) {
      as.numeric(result$ack_time_ns)
    } else if (
        identical(clock$mode, "explicit") ||
        trigger$runtime$transport %in% c("loopback", "ttl_loopback")) {
      clock$value
    } else {
      .trigger_clock_value(trigger, attempted, NULL)$value
    }
  }
  if (is.list(result) && !inherits(result, "condition")) {
    details <- result[setdiff(
      names(result), c("ok", "ack_code")
    )]
    if (length(details)) {
      .trigger_plain_bytes(details, "transport receipt details", 1024^2)
      receipt$transport_details <- details
    }
  }
  .trigger_plain_bytes(receipt, "stimulation receipt", 1024^2)

  final <- attempted
  if (!is.null(receipt$acknowledged_at_ns)) {
    final$clock$last_ns <- receipt$acknowledged_at_ns
  }
  if (acknowledged) {
    final$counters$acknowledged <- final$counters$acknowledged + 1
  } else {
    final$counters$unknown <- final$counters$unknown + 1
  }
  final <- .trigger_replace_last_audit(final, receipt)
  trigger$state <- .trigger_seal_state(final)
  if (inherits(result, "interrupt")) {
    stop(result)
  }
  .dsp_deep_copy(receipt)
}

#' @export
print.TriggerBackend <- function(x, ...) {
  .trigger_assert(x)
  cat(sprintf(
    "<%s: %s, arm=%s, attempted=%s, unknown=%s>\n",
    class(x)[[1L]], x$state$lifecycle$status, x$state$arm$status,
    x$state$counters$attempted, x$state$counters$unknown
  ))
  invisible(x)
}
