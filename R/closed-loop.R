.closed_loop_max_exact_ns <- 2^53

.closed_loop_ms_to_ns <- function(x, name, allow_zero = TRUE) {
  x <- .closed_loop_scalar(
    x, name, lower = 0, lower_open = !allow_zero,
    upper = .closed_loop_max_exact_ns / 1e6
  )
  value <- x * 1e6
  if (!is.finite(value) || value != floor(value) ||
      value > .closed_loop_max_exact_ns) {
    .closed_loop_abort(
      sprintf("`%s` cannot be represented as exact nanoseconds", name),
      "PhysioStream_closed_loop_validation_error"
    )
  }
  as.numeric(value)
}

.closed_loop_graph_payload <- function(pipeline) {
  state <- pipelineState(pipeline)
  operations <- lapply(state$operations, function(operation) {
    list(
      name = operation$name,
      kind = operation$kind,
      type = operation$type,
      configuration = operation$configuration
    )
  })
  list(configuration = state$configuration, operations = operations)
}

.closed_loop_graph_hash <- function(pipeline) {
  digest::digest(
    serialize(.closed_loop_graph_payload(pipeline), NULL, version = 3L),
    algo = "sha256", serialize = FALSE
  )
}

.closed_loop_empty_counters <- function() {
  list(
    candidates = 0,
    accepted = 0,
    queued = 0,
    attempted = 0,
    acknowledged = 0,
    unknown = 0,
    suppressed_duplicate = 0,
    suppressed_refractory = 0,
    suppressed_capacity = 0,
    suppressed_stop = 0,
    stops = 0,
    emergency_stops = 0,
    errors = 0,
    log_truncated = 0
  )
}

.closed_loop_seal <- function(state) {
  state$controller_schema <- .closed_loop_schema
  .dsp_seal_state(state)
}

.closed_loop_runtime_fields <- c(
  "callbacks", "clock", "pipeline", "session_id", "trigger"
)

.closed_loop_runtime_snapshot <- function(controller) {
  list(
    callbacks = controller$runtime$callbacks,
    clock = controller$runtime$clock,
    pipeline = controller$runtime$pipeline,
    session_id = controller$runtime$session_id,
    trigger = controller$runtime$trigger
  )
}

.closed_loop_runtime_unchanged <- function(controller, snapshot) {
  identical(controller$runtime$callbacks, snapshot$callbacks) &&
    identical(controller$runtime$clock, snapshot$clock) &&
    identical(controller$runtime$pipeline, snapshot$pipeline) &&
    identical(controller$runtime$session_id, snapshot$session_id) &&
    identical(controller$runtime$trigger, snapshot$trigger)
}

.closed_loop_restore_runtime <- function(controller, snapshot) {
  controller$runtime$callbacks <- snapshot$callbacks
  controller$runtime$clock <- snapshot$clock
  controller$runtime$pipeline <- snapshot$pipeline
  controller$runtime$session_id <- snapshot$session_id
  controller$runtime$trigger <- snapshot$trigger
  invisible(controller)
}

.closed_loop_integrity_code <- function(controller) {
  if (!identical(
      controller$runtime$pipeline$callbacks,
      controller$runtime$callbacks
  ) ||
      !identical(
        .closed_loop_graph_hash(controller$runtime$pipeline),
        controller$state$pipeline_graph_sha256
      )) {
    return("pipeline_graph_mutation")
  }
  pipeline_state <- pipelineState(controller$runtime$pipeline)
  if (!identical(
      pipeline_state$sha256,
      controller$state$pipeline_state_sha256
  )) {
    return("pipeline_state_mutation")
  }
  trigger_state <- triggerState(controller$runtime$trigger)
  if (!identical(
      trigger_state$state_sha256,
      controller$state$trigger_state_sha256
  )) {
    return("trigger_state_mutation")
  }
  NULL
}

.closed_loop_assert <- function(controller, integrity = TRUE) {
  if (!inherits(controller, "ClosedLoopController") ||
      !is.environment(controller) ||
      !identical(
        sort(ls(controller, all.names = TRUE)),
        sort(c("busy", "runtime", "state"))
      )) {
    .closed_loop_abort(
      "`controller` must be an intact ClosedLoopController",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  .dsp_validate_state(controller$state)
  if (!identical(
      controller$state$controller_schema,
      .closed_loop_schema
  ) ||
      !is.logical(controller$busy) || length(controller$busy) != 1L ||
      is.na(controller$busy) ||
      !is.environment(controller$runtime) ||
      !identical(
        sort(ls(controller$runtime, all.names = TRUE)),
        sort(.closed_loop_runtime_fields)
      ) ||
      !inherits(controller$runtime$pipeline, "StreamPipeline") ||
      !inherits(controller$runtime$trigger, "TriggerBackend") ||
      (!is.null(controller$runtime$clock) &&
       !is.function(controller$runtime$clock)) ||
      !is.list(controller$runtime$callbacks)) {
    .closed_loop_abort(
      "closed-loop runtime registry is invalid",
      "PhysioStream_closed_loop_state_error"
    )
  }
  .pipeline_assert(controller$runtime$pipeline)
  .trigger_assert(controller$runtime$trigger)
  if (isTRUE(integrity)) {
    code <- .closed_loop_integrity_code(controller)
    if (!is.null(code)) {
      .closed_loop_abort(
        sprintf("closed-loop ownership integrity failed: %s", code),
        "PhysioStream_closed_loop_state_error"
      )
    }
  }
  invisible(TRUE)
}

.closed_loop_require_idle <- function(controller) {
  if (isTRUE(controller$busy)) {
    .closed_loop_abort(
      "closed-loop controller mutation is not reentrant",
      "PhysioStream_closed_loop_state_error"
    )
  }
  invisible(TRUE)
}

.closed_loop_clock <- function(controller, state, now_ns) {
  mode <- if (!is.null(now_ns)) {
    "explicit"
  } else if (is.function(controller$runtime$clock)) {
    "injected"
  } else {
    "internal"
  }
  if (!is.null(state$clock$mode) &&
      !identical(state$clock$mode, mode)) {
    .closed_loop_abort(
      "closed-loop monotonic clock domains cannot be mixed",
      "PhysioStream_closed_loop_timing_error"
    )
  }
  if (is.null(now_ns)) {
    now_ns <- if (identical(mode, "injected")) {
      controller$runtime$clock()
    } else {
      cpp_monotonic_ns()
    }
  }
  valid <- !is.factor(now_ns) && !is.object(now_ns) &&
    is.numeric(now_ns) && is.null(dim(now_ns)) &&
    length(now_ns) == 1L && is.finite(now_ns) && now_ns >= 0 &&
    now_ns == floor(now_ns) && now_ns <= .closed_loop_max_exact_ns
  if (!valid) {
    .closed_loop_abort(
      "`now_ns` must be an exact non-negative monotonic nanosecond value",
      "PhysioStream_closed_loop_timing_error"
    )
  }
  now_ns <- as.numeric(now_ns)
  if (!is.null(state$clock$last_ns) &&
      now_ns < state$clock$last_ns) {
    .closed_loop_abort(
      "closed-loop monotonic time decreased",
      "PhysioStream_closed_loop_timing_error"
    )
  }
  list(value = now_ns, mode = mode)
}

.closed_loop_apply_clock <- function(state, clock) {
  state$clock$mode <- clock$mode
  state$clock$last_ns <- clock$value
  state
}

.closed_loop_append_log <- function(state, record) {
  .pipeline_state_bytes(record, "closed-loop log record", 1024^2)
  if (length(state$log) >= state$configuration$log_capacity) {
    state$log <- state$log[-1L]
    state$counters$log_truncated <-
      state$counters$log_truncated + 1
  }
  state$log[[length(state$log) + 1L]] <- record
  ceiling <- .dsp_state_limit - 1024^2
  size <- length(serialize(state, NULL, version = 3L))
  while (size > ceiling && length(state$log) > 1L) {
    state$log <- state$log[-1L]
    state$counters$log_truncated <-
      state$counters$log_truncated + 1
    size <- length(serialize(state, NULL, version = 3L))
  }
  if (size > ceiling) {
    .closed_loop_abort(
      "closed-loop state cannot retain the required audit reserve",
      "PhysioStream_closed_loop_resource_error"
    )
  }
  state
}

.closed_loop_log_base <- function(state, type, now_ns) {
  list(
    schema = .closed_loop_schema,
    type = type,
    controller_generation = state$lifecycle$generation,
    session_sha256 = state$session_sha256,
    monotonic_ns = as.numeric(now_ns),
    clock_domain = state$clock$domain
  )
}

.closed_loop_update_owned <- function(state, controller) {
  pipeline_state <- pipelineState(controller$runtime$pipeline)
  trigger_state <- triggerState(controller$runtime$trigger)
  state$pipeline_state_sha256 <- pipeline_state$sha256
  state$trigger_state_sha256 <- trigger_state$state_sha256
  state$trigger_lifecycle_generation <-
    trigger_state$lifecycle$generation
  state$trigger_arm_generation <- trigger_state$arm$generation
  state
}

.closed_loop_state_unchanged <- function(controller, state) {
  isTRUE(tryCatch(
    identical(
      serialize(controller$state, NULL, version = 3L),
      serialize(state, NULL, version = 3L)
    ),
    error = function(e) FALSE
  ))
}

.closed_loop_commit <- function(controller, old, candidate) {
  if (!.closed_loop_state_unchanged(controller, old)) {
    .closed_loop_abort(
      "controller state changed during a transactional update",
      "PhysioStream_closed_loop_state_error"
    )
  }
  controller$state <- .closed_loop_seal(candidate)
  invisible(controller)
}

.closed_loop_decode_event <- function(event, state) {
  if (!is.list(event) || !identical(event$type, "closed_loop_detection") ||
      !is.character(event$value) || length(event$value) != 1L ||
      is.na(event$value) ||
      nchar(event$value, type = "bytes") >
        .closed_loop_log_limit_bytes) {
    return(NULL)
  }
  value <- tryCatch(
    jsonlite::fromJSON(event$value, simplifyVector = FALSE),
    error = function(e) NULL
  )
  required <- c(
    "schema", "detector", "detector_signature", "detector_event_id",
    "detector_generation", "sample_index", "signal_timestamp", "score"
  )
  if (!is.list(value) || is.object(value) ||
      !all(required %in% names(value)) ||
      !identical(value$schema, .closed_loop_event_schema) ||
      !identical(value$detector, state$configuration$detector$kind) ||
      !identical(value$detector_signature, state$detector_signature) ||
      !is.character(value$detector_event_id) ||
      length(value$detector_event_id) != 1L ||
      !grepl("\\A[0-9a-f]{64}\\z", value$detector_event_id, perl = TRUE) ||
      !is.numeric(value$detector_generation) ||
      length(value$detector_generation) != 1L ||
      value$detector_generation != 1 ||
      !is.numeric(value$sample_index) ||
      length(value$sample_index) != 1L ||
      !is.finite(value$sample_index) ||
      value$sample_index < 1 ||
      value$sample_index != floor(value$sample_index) ||
      !is.numeric(value$signal_timestamp) ||
      length(value$signal_timestamp) != 1L ||
      !is.finite(value$signal_timestamp) ||
      !identical(
        as.numeric(value$signal_timestamp),
        as.numeric(event$timestamp)
      ) ||
      !is.numeric(value$score) || length(value$score) != 1L ||
      !is.finite(value$score)) {
    .closed_loop_abort(
      "committed closed-loop detector event is malformed",
      "PhysioStream_closed_loop_event_error"
    )
  }
  optional_numeric <- c(
    "phase_degrees", "predicted_phase_degrees", "phase_error_degrees",
    "amplitude", "fit"
  )
  for (name in optional_numeric) {
    item <- value[[name]]
    if (!is.null(item) &&
        (!is.numeric(item) || length(item) != 1L ||
         !is.finite(item))) {
      .closed_loop_abort(
        "committed closed-loop detector event has malformed metrics",
        "PhysioStream_closed_loop_event_error"
      )
    }
  }
  value
}

.closed_loop_action_id <- function(state, event) {
  paste0(
    "cl-",
    substr(
      digest::digest(
        paste(
          state$session_sha256,
          event$detector_event_id,
          state$next_action_sequence,
          sep = ":"
        ),
        algo = "sha256", serialize = FALSE
      ),
      1L, 40L
    )
  )
}

.closed_loop_suppress_pending <- function(state, reason, now_ns) {
  if (!length(state$pending)) {
    return(state)
  }
  count <- length(state$pending)
  state$counters$suppressed_stop <-
    state$counters$suppressed_stop + count
  for (item in state$pending) {
    record <- c(
      .closed_loop_log_base(state, "action_suppressed", now_ns),
      list(
        action_id = item$action_id,
        detector_event_id = item$detector_event_id,
        reason = reason,
        due_monotonic_ns = item$due_monotonic_ns
      )
    )
    state <- .closed_loop_append_log(state, record)
  }
  state$pending <- list()
  state
}

.closed_loop_stop_transport <- function(controller, state, now_ns,
                                        emergency, reason) {
  controller_state <- .dsp_deep_copy(controller$state)
  controller_runtime <- .closed_loop_runtime_snapshot(controller)
  trigger <- controller$runtime$trigger
  error_code <- NULL
  trigger_state <- tryCatch(triggerState(trigger), error = function(e) NULL)
  if (!is.null(trigger_state) &&
      identical(trigger_state$lifecycle$status, "open")) {
    if (isTRUE(emergency)) {
      stopped <- tryCatch(
        {
          emergencyStop(trigger, reason = reason, now_ns = now_ns)
          TRUE
        },
        error = function(e) {
          error_code <<- "emergency_stop_failed"
          FALSE
        },
        interrupt = function(e) {
          error_code <<- "emergency_stop_interrupted"
          FALSE
        }
      )
    } else {
      stopped <- tryCatch(
        {
          current <- triggerState(trigger)
          if (identical(current$arm$status, "armed")) {
            disarmTrigger(trigger, reason = reason, now_ns = now_ns)
          }
          TRUE
        },
        error = function(e) {
          error_code <<- "disarm_failed"
          FALSE
        },
        interrupt = function(e) {
          error_code <<- "disarm_interrupted"
          FALSE
        }
      )
    }
    closed <- tryCatch(
      {
        triggerClose(trigger)
        TRUE
      },
      error = function(e) {
        if (is.null(error_code)) {
          error_code <<- "trigger_close_failed"
        }
        FALSE
      },
      interrupt = function(e) {
        if (is.null(error_code)) {
          error_code <<- "trigger_close_interrupted"
        }
        FALSE
      }
    )
  }
  controller_mutated <- !.closed_loop_state_unchanged(
    controller, controller_state
  ) ||
    !.closed_loop_runtime_unchanged(controller, controller_runtime)
  if (controller_mutated) {
    controller$state <- controller_state
    .closed_loop_restore_runtime(controller, controller_runtime)
    if (is.null(error_code)) {
      error_code <- "controller_runtime_mutation"
    }
  }
  state <- .closed_loop_suppress_pending(state, reason, now_ns)
  state$lifecycle$status <- if (isTRUE(emergency)) {
    "emergency_stopped"
  } else {
    "stopped"
  }
  state$lifecycle$stopped_at_ns <- now_ns
  state$lifecycle$stop_reason <- reason
  state$lifecycle$last_error_code <- error_code
  state$counters$stops <- state$counters$stops + 1
  if (isTRUE(emergency)) {
    state$counters$emergency_stops <-
      state$counters$emergency_stops + 1
  }
  controller$runtime$session_id <- NULL
  state <- .closed_loop_update_owned(state, controller)
  state <- .closed_loop_append_log(
    state,
    c(
      .closed_loop_log_base(state, state$lifecycle$status, now_ns),
      list(reason = reason, error_code = error_code)
    )
  )
  state
}

.closed_loop_fail_closed <- function(controller, state, now_ns,
                                     reason, error_code) {
  state$counters$errors <- state$counters$errors + 1
  state <- .closed_loop_stop_transport(
    controller, state, now_ns, emergency = TRUE, reason = reason
  )
  stop_error <- state$lifecycle$last_error_code
  state$lifecycle$status <- "error_stopped"
  state$lifecycle$last_error_code <- if (is.null(stop_error)) {
    error_code
  } else {
    paste0(error_code, ":", stop_error)
  }
  state
}

.closed_loop_enqueue_events <- function(state, events, event_sources,
                                        now_ns, pipeline_sha256) {
  accepted <- list()
  if (!length(events)) {
    if (length(event_sources)) {
      .closed_loop_abort(
        "pipeline event-source identity is malformed",
        "PhysioStream_closed_loop_event_error"
      )
    }
    return(list(state = state, accepted = accepted))
  }
  if (!is.list(event_sources) || is.object(event_sources) ||
      length(event_sources) != length(events)) {
    .closed_loop_abort(
      "pipeline event-source identity is malformed",
      "PhysioStream_closed_loop_event_error"
    )
  }
  governed <- vapply(
    event_sources,
    function(source) {
      is.list(source) && !is.object(source) &&
        identical(
          source$operation_name,
          state$configuration$detector_operation_name
        ) &&
        identical(source$operation_kind, "detector") &&
        identical(source$operation_type, "closed_loop_detector")
    },
    logical(1)
  )
  decoded <- lapply(
    events[governed], .closed_loop_decode_event, state = state
  )
  decoded <- Filter(Negate(is.null), decoded)
  if (!length(decoded)) {
    return(list(state = state, accepted = accepted))
  }
  order_index <- order(
    vapply(decoded, `[[`, numeric(1), "sample_index"),
    vapply(decoded, `[[`, character(1), "detector_event_id"),
    method = "radix"
  )
  decoded <- decoded[order_index]
  for (event in decoded) {
    state$counters$candidates <- state$counters$candidates + 1
    if (event$detector_event_id %in% state$recent_detector_event_ids) {
      state$counters$suppressed_duplicate <-
        state$counters$suppressed_duplicate + 1
      state <- .closed_loop_append_log(
        state,
        c(
          .closed_loop_log_base(state, "detection_suppressed", now_ns),
          list(
            detector_event_id = event$detector_event_id,
            reason = "duplicate"
          )
        )
      )
      next
    }
    if (!is.null(state$last_accepted_monotonic_ns) &&
        now_ns - state$last_accepted_monotonic_ns <
          state$configuration$event_refractory_ns) {
      state$counters$suppressed_refractory <-
        state$counters$suppressed_refractory + 1
      state <- .closed_loop_append_log(
        state,
        c(
          .closed_loop_log_base(state, "detection_suppressed", now_ns),
          list(
            detector_event_id = event$detector_event_id,
            reason = "controller_refractory"
          )
        )
      )
      next
    }
    if (length(state$pending) >=
        state$configuration$pending_capacity) {
      state$counters$suppressed_capacity <-
        state$counters$suppressed_capacity + 1
      return(list(
        state = state, accepted = accepted,
        capacity_error = TRUE
      ))
    }
    action_id <- .closed_loop_action_id(state, event)
    due <- now_ns + state$configuration$delay_ns
    if (!is.finite(due) || due > .closed_loop_max_exact_ns) {
      .closed_loop_abort(
        "closed-loop due time exceeds exact monotonic range",
        "PhysioStream_closed_loop_resource_error"
      )
    }
    action <- list(
      action_id = action_id,
      detector_event_id = event$detector_event_id,
      detector = event$detector,
      detector_generation = event$detector_generation,
      sample_index = event$sample_index,
      signal_timestamp = event$signal_timestamp,
      score = event$score,
      phase_degrees = event$phase_degrees,
      predicted_phase_degrees = event$predicted_phase_degrees,
      phase_error_degrees = event$phase_error_degrees,
      amplitude = event$amplitude,
      fit = event$fit,
      detection_commit_monotonic_ns = now_ns,
      due_monotonic_ns = due,
      pipeline_state_sha256 = pipeline_sha256
    )
    state$next_action_sequence <- state$next_action_sequence + 1
    state$recent_detector_event_ids <- c(
      state$recent_detector_event_ids, event$detector_event_id
    )
    if (length(state$recent_detector_event_ids) >
        state$configuration$log_capacity) {
      state$recent_detector_event_ids <- tail(
        state$recent_detector_event_ids,
        state$configuration$log_capacity
      )
    }
    state$pending[[length(state$pending) + 1L]] <- action
    state$pending <- state$pending[order(
      vapply(state$pending, `[[`, numeric(1), "due_monotonic_ns"),
      vapply(state$pending, `[[`, numeric(1), "sample_index"),
      vapply(state$pending, `[[`, character(1), "action_id"),
      method = "radix"
    )]
    state$last_accepted_monotonic_ns <- now_ns
    state$counters$accepted <- state$counters$accepted + 1
    state$counters$queued <- state$counters$queued + 1
    accepted[[length(accepted) + 1L]] <- action
    state <- .closed_loop_append_log(
      state,
      c(
        .closed_loop_log_base(state, "detection_queued", now_ns),
        action
      )
    )
  }
  list(state = state, accepted = accepted, capacity_error = FALSE)
}

.closed_loop_receipt_summary <- function(receipt, trigger_state) {
  list(
    command_id = receipt$command$command_id,
    status = receipt$status,
    validated_at_ns = receipt$validated_at_ns,
    attempted_at_ns = receipt$attempted_at_ns,
    acknowledged_at_ns = receipt$acknowledged_at_ns,
    transport = receipt$transport,
    ack_code = receipt$ack_code,
    error_class = receipt$error_class,
    error_code = receipt$error_code,
    command_payload_sha256 = receipt$command$payload_sha256,
    trigger_state_sha256 = trigger_state$state_sha256,
    trigger_lifecycle_generation = receipt$lifecycle_generation,
    trigger_arm_generation = receipt$arm_generation
  )
}

.closed_loop_dispatch_due <- function(controller, state, now_ns) {
  receipts <- list()
  while (length(state$pending) &&
         state$pending[[1L]]$due_monotonic_ns <= now_ns &&
         identical(state$lifecycle$status, "running")) {
    if (length(serialize(state, NULL, version = 3L)) >
        .dsp_state_limit - 1024^2) {
      state <- .closed_loop_fail_closed(
        controller, state, now_ns, "state_audit_reserve",
        "state_audit_reserve"
      )
      return(list(state = state, receipts = receipts))
    }
    action <- state$pending[[1L]]
    state$pending <- state$pending[-1L]
    owner_state <- .dsp_deep_copy(controller$state)
    owner_runtime <- .closed_loop_runtime_snapshot(controller)
    result <- tryCatch(
      sendStim(
        controller$runtime$trigger,
        intensity = state$configuration$intensity,
        channel = state$configuration$stim_channel,
        duration_ms = state$configuration$duration_ms,
        command_id = action$action_id,
        now_ns = now_ns
      ),
      error = function(e) e,
      interrupt = function(e) e
    )
    owner_mutated <- !.closed_loop_state_unchanged(
      controller, owner_state
    ) ||
      !.closed_loop_runtime_unchanged(controller, owner_runtime)
    if (owner_mutated) {
      controller$state <- owner_state
      .closed_loop_restore_runtime(controller, owner_runtime)
    }
    trigger_state <- tryCatch(
      triggerState(controller$runtime$trigger),
      error = function(e) NULL
    )
    if (inherits(result, "interrupt")) {
      if (!is.null(trigger_state) && length(trigger_state$audit)) {
        receipt <- tail(trigger_state$audit, 1L)[[1L]]
        summary <- .closed_loop_receipt_summary(receipt, trigger_state)
        state$counters$attempted <- state$counters$attempted + 1
        state$counters$unknown <- state$counters$unknown + 1
        state <- .closed_loop_append_log(
          state,
          c(
            .closed_loop_log_base(state, "action_result", now_ns),
            action,
            summary
          )
        )
      }
      state <- .closed_loop_fail_closed(
        controller, state, now_ns,
        if (owner_mutated) {
          "controller_runtime_mutation"
        } else {
          "transport_interrupt"
        },
        if (owner_mutated) {
          "controller_runtime_mutation"
        } else {
          "transport_interrupt"
        }
      )
      controller$state <- .closed_loop_seal(state)
      stop(result)
    }
    if (inherits(result, "condition")) {
      state <- .closed_loop_fail_closed(
        controller, state, now_ns,
        if (owner_mutated) {
          "controller_runtime_mutation"
        } else {
          "send_failed_before_attempt"
        },
        if (owner_mutated) {
          "controller_runtime_mutation"
        } else {
          "send_failed_before_attempt"
        }
      )
      return(list(state = state, receipts = receipts))
    }
    summary <- .closed_loop_receipt_summary(result, trigger_state)
    state$counters$attempted <- state$counters$attempted + 1
    if (identical(result$status, "acknowledged")) {
      state$counters$acknowledged <-
        state$counters$acknowledged + 1
    } else {
      state$counters$unknown <- state$counters$unknown + 1
    }
    state <- .closed_loop_append_log(
      state,
      c(
        .closed_loop_log_base(state, "action_result", now_ns),
        action,
        summary
      )
    )
    receipts[[length(receipts) + 1L]] <- summary
    state <- .closed_loop_update_owned(state, controller)
    if (owner_mutated) {
      state <- .closed_loop_fail_closed(
        controller, state, now_ns, "controller_runtime_mutation",
        "controller_runtime_mutation"
      )
    } else if (identical(result$status, "unknown")) {
      state <- .closed_loop_fail_closed(
        controller, state, now_ns, "uncertain_delivery",
        "uncertain_delivery"
      )
    }
  }
  list(state = state, receipts = receipts)
}

#' Construct a governed detector-to-stimulation controller
#'
#' `closedLoop()` is side-effect free with respect to the trigger transport. It
#' registers one governed detector operation in the supplied pipeline and owns
#' that graph and trigger for a terminal session lifecycle. Detector events are
#' decoded only after [pipelineStep()] commits; callbacks never stimulate.
#'
#' @param pipeline An empty-queue `StreamPipeline`.
#' @param trigger A closed, disarmed `TriggerBackend`.
#' @param detector An [emgOnsetOp()], [erdIntentOp()], or [phaseTargetOp()]
#'   descriptor.
#' @param intensity,stim_channel,duration_ms Explicit stimulation dose fields.
#' @param delay_ms Detection-commit to attempt delay. Positive delays require a
#'   later step or [closedLoopFlush()]; no sleep or background task is used.
#' @param event_refractory_ms Controller-level detection refractory period.
#' @param pending_capacity Maximum delayed actions.
#' @param log_capacity Maximum retained audit records and detector identities.
#' @param clock Optional deterministic monotonic clock, for loopback tests.
#' @return A closed `ClosedLoopController`.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 2048L)
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' detector <- emgOnsetOp("emg", sampling_rate = 1000,
#'                        baseline_samples = 30, rms_window_samples = 5)
#' controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
#'                          stim_channel = "left", duration_ms = 10)
#' closedLoopState(controller)$lifecycle$status
#' @export
closedLoop <- function(
    pipeline,
    trigger,
    detector,
    intensity,
    stim_channel,
    duration_ms,
    delay_ms = 0,
    event_refractory_ms = 0,
    pending_capacity = 128L,
    log_capacity = 4096L,
    clock = NULL) {
  .pipeline_assert(pipeline)
  .pipeline_require_idle(pipeline)
  .trigger_assert(trigger)
  .trigger_require_idle(trigger)
  if (!inherits(detector, "PipelineOperation") ||
      !identical(detector$kind, "detector") ||
      !identical(detector$descriptor$type, "closed_loop_detector") ||
      !(detector$descriptor$configuration$kind %in%
        c("emg_onset", "erd_intent", "phase_target"))) {
    .closed_loop_abort(
      "`detector` must be a governed closed-loop detector operation",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  pipeline_state <- pipelineState(pipeline)
  if (length(pipeline_state$queue)) {
    .closed_loop_abort(
      "closed-loop construction requires an empty pipeline queue",
      "PhysioStream_closed_loop_state_error"
    )
  }
  if (any(vapply(
      pipeline_state$operations,
      function(x) identical(x$type, "closed_loop_detector"),
      logical(1)
  ))) {
    .closed_loop_abort(
      "pipeline already contains a closed-loop detector",
      "PhysioStream_closed_loop_state_error"
    )
  }
  trigger_state <- triggerState(trigger)
  if (!identical(trigger_state$lifecycle$status, "closed") ||
      !identical(trigger_state$arm$status, "disarmed") ||
      isTRUE(trigger_state$lifecycle$stopped)) {
    .closed_loop_abort(
      "trigger must be closed, disarmed, and not stopped",
      "PhysioStream_closed_loop_state_error"
    )
  }
  stim_channel <- .closed_loop_string(stim_channel, "stim_channel")
  if (!(stim_channel %in%
        trigger_state$configuration$allowed_channels)) {
    .closed_loop_abort(
      "`stim_channel` is not allowed by the trigger",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  intensity <- .closed_loop_scalar(intensity, "intensity", lower = 0)
  if (intensity >
      trigger_state$configuration$max_intensity[[stim_channel]]) {
    .closed_loop_abort(
      "`intensity` exceeds the trigger maximum",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  duration_ms <- .closed_loop_scalar(
    duration_ms, "duration_ms", lower = 0, lower_open = TRUE
  )
  duration_ns <- .closed_loop_ms_to_ns(
    duration_ms, "duration_ms", allow_zero = FALSE
  )
  if (duration_ns >
      trigger_state$configuration$max_duration_ns[[stim_channel]]) {
    .closed_loop_abort(
      "`duration_ms` exceeds the trigger maximum",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  delay_ns <- .closed_loop_ms_to_ns(delay_ms, "delay_ms")
  event_refractory_ns <- .closed_loop_ms_to_ns(
    event_refractory_ms, "event_refractory_ms"
  )
  pending_capacity <- .closed_loop_scalar(
    pending_capacity, "pending_capacity", lower = 1, upper = 16384,
    integer = TRUE
  )
  log_capacity <- .closed_loop_scalar(
    log_capacity, "log_capacity", lower = 1, upper = 16384,
    integer = TRUE
  )
  if (!is.null(clock) && !is.function(clock)) {
    .closed_loop_abort(
      "`clock` must be NULL or a function",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  if (!is.null(clock) && !inherits(trigger, "LoopbackTrigger")) {
    .closed_loop_abort(
      "injected controller clocks are supported only with LoopbackTrigger",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  if (!is.null(trigger$clock) && !identical(trigger$clock, clock)) {
    .closed_loop_abort(
      "controller and trigger must use the same injected clock",
      "PhysioStream_closed_loop_timing_error"
    )
  }

  detector <- .closed_loop_bind_delay(detector, as.numeric(delay_ms))
  onChunk(pipeline, detector)
  bound_pipeline_state <- pipelineState(pipeline)
  configuration <- list(
    detector = detector$state$configuration,
    detector_operation_name = detector$name,
    intensity = intensity,
    stim_channel = stim_channel,
    duration_ms = duration_ms,
    delay_ms = as.numeric(delay_ms),
    delay_ns = delay_ns,
    event_refractory_ms = as.numeric(event_refractory_ms),
    event_refractory_ns = event_refractory_ns,
    pending_capacity = pending_capacity,
    log_capacity = log_capacity,
    research_only = TRUE,
    acknowledgement_is_not_delivery = TRUE
  )
  state <- list(
    configuration = configuration,
    detector_signature = detector$state$signature,
    pipeline_graph_sha256 = .closed_loop_graph_hash(pipeline),
    pipeline_state_sha256 = bound_pipeline_state$sha256,
    trigger_state_sha256 = trigger_state$state_sha256,
    trigger_lifecycle_generation = trigger_state$lifecycle$generation,
    trigger_arm_generation = trigger_state$arm$generation,
    lifecycle = list(
      status = "constructed",
      generation = 0,
      started_at_ns = NULL,
      stopped_at_ns = NULL,
      stop_reason = NULL,
      last_error_code = NULL
    ),
    session_sha256 = NULL,
    clock = list(mode = NULL, domain = NULL, last_ns = NULL),
    pending = list(),
    recent_detector_event_ids = character(),
    last_accepted_monotonic_ns = NULL,
    next_action_sequence = 1,
    counters = .closed_loop_empty_counters(),
    log = list()
  )
  runtime <- new.env(parent = emptyenv())
  runtime$pipeline <- pipeline
  runtime$trigger <- trigger
  runtime$clock <- clock
  runtime$callbacks <- pipeline$callbacks
  runtime$session_id <- NULL
  object <- new.env(parent = emptyenv())
  object$busy <- FALSE
  object$runtime <- runtime
  object$state <- .closed_loop_seal(state)
  class(object) <- c("ClosedLoopController", "ClosedLoopRuntime")
  object
}

#' Run a governed closed-loop session
#'
#' The controller is synchronous and terminal. Start explicitly opens and arms
#' its trigger. Step processes one committed chunk and then dispatches due
#' actions. Flush dispatches due delayed actions without ingesting samples.
#' Stop and emergency stop suppress pending actions and close the trigger.
#'
#' @param controller A `ClosedLoopController`.
#' @param session_id New bounded trigger session identifier.
#' @param samples Finite sample-by-channel chunk.
#' @param timestamps Optional signal-domain timestamps.
#' @param now_ns Optional exact test monotonic nanosecond value.
#' @param reason Bounded non-sensitive stop reason.
#' @return Start/stop functions return `controller` invisibly. Step and flush
#'   return bounded plain result lists.
#' @examples
#' set.seed(1)
#' pipeline <- streamPipeline(chunk_size = 2048L)
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' detector <- emgOnsetOp("emg", sampling_rate = 1000,
#'                        baseline_samples = 30, rms_window_samples = 5)
#' controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
#'                          stim_channel = "left", duration_ms = 10)
#' closedLoopStart(controller, "demo-session", now_ns = 0)
#' emg <- rnorm(100, sd = 0.08)
#' result <- closedLoopStep(controller,
#'   matrix(emg, ncol = 1, dimnames = list(NULL, "emg")),
#'   timestamps = (seq_along(emg) - 1) / 1000, now_ns = 1e6)
#' length(result$actions)
#' closedLoopStop(controller, now_ns = 2e6)
#' @name closed-loop-lifecycle
NULL

#' @rdname closed-loop-lifecycle
#' @export
closedLoopStart <- function(controller, session_id, now_ns = NULL) {
  .closed_loop_assert(controller)
  .closed_loop_require_idle(controller)
  old <- .dsp_deep_copy(controller$state)
  if (!identical(old$lifecycle$status, "constructed")) {
    .closed_loop_abort(
      "only a newly constructed controller can start",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  if (length(pipelineState(controller$runtime$pipeline)$queue)) {
    .closed_loop_abort(
      "controller cannot start with queued pipeline chunks",
      "PhysioStream_closed_loop_state_error"
    )
  }
  session_id <- .closed_loop_string(
    session_id, "session_id", max_bytes = 128L,
    pattern = "\\A[A-Za-z0-9][A-Za-z0-9._:-]{0,127}\\z"
  )
  clock <- .closed_loop_clock(controller, old, now_ns)
  candidate <- .closed_loop_apply_clock(old, clock)
  candidate$clock$domain <- paste0("process_monotonic_", clock$mode)
  controller_runtime <- .closed_loop_runtime_snapshot(controller)
  controller$busy <- TRUE
  on.exit({
    controller$busy <- FALSE
  }, add = TRUE)
  opened <- FALSE
  result <- tryCatch(
    {
      triggerOpen(controller$runtime$trigger)
      opened <- TRUE
      armTrigger(
        controller$runtime$trigger, session_id, now_ns = clock$value
      )
      TRUE
    },
    error = function(e) e,
    interrupt = function(e) e
  )
  if (inherits(result, "condition")) {
    if (!.closed_loop_state_unchanged(controller, old)) {
      controller$state <- old
    }
    if (!.closed_loop_runtime_unchanged(
        controller, controller_runtime)) {
      .closed_loop_restore_runtime(controller, controller_runtime)
    }
    if (opened) {
      try(triggerClose(controller$runtime$trigger), silent = TRUE)
    }
    candidate <- .closed_loop_update_owned(candidate, controller)
    candidate$lifecycle$status <- "error_stopped"
    candidate$lifecycle$stopped_at_ns <- clock$value
    candidate$lifecycle$stop_reason <- "start_failed"
    candidate$lifecycle$last_error_code <- "start_failed"
    candidate$counters$errors <- candidate$counters$errors + 1
    .closed_loop_commit(controller, old, candidate)
    if (inherits(result, "interrupt")) {
      stop(result)
    }
    .closed_loop_abort(
      "closed-loop trigger open/arm failed",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  start_mutated <- !.closed_loop_state_unchanged(controller, old) ||
    !.closed_loop_runtime_unchanged(controller, controller_runtime) ||
    !identical(
      controller$runtime$pipeline$callbacks,
      controller$runtime$callbacks
    ) ||
    !identical(
      .closed_loop_graph_hash(controller$runtime$pipeline),
      old$pipeline_graph_sha256
    ) ||
    !identical(
      pipelineState(controller$runtime$pipeline)$sha256,
      old$pipeline_state_sha256
    )
  if (start_mutated) {
    controller$state <- old
    .closed_loop_restore_runtime(controller, controller_runtime)
    candidate <- .closed_loop_fail_closed(
      controller, candidate, clock$value, "start_runtime_mutation",
      "start_runtime_mutation"
    )
    candidate$pipeline_graph_sha256 <-
      .closed_loop_graph_hash(controller$runtime$pipeline)
    controller$runtime$callbacks <-
      controller$runtime$pipeline$callbacks
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop runtime changed during start; controller stopped",
      "PhysioStream_closed_loop_state_error"
    )
  }
  controller$runtime$session_id <- session_id
  candidate$session_sha256 <- digest::digest(
    paste0("closed-loop-session:", session_id),
    algo = "sha256", serialize = FALSE
  )
  candidate$lifecycle$status <- "running"
  candidate$lifecycle$generation <-
    candidate$lifecycle$generation + 1
  candidate$lifecycle$started_at_ns <- clock$value
  candidate <- .closed_loop_update_owned(candidate, controller)
  candidate <- .closed_loop_append_log(
    candidate,
    c(
      .closed_loop_log_base(candidate, "session_started", clock$value),
      list(
        pipeline_graph_sha256 = candidate$pipeline_graph_sha256,
        detector_signature = candidate$detector_signature,
        trigger_lifecycle_generation =
          candidate$trigger_lifecycle_generation,
        trigger_arm_generation = candidate$trigger_arm_generation
      )
    )
  )
  .closed_loop_commit(controller, old, candidate)
}

.closed_loop_begin_running <- function(controller, now_ns) {
  .closed_loop_assert(controller, integrity = FALSE)
  .closed_loop_require_idle(controller)
  old <- .dsp_deep_copy(controller$state)
  if (!identical(old$lifecycle$status, "running")) {
    .closed_loop_abort(
      "closed-loop operation requires a running controller",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  clock <- .closed_loop_clock(controller, old, now_ns)
  integrity_code <- .closed_loop_integrity_code(controller)
  if (!is.null(integrity_code)) {
    candidate <- .closed_loop_apply_clock(old, clock)
    candidate <- .closed_loop_fail_closed(
      controller, candidate, clock$value, "ownership_integrity",
      integrity_code
    )
    candidate$pipeline_graph_sha256 <-
      .closed_loop_graph_hash(controller$runtime$pipeline)
    controller$runtime$callbacks <-
      controller$runtime$pipeline$callbacks
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop ownership changed; controller stopped",
      "PhysioStream_closed_loop_state_error"
    )
  }
  list(old = old, state = .closed_loop_apply_clock(old, clock), clock = clock)
}

#' @rdname closed-loop-lifecycle
#' @export
closedLoopStep <- function(controller, samples, timestamps = NULL,
                           now_ns = NULL) {
  started <- .closed_loop_begin_running(controller, now_ns)
  old <- started$old
  candidate <- started$state
  now <- started$clock$value
  controller_runtime <- .closed_loop_runtime_snapshot(controller)
  controller$busy <- TRUE
  on.exit({
    controller$busy <- FALSE
  }, add = TRUE)

  heartbeat <- tryCatch(
    {
      triggerHeartbeat(controller$runtime$trigger, now_ns = now)
      TRUE
    },
    error = function(e) e,
    interrupt = function(e) e
  )
  if (inherits(heartbeat, "condition")) {
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "heartbeat_failed", "heartbeat_failed"
    )
    .closed_loop_commit(controller, old, candidate)
    if (inherits(heartbeat, "interrupt")) {
      stop(heartbeat)
    }
    .closed_loop_abort(
      "closed-loop trigger heartbeat failed",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  candidate <- .closed_loop_update_owned(candidate, controller)
  due_before <- .closed_loop_dispatch_due(controller, candidate, now)
  candidate <- due_before$state
  receipts <- due_before$receipts
  if (!identical(candidate$lifecycle$status, "running")) {
    .closed_loop_commit(controller, old, candidate)
    return(list(
      pipeline = NULL,
      detections = list(),
      actions = receipts,
      state_sha256 = controller$state$sha256,
      schema = .closed_loop_schema
    ))
  }

  processed <- tryCatch(
    {
      pipelineEnqueue(
        controller$runtime$pipeline, samples, timestamps
      )
      pipelineStep(controller$runtime$pipeline, n = 1L)
    },
    error = function(e) e,
    interrupt = function(e) e
  )
  if (inherits(processed, "condition")) {
    if (!.closed_loop_state_unchanged(controller, old)) {
      controller$state <- old
    }
    if (!.closed_loop_runtime_unchanged(
        controller, controller_runtime)) {
      .closed_loop_restore_runtime(controller, controller_runtime)
    }
    candidate$pipeline_state_sha256 <-
      pipelineState(controller$runtime$pipeline)$sha256
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "pipeline_failed", "pipeline_failed"
    )
    .closed_loop_commit(controller, old, candidate)
    if (inherits(processed, "interrupt")) {
      stop(processed)
    }
    .closed_loop_abort(
      "closed-loop pipeline processing failed before stimulation",
      "PhysioStream_closed_loop_pipeline_error"
    )
  }
  if (!.closed_loop_state_unchanged(controller, old) ||
      !.closed_loop_runtime_unchanged(controller, controller_runtime) ||
      !identical(
        controller$runtime$pipeline$callbacks,
        controller$runtime$callbacks
      ) ||
      !identical(
        .closed_loop_graph_hash(controller$runtime$pipeline),
        old$pipeline_graph_sha256
      )) {
    controller$state <- old
    .closed_loop_restore_runtime(controller, controller_runtime)
    candidate$pipeline_state_sha256 <-
      pipelineState(controller$runtime$pipeline)$sha256
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "runtime_mutation", "runtime_mutation"
    )
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop runtime changed during pipeline commit",
      "PhysioStream_closed_loop_state_error"
    )
  }
  current_trigger <- triggerState(controller$runtime$trigger)
  if (!identical(
      current_trigger$state_sha256,
      candidate$trigger_state_sha256
  )) {
    candidate$pipeline_state_sha256 <-
      pipelineState(controller$runtime$pipeline)$sha256
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "trigger_mutation", "trigger_mutation"
    )
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "trigger changed during pipeline processing",
      "PhysioStream_closed_loop_state_error"
    )
  }
  if (!identical(processed$n_processed, 1L) ||
      length(processed$results) != 1L) {
    candidate$pipeline_state_sha256 <- processed$state_sha256
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "pipeline_commit_missing",
      "pipeline_commit_missing"
    )
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "pipeline did not return exactly one committed chunk",
      "PhysioStream_closed_loop_pipeline_error"
    )
  }
  candidate$pipeline_state_sha256 <- processed$state_sha256
  queued <- tryCatch(
    .closed_loop_enqueue_events(
      candidate,
      processed$results[[1L]]$events,
      processed$results[[1L]]$event_sources,
      now,
      processed$state_sha256
    ),
    error = function(e) e
  )
  if (inherits(queued, "condition")) {
    failure_code <- if (inherits(
        queued, "PhysioStream_closed_loop_resource_error")) {
      "event_queue_resource"
    } else {
      "malformed_detector_event"
    }
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, failure_code, failure_code
    )
    .closed_loop_commit(controller, old, candidate)
    stop(queued)
  }
  candidate <- queued$state
  if (isTRUE(queued$capacity_error)) {
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "pending_capacity",
      "pending_capacity"
    )
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop pending capacity was exceeded",
      "PhysioStream_closed_loop_resource_error"
    )
  }
  due_after <- .closed_loop_dispatch_due(controller, candidate, now)
  candidate <- due_after$state
  receipts <- c(receipts, due_after$receipts)
  candidate <- .closed_loop_update_owned(candidate, controller)
  .closed_loop_commit(controller, old, candidate)
  list(
    pipeline = processed,
    detections = queued$accepted,
    actions = receipts,
    state_sha256 = controller$state$sha256,
    schema = .closed_loop_schema
  )
}

#' @rdname closed-loop-lifecycle
#' @export
closedLoopFlush <- function(controller, now_ns = NULL) {
  started <- .closed_loop_begin_running(controller, now_ns)
  old <- started$old
  candidate <- started$state
  now <- started$clock$value
  controller$busy <- TRUE
  on.exit({
    controller$busy <- FALSE
  }, add = TRUE)
  heartbeat <- tryCatch(
    {
      triggerHeartbeat(controller$runtime$trigger, now_ns = now)
      TRUE
    },
    error = function(e) e,
    interrupt = function(e) e
  )
  if (inherits(heartbeat, "condition")) {
    candidate <- .closed_loop_fail_closed(
      controller, candidate, now, "heartbeat_failed", "heartbeat_failed"
    )
    .closed_loop_commit(controller, old, candidate)
    if (inherits(heartbeat, "interrupt")) {
      stop(heartbeat)
    }
    .closed_loop_abort(
      "closed-loop trigger heartbeat failed",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  candidate <- .closed_loop_update_owned(candidate, controller)
  due <- .closed_loop_dispatch_due(controller, candidate, now)
  candidate <- .closed_loop_update_owned(due$state, controller)
  .closed_loop_commit(controller, old, candidate)
  list(
    actions = due$receipts,
    n_pending = length(controller$state$pending),
    state_sha256 = controller$state$sha256,
    schema = .closed_loop_schema
  )
}

#' @rdname closed-loop-lifecycle
#' @export
closedLoopStop <- function(controller, reason = "caller",
                           now_ns = NULL) {
  .closed_loop_assert(controller, integrity = FALSE)
  .closed_loop_require_idle(controller)
  old <- .dsp_deep_copy(controller$state)
  if (old$lifecycle$status %in%
      c("stopped", "emergency_stopped", "error_stopped")) {
    return(invisible(controller))
  }
  reason <- .closed_loop_string(reason, "reason", max_bytes = 128L)
  clock <- .closed_loop_clock(controller, old, now_ns)
  candidate <- .closed_loop_apply_clock(old, clock)
  integrity_code <- .closed_loop_integrity_code(controller)
  if (!is.null(integrity_code)) {
    candidate <- .closed_loop_fail_closed(
      controller, candidate, clock$value, "ownership_integrity",
      integrity_code
    )
    candidate$pipeline_graph_sha256 <-
      .closed_loop_graph_hash(controller$runtime$pipeline)
    controller$runtime$callbacks <-
      controller$runtime$pipeline$callbacks
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop ownership changed; controller stopped",
      "PhysioStream_closed_loop_state_error"
    )
  }
  controller$busy <- TRUE
  on.exit({
    controller$busy <- FALSE
  }, add = TRUE)
  candidate <- .closed_loop_stop_transport(
    controller, candidate, clock$value, emergency = FALSE, reason = reason
  )
  stop_failed <- !is.null(candidate$lifecycle$last_error_code)
  if (stop_failed) {
    candidate$lifecycle$status <- "error_stopped"
    candidate$counters$errors <- candidate$counters$errors + 1
  }
  .closed_loop_commit(controller, old, candidate)
  if (stop_failed) {
    .closed_loop_abort(
      "closed-loop stop completed with a deassert/close failure",
      "PhysioStream_closed_loop_transport_error"
    )
  }
}

#' @rdname closed-loop-lifecycle
#' @export
closedLoopEmergencyStop <- function(controller, reason = "caller",
                                    now_ns = NULL) {
  .closed_loop_assert(controller, integrity = FALSE)
  .closed_loop_require_idle(controller)
  old <- .dsp_deep_copy(controller$state)
  if (old$lifecycle$status %in%
      c("stopped", "emergency_stopped", "error_stopped")) {
    return(invisible(controller))
  }
  reason <- .closed_loop_string(reason, "reason", max_bytes = 128L)
  clock <- .closed_loop_clock(controller, old, now_ns)
  candidate <- .closed_loop_apply_clock(old, clock)
  integrity_code <- .closed_loop_integrity_code(controller)
  if (!is.null(integrity_code)) {
    candidate <- .closed_loop_fail_closed(
      controller, candidate, clock$value, "ownership_integrity",
      integrity_code
    )
    candidate$pipeline_graph_sha256 <-
      .closed_loop_graph_hash(controller$runtime$pipeline)
    controller$runtime$callbacks <-
      controller$runtime$pipeline$callbacks
    .closed_loop_commit(controller, old, candidate)
    .closed_loop_abort(
      "closed-loop ownership changed; controller stopped",
      "PhysioStream_closed_loop_state_error"
    )
  }
  controller$busy <- TRUE
  on.exit({
    controller$busy <- FALSE
  }, add = TRUE)
  candidate <- .closed_loop_stop_transport(
    controller, candidate, clock$value, emergency = TRUE, reason = reason
  )
  stop_failed <- !is.null(candidate$lifecycle$last_error_code)
  if (stop_failed) {
    candidate$lifecycle$status <- "error_stopped"
    candidate$counters$errors <- candidate$counters$errors + 1
  }
  .closed_loop_commit(controller, old, candidate)
  if (stop_failed) {
    .closed_loop_abort(
      "closed-loop emergency stop completed with a deassert/close failure",
      "PhysioStream_closed_loop_transport_error"
    )
  }
}

#' Inspect governed closed-loop state and audit
#'
#' Returned values are deep portable copies. They contain no runtime object,
#' trigger credential, patient identifier, or raw physiological samples.
#'
#' @param controller A `ClosedLoopController`.
#' @return `closedLoopState()` returns the sealed state; `closedLoopLog()`
#'   returns its bounded structured session log.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 2048L)
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' detector <- emgOnsetOp("emg", sampling_rate = 1000,
#'                        baseline_samples = 30, rms_window_samples = 5)
#' controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
#'                          stim_channel = "left", duration_ms = 10)
#' closedLoopState(controller)$lifecycle$status
#' closedLoopLog(controller)
#' @name closed-loop-state
NULL

#' @rdname closed-loop-state
#' @export
closedLoopState <- function(controller) {
  .closed_loop_assert(controller)
  .dsp_deep_copy(controller$state)
}

#' @rdname closed-loop-state
#' @export
closedLoopLog <- function(controller) {
  .closed_loop_assert(controller)
  .dsp_deep_copy(controller$state$log)
}

#' Append a stopped closed-loop session to PhysioExperiment provenance
#'
#' The bounded log records detection, queue, and acknowledgement evidence.
#' Acknowledgement is explicitly not a physical-delivery claim. Raw samples
#' and trigger runtime values are excluded.
#'
#' @param controller A stopped `ClosedLoopController`.
#' @param x A `PhysioExperiment`.
#' @param input_assay Exact existing assay name or `NA_character_`.
#' @return A modified `PhysioExperiment` with one provenance activity.
#' @examples
#' pipeline <- streamPipeline(chunk_size = 2048L)
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' detector <- emgOnsetOp("emg", sampling_rate = 1000,
#'                        baseline_samples = 30, rms_window_samples = 5)
#' controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
#'                          stim_channel = "left", duration_ms = 10)
#' closedLoopStart(controller, "demo-session", now_ns = 0)
#' closedLoopStep(controller,
#'   matrix(rnorm(100, sd = 0.08), ncol = 1, dimnames = list(NULL, "emg")),
#'   now_ns = 1e6)
#' closedLoopStop(controller, now_ns = 2e6)
#' pe <- PhysioExperiment(assays = list(raw = matrix(as.double(1:20), 10, 2)),
#'                        samplingRate = 100)
#' result <- closedLoopProvenance(controller, pe, "raw")
#' length(S4Vectors::metadata(result)$provenance)
#' @export
closedLoopProvenance <- function(controller, x,
                                 input_assay = NA_character_) {
  .closed_loop_assert(controller)
  state <- .dsp_deep_copy(controller$state)
  if (!(state$lifecycle$status %in%
        c("stopped", "emergency_stopped", "error_stopped"))) {
    .closed_loop_abort(
      "provenance requires a stopped controller",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  if (is.null(state$session_sha256) ||
      state$lifecycle$generation < 1) {
    .closed_loop_abort(
      "provenance requires a session that was started",
      "PhysioStream_closed_loop_lifecycle_error"
    )
  }
  if (!methods::is(x, "PhysioExperiment")) {
    .closed_loop_abort(
      "`x` must be a PhysioExperiment",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  valid_assay <- is.character(input_assay) &&
    is.null(dim(input_assay)) && length(input_assay) == 1L &&
    (is.na(input_assay) ||
     (!is.factor(input_assay) && !is.object(input_assay) &&
      nzchar(input_assay)))
  if (!valid_assay) {
    .closed_loop_abort(
      "`input_assay` must be one assay name or NA_character_",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  if (!is.na(input_assay) &&
      !(input_assay %in% SummarizedExperiment::assayNames(x))) {
    .closed_loop_abort(
      "`input_assay` does not identify an existing assay",
      "PhysioStream_closed_loop_validation_error"
    )
  }
  entries <- S4Vectors::metadata(x)[["provenance"]]
  if (is.null(entries)) {
    entries <- list()
  }
  duplicate <- any(vapply(
    entries,
    function(entry) {
      is.list(entry) && is.list(entry$params) &&
        identical(entry$params$session_sha256, state$session_sha256)
    },
    logical(1)
  ))
  if (duplicate) {
    .closed_loop_abort(
      "this closed-loop session is already in provenance",
      "PhysioStream_closed_loop_provenance_error"
    )
  }
  log_sha256 <- digest::digest(
    serialize(state$log, NULL, version = 3L),
    algo = "sha256", serialize = FALSE
  )
  params <- list(
    schema = .closed_loop_schema,
    session_sha256 = state$session_sha256,
    detector = state$configuration$detector,
    stimulation = list(
      intensity = state$configuration$intensity,
      channel = state$configuration$stim_channel,
      duration_ms = state$configuration$duration_ms,
      delay_ms = state$configuration$delay_ms,
      event_refractory_ms =
        state$configuration$event_refractory_ms
    ),
    clock_domain = state$clock$domain,
    started_at_monotonic_ns = state$lifecycle$started_at_ns,
    stopped_at_monotonic_ns = state$lifecycle$stopped_at_ns,
    stop_reason = state$lifecycle$stop_reason,
    controller_generation = state$lifecycle$generation,
    trigger_lifecycle_generation =
      state$trigger_lifecycle_generation,
    trigger_arm_generation = state$trigger_arm_generation,
    counters = state$counters,
    session_log = state$log,
    session_log_sha256 = log_sha256,
    pipeline_state_sha256 = state$pipeline_state_sha256,
    trigger_state_sha256 = state$trigger_state_sha256,
    research_only = TRUE,
    acknowledgement_is_not_delivery = TRUE
  )
  .pipeline_state_bytes(params, "closed-loop provenance", .dsp_state_limit)
  PhysioExperiment::appendProvenance(
    x,
    activity = "closed_loop_session",
    params = params,
    input_assay = input_assay,
    output_assay = NA_character_,
    package = "PhysioStream",
    software_version = as.character(utils::packageVersion("PhysioStream"))
  )
}

#' @export
print.ClosedLoopController <- function(x, ...) {
  .closed_loop_assert(x)
  cat(sprintf(
    "<ClosedLoopController: %s, detector=%s, pending=%d, attempted=%s, unknown=%s>\n",
    x$state$lifecycle$status,
    x$state$configuration$detector$kind,
    length(x$state$pending),
    x$state$counters$attempted,
    x$state$counters$unknown
  ))
  invisible(x)
}
