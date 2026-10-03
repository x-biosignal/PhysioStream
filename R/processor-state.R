.dsp_schema_version <- "1.0.0"
.dsp_state_limit <- 16 * 1024^2
.dsp_allocation_limit <- 512 * 1024^2

.dsp_abort <- function(message, class = "PhysioStream_dsp_error") {
  .stream_abort(message, class)
}

.dsp_scalar <- function(x, name, lower = -Inf, upper = Inf,
                        integer = FALSE, lower_open = FALSE,
                        upper_open = FALSE) {
  valid <- !is.factor(x) && is.numeric(x) && length(x) == 1L &&
    is.finite(x) && if (lower_open) x > lower else x >= lower
  valid <- valid && if (upper_open) x < upper else x <= upper
  if (integer) {
    valid <- valid && x == floor(x) && x <= .Machine$integer.max
  }
  if (!valid) {
    .dsp_abort(
      sprintf("`%s` has an invalid value", name),
      "PhysioStream_dsp_validation_error"
    )
  }
  if (integer) as.integer(x) else as.numeric(x)
}

.dsp_enum <- function(x, choices, name) {
  if (is.factor(x) || !is.character(x) || length(x) != 1L ||
      is.na(x) || !(x %in% choices)) {
    .dsp_abort(
      sprintf("`%s` must be exactly one of: %s", name,
              paste(sprintf("'%s'", choices), collapse = ", ")),
      "PhysioStream_dsp_validation_error"
    )
  }
  x
}

.dsp_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .dsp_abort(
      sprintf("`%s` must be one non-missing logical value", name),
      "PhysioStream_dsp_validation_error"
    )
  }
  x
}

.dsp_deep_copy <- function(x) {
  unserialize(serialize(x, NULL, version = 3L))
}

.dsp_runtime_path <- function(x, path = "state") {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.environment(x) || is.function(x) || inherits(x, "connection") ||
      typeof(x) == "externalptr" || inherits(x, "python.builtin.object")) {
    return(path)
  }
  if (is.numeric(x) || is.complex(x)) {
    if (any(!is.finite(Re(x))) || any(!is.finite(Im(x)))) {
      return(path)
    }
  }
  if (is.pairlist(x) || is.language(x)) {
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
      bad <- .dsp_runtime_path(x[[i]], paste0(path, "$", label))
      if (!is.null(bad)) {
        return(bad)
      }
    }
  }
  NULL
}

.dsp_hash_state <- function(state) {
  payload <- state
  payload$sha256 <- NULL
  digest::digest(
    serialize(payload, NULL, version = 3L),
    algo = "sha256",
    serialize = FALSE
  )
}

.dsp_seal_state <- function(state) {
  state$schema <- .dsp_schema_version
  state$sha256 <- NULL
  bad <- .dsp_runtime_path(state)
  if (!is.null(bad)) {
    .dsp_abort(
      sprintf("processor state contains an unsupported value at `%s`", bad),
      "PhysioStream_dsp_state_error"
    )
  }
  size <- length(serialize(state, NULL, version = 3L))
  if (size > .dsp_state_limit) {
    .dsp_abort(
      "processor state exceeds the 16 MiB serialization ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  state$sha256 <- .dsp_hash_state(state)
  if (length(serialize(state, NULL, version = 3L)) > .dsp_state_limit) {
    .dsp_abort(
      "processor state exceeds the 16 MiB serialization ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  state
}

.dsp_validate_state <- function(state) {
  if (!is.list(state) || is.object(state) ||
      !identical(state$schema, .dsp_schema_version) ||
      !is.character(state$sha256) || length(state$sha256) != 1L ||
      is.na(state$sha256) || !nzchar(state$sha256)) {
    .dsp_abort(
      "processor state has an invalid schema",
      "PhysioStream_dsp_state_error"
    )
  }
  bad <- .dsp_runtime_path(state)
  if (!is.null(bad)) {
    .dsp_abort(
      sprintf("processor state contains an unsupported value at `%s`", bad),
      "PhysioStream_dsp_state_error"
    )
  }
  if (length(serialize(state, NULL, version = 3L)) > .dsp_state_limit) {
    .dsp_abort(
      "processor state exceeds the 16 MiB serialization ceiling",
      "PhysioStream_dsp_resource_error"
    )
  }
  expected <- .dsp_hash_state(state)
  if (!identical(state$sha256, expected)) {
    .dsp_abort(
      "processor state hash does not match its payload",
      "PhysioStream_dsp_state_error"
    )
  }
  invisible(TRUE)
}

.dsp_new_processor <- function(class, state, resetter) {
  object <- new.env(parent = emptyenv())
  object$state <- .dsp_seal_state(state)
  object$resetter <- resetter
  class(object) <- c(class, "StreamProcessor")
  object
}

.dsp_assert_processor <- function(object) {
  if (!inherits(object, "StreamProcessor") || !is.environment(object)) {
    .dsp_abort(
      "`object` must be a StreamProcessor",
      "PhysioStream_dsp_validation_error"
    )
  }
  .dsp_validate_state(object$state)
  invisible(TRUE)
}

#' Inspect or reset governed streaming processor state
#'
#' `processorState()` returns a deep, portable copy of the numeric processor
#' state. `processorReset()` clears learned and delay state, increments the
#' reset counter, and can retain the bound channel identity.
#'
#' @param object A `StreamProcessor`.
#' @param keep_channels Whether to retain channel identity across reset.
#' @param ... Reserved for methods.
#' @return `processorState()` returns a plain list. `processorReset()` returns
#'   `object` invisibly.
#' @examples
#' filt <- lmsFilter(n_taps = 3L, step_size = 0.05)
#' update(filt, matrix(1:8, 4L, 2L), reference = matrix(1:4, 4L, 1L))
#' processorState(filt)$n_samples
#' processorReset(filt)
#' processorState(filt)$n_samples
#' @export
processorState <- function(object, ...) {
  UseMethod("processorState")
}

#' @export
processorState.StreamProcessor <- function(object, ...) {
  .dsp_assert_processor(object)
  .dsp_deep_copy(object$state)
}

#' @rdname processorState
#' @export
processorReset <- function(object, keep_channels = TRUE, ...) {
  UseMethod("processorReset")
}

#' @export
processorReset.StreamProcessor <- function(object, keep_channels = TRUE, ...) {
  .dsp_assert_processor(object)
  keep_channels <- .dsp_logical(keep_channels, "keep_channels")
  resetter <- object$resetter
  if (!is.function(resetter)) {
    .dsp_abort(
      "processor reset implementation is unavailable",
      "PhysioStream_dsp_state_error"
    )
  }
  candidate <- resetter(.dsp_deep_copy(object$state), keep_channels)
  candidate <- .dsp_seal_state(candidate)
  runtime_resetter <- object$runtime_resetter
  if (is.function(runtime_resetter)) {
    runtime_state <- runtime_resetter(keep_channels)
    if (!is.null(runtime_state)) {
      candidate$runtime_state <- runtime_state
      candidate <- .dsp_seal_state(candidate)
    }
  }
  object$state <- candidate
  invisible(object)
}

#' @export
print.StreamProcessor <- function(x, ...) {
  .dsp_assert_processor(x)
  cat(sprintf(
    "<%s: samples=%s, chunks=%s, resets=%s>\n",
    class(x)[[1L]], x$state$n_samples, x$state$n_chunks,
    x$state$reset_count
  ))
  invisible(x)
}

.dsp_normalize_matrix <- function(x, name = "samples") {
  if (is.factor(x) || !is.numeric(x) || is.object(x)) {
    .dsp_abort(
      sprintf("`%s` must be a plain real numeric vector or matrix", name),
      "PhysioStream_dsp_validation_error"
    )
  }
  if (is.null(dim(x))) {
    x <- matrix(as.numeric(x), ncol = 1L)
  } else if (length(dim(x)) == 2L) {
    x <- matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x),
                dimnames = dimnames(x))
  } else {
    .dsp_abort(
      sprintf("`%s` must have exactly two dimensions", name),
      "PhysioStream_dsp_validation_error"
    )
  }
  if (ncol(x) < 1L || any(!is.finite(x))) {
    .dsp_abort(
      sprintf("`%s` must have at least one column and finite values", name),
      "PhysioStream_dsp_validation_error"
    )
  }
  bytes <- as.double(length(x)) * 8
  if (!is.finite(bytes) || bytes > .dsp_allocation_limit) {
    .dsp_abort(
      sprintf("`%s` exceeds the 512 MiB allocation ceiling", name),
      "PhysioStream_dsp_resource_error"
    )
  }
  x
}

.dsp_channel_names <- function(x) {
  current <- colnames(x)
  if (is.null(current)) {
    return(sprintf("channel_%d", seq_len(ncol(x))))
  }
  if (length(current) != ncol(x) || anyNA(current) ||
      any(!nzchar(current)) || anyDuplicated(current)) {
    .dsp_abort(
      "sample column names must be unique non-empty strings",
      "PhysioStream_dsp_validation_error"
    )
  }
  current
}

.dsp_validate_chunk <- function(state, samples, timestamps = NULL) {
  samples <- .dsp_normalize_matrix(samples)
  n <- nrow(samples)
  if (n == 0L) {
    if (!is.null(timestamps) &&
        (!is.numeric(timestamps) || length(timestamps) != 0L)) {
      .dsp_abort(
        "`timestamps` must be empty for an empty chunk",
        "PhysioStream_dsp_validation_error"
      )
    }
    return(list(
      samples = samples, timestamps = numeric(), names = NULL, empty = TRUE
    ))
  }
  channel_names <- .dsp_channel_names(samples)
  if (!is.null(state$channel_names) &&
      (!identical(ncol(samples), length(state$channel_names)) ||
       !identical(channel_names, state$channel_names))) {
    .dsp_abort(
      "sample channel count, order, or names changed",
      "PhysioStream_dsp_channel_error"
    )
  }
  if (is.null(timestamps)) {
    timestamps <- NULL
  } else {
    if (is.factor(timestamps) || !is.numeric(timestamps) ||
        !is.null(dim(timestamps)) || length(timestamps) != n ||
        any(!is.finite(timestamps)) ||
        (n > 1L && any(diff(timestamps) <= 0))) {
      .dsp_abort(
        "`timestamps` must be a finite strictly increasing row-matched vector",
        "PhysioStream_dsp_timestamp_error"
      )
    }
    timestamps <- as.numeric(timestamps)
    if (!is.null(state$last_timestamp) &&
        timestamps[[1L]] <= state$last_timestamp) {
      .dsp_abort(
        "timestamps must increase across chunks",
        "PhysioStream_dsp_timestamp_error"
      )
    }
  }
  next_count <- as.double(state$n_samples) + n
  if (!is.finite(next_count) || next_count > 2^53) {
    .dsp_abort(
      "processor sample sequence would overflow exact double integers",
      "PhysioStream_dsp_resource_error"
    )
  }
  list(
    samples = samples,
    timestamps = timestamps,
    names = channel_names,
    empty = FALSE
  )
}

.dsp_empty_result <- function(state, n_channels = NULL) {
  if (is.null(n_channels)) {
    n_channels <- if (is.null(state$channel_names)) 0L
      else length(state$channel_names)
  }
  list(
    output = matrix(numeric(), nrow = 0L, ncol = n_channels),
    timestamps = numeric(),
    n_input = 0L,
    n_emitted = 0L,
    sequence_start = numeric(),
    sequence_end = numeric(),
    state_sha256 = state$sha256,
    diagnostics = list(),
    schema = .dsp_schema_version
  )
}

.dsp_commit <- function(object, old_state, new_state, output, timestamps,
                        n_input, n_emitted, diagnostics,
                        sequence_start = NULL, sequence_end = NULL) {
  if (!identical(object$state$sha256, old_state$sha256)) {
    .dsp_abort(
      "processor state changed during update",
      "PhysioStream_dsp_state_error"
    )
  }
  new_state <- .dsp_seal_state(new_state)
  if (is.null(sequence_start)) {
    sequence_start <- if (n_input) old_state$n_samples + 1 else numeric()
  }
  if (is.null(sequence_end)) {
    sequence_end <- if (n_input) old_state$n_samples + n_input else numeric()
  }
  result <- list(
    output = output,
    timestamps = timestamps,
    n_input = as.integer(n_input),
    n_emitted = as.integer(n_emitted),
    sequence_start = as.numeric(sequence_start),
    sequence_end = as.numeric(sequence_end),
    state_sha256 = new_state$sha256,
    diagnostics = diagnostics,
    schema = .dsp_schema_version
  )
  bad <- .dsp_runtime_path(result, "result")
  if (!is.null(bad)) {
    .dsp_abort(
      sprintf("processor output contains an unsupported value at `%s`", bad),
      "PhysioStream_dsp_state_error"
    )
  }
  object$state <- new_state
  result
}
