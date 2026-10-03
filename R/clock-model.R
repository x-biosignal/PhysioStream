.clock_schema_version <- "1.0.0"
.clock_allocation_limit <- 512 * 1024^2
.clock_segment_columns <- c(
  "segment", "observation_start", "observation_end",
  "device_start", "device_end", "intercept", "drift", "drift_ppm",
  "rmse", "mad", "max_abs", "n_inlier", "n_total"
)
.clock_model_fields <- c(
  "schema", "method", "origin", "origin_rule", "segments",
  "sign_convention", "input_domain", "quality", "inliers", "fit_details",
  "parameters", "input_sha256", "evidence_sha256"
)

.clock_enum <- function(x, choices, name) {
  if (is.factor(x) || !is.character(x) || length(x) != 1L ||
      is.na(x) || !(x %in% choices)) {
    .stream_abort(
      sprintf("`%s` must be exactly one of: %s", name,
              paste(sprintf("'%s'", choices), collapse = ", ")),
      "PhysioStream_clock_validation_error"
    )
  }
  x
}

.clock_scalar_number <- function(x, name, lower = -Inf, upper = Inf,
                                 integer = FALSE) {
  if (is.factor(x) || !is.numeric(x) || length(x) != 1L ||
      !is.finite(x) || x < lower || x > upper ||
      (integer && x != floor(x))) {
    .stream_abort(
      sprintf("`%s` must be one finite %s in [%s, %s]", name,
              if (integer) "integer" else "number", lower, upper),
      "PhysioStream_clock_validation_error"
    )
  }
  if (integer) as.integer(x) else as.numeric(x)
}

.clock_numeric_vector <- function(x, name, finite = TRUE) {
  if (is.factor(x) || !is.numeric(x) || !is.vector(x)) {
    .stream_abort(
      sprintf("`%s` must be a plain numeric vector", name),
      "PhysioStream_clock_validation_error"
    )
  }
  x <- as.numeric(x)
  if (finite && any(!is.finite(x))) {
    .stream_abort(
      sprintf("`%s` must contain only finite values", name),
      "PhysioStream_clock_validation_error"
    )
  }
  x
}

.clock_normalize_segments <- function(segments, n, name = "segments") {
  n <- .clock_scalar_number(
    n, "segment sample count", 0, .Machine$integer.max, integer = TRUE
  )
  if (is.null(segments)) {
    labels <- if (n) rep.int(1L, n) else integer()
    ranges <- if (n) {
      matrix(c(0L, n - 1L), nrow = 1L,
             dimnames = list(NULL, c("start", "end")))
    } else {
      matrix(integer(), nrow = 0L, ncol = 2L,
             dimnames = list(NULL, c("start", "end")))
    }
    return(list(labels = labels, ranges = ranges))
  }

  if (is.data.frame(segments)) {
    segments <- as.matrix(segments)
  }
  if (is.matrix(segments)) {
    if (ncol(segments) != 2L || anyNA(segments) ||
        !is.numeric(segments) || any(!is.finite(segments)) ||
        any(segments != floor(segments))) {
      .stream_abort(
        sprintf("`%s` ranges must be a finite two-column integer matrix", name),
        "PhysioStream_clock_segment_error"
      )
    }
    ranges <- matrix(as.integer(segments), ncol = 2L)
    colnames(ranges) <- c("start", "end")
    if (!n) {
      if (nrow(ranges)) {
        .stream_abort(
          sprintf("`%s` must be empty when the input is empty", name),
          "PhysioStream_clock_segment_error"
        )
      }
      return(list(labels = integer(), ranges = ranges))
    }
    valid <- nrow(ranges) > 0L &&
      ranges[1L, 1L] == 0L &&
      ranges[nrow(ranges), 2L] == n - 1L &&
      all(ranges[, 1L] >= 0L) &&
      all(ranges[, 2L] >= ranges[, 1L])
    if (valid && nrow(ranges) > 1L) {
      valid <- all(ranges[-1L, 1L] ==
                     ranges[-nrow(ranges), 2L] + 1L)
    }
    if (!valid) {
      .stream_abort(
        sprintf("`%s` ranges must exactly and contiguously partition the input",
                name),
        "PhysioStream_clock_segment_error"
      )
    }
    labels <- integer(n)
    for (i in seq_len(nrow(ranges))) {
      labels[seq.int(ranges[i, 1L] + 1L, ranges[i, 2L] + 1L)] <- i
    }
    return(list(labels = labels, ranges = ranges))
  }

  if (is.factor(segments) || !is.numeric(segments) ||
      !is.vector(segments) || length(segments) != n ||
      anyNA(segments) || any(!is.finite(segments)) ||
      any(segments != floor(segments)) || any(segments < 1) ||
      any(segments > .Machine$integer.max)) {
    .stream_abort(
      sprintf("`%s` labels must be positive integers matching the input", name),
      "PhysioStream_clock_segment_error"
    )
  }
  labels <- as.integer(segments)
  runs <- rle(labels)$values
  if (!identical(runs, seq_along(runs))) {
    .stream_abort(
      sprintf("`%s` labels must be contiguous, ordered, and non-recurring",
              name),
      "PhysioStream_clock_segment_error"
    )
  }
  ends <- cumsum(rle(labels)$lengths) - 1L
  starts <- c(0L, utils::head(ends, -1L) + 1L)
  ranges <- cbind(start = as.integer(starts), end = as.integer(ends))
  list(labels = labels, ranges = ranges)
}

.clock_ols <- function(x, y, weights = NULL) {
  design <- cbind(intercept = 1, drift = x)
  if (!is.null(weights)) {
    root <- sqrt(weights)
    design <- design * root
    y <- y * root
  }
  fit <- qr(design, LAPACK = FALSE, tol = 1e-12)
  if (fit$rank != 2L) {
    .stream_abort(
      "clock observations are rank deficient",
      "PhysioStream_clock_rank_error"
    )
  }
  coefficient <- as.numeric(qr.coef(fit, y))
  if (length(coefficient) != 2L || any(!is.finite(coefficient))) {
    .stream_abort(
      "clock fit produced non-finite coefficients",
      "PhysioStream_clock_fit_error"
    )
  }
  coefficient
}

.clock_huber <- function(x, y, k) {
  coefficient <- .clock_ols(x, y)
  converged <- FALSE
  weights <- rep(1, length(x))
  for (iteration in seq_len(100L)) {
    residual <- y - coefficient[[1L]] - coefficient[[2L]] * x
    scale <- 1.4826 * stats::median(
      abs(residual - stats::median(residual))
    )
    floor_scale <- .Machine$double.eps *
      max(1, max(abs(y)), max(abs(coefficient)))
    if (scale <= floor_scale) {
      exact <- abs(residual) <= 64 * floor_scale
      if (sum(exact) >= 2L && length(unique(x[exact])) >= 2L) {
        coefficient <- .clock_ols(x[exact], y[exact])
        weights <- as.numeric(exact)
      } else if (max(abs(residual)) > 64 * floor_scale) {
        .stream_abort(
          "Huber clock fit has zero robust scale without enough exact inliers",
          "PhysioStream_clock_fit_error"
        )
      }
      converged <- TRUE
      break
    }
    weights <- pmin(1, k * scale / pmax(abs(residual), .Machine$double.xmin))
    updated <- .clock_ols(x, y, weights)
    delta <- max(abs(updated - coefficient)) /
      (1 + max(abs(coefficient)))
    coefficient <- updated
    if (delta <= 1e-12) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    .stream_abort(
      "Huber clock fit did not converge in 100 iterations",
      "PhysioStream_clock_convergence_error"
    )
  }
  residual <- y - coefficient[[1L]] - coefficient[[2L]] * x
  scale <- 1.4826 * stats::median(
    abs(residual - stats::median(residual))
  )
  numeric_floor <- 64 * .Machine$double.eps *
    max(1, max(abs(y)), max(abs(coefficient)))
  inlier <- abs(residual) <= max(k * scale, numeric_floor, 1e-12)
  list(coefficient = coefficient, inlier = inlier, converged = TRUE)
}

.clock_ransac <- function(x, y, threshold, trials, seed) {
  ordinary <- .clock_ols(x, y)
  ordinary_residual <- y - ordinary[[1L]] - ordinary[[2L]] * x
  if (is.null(threshold)) {
    threshold <- max(
      3 * 1.4826 * stats::median(
        abs(ordinary_residual - stats::median(ordinary_residual))
      ),
      1e-9
    )
  } else {
    threshold <- .clock_scalar_number(
      threshold, "ransac_threshold", .Machine$double.xmin
    )
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  best <- NULL
  best_count <- -1L
  best_median <- Inf
  best_pair <- c(.Machine$integer.max, .Machine$integer.max)
  for (iteration in seq_len(trials)) {
    pair <- sort(sample.int(length(x), 2L, replace = FALSE))
    if (x[pair[[1L]]] == x[pair[[2L]]]) {
      next
    }
    drift <- (y[pair[[2L]]] - y[pair[[1L]]]) /
      (x[pair[[2L]]] - x[pair[[1L]]])
    intercept <- y[pair[[1L]]] - drift * x[pair[[1L]]]
    residual <- abs(y - intercept - drift * x)
    inlier <- residual <= threshold
    count <- sum(inlier)
    median_residual <- if (count) stats::median(residual[inlier]) else Inf
    better <- count > best_count ||
      (count == best_count && median_residual < best_median) ||
      (count == best_count && median_residual == best_median &&
       (pair[[1L]] < best_pair[[1L]] ||
        (pair[[1L]] == best_pair[[1L]] &&
         pair[[2L]] < best_pair[[2L]])))
    if (better) {
      best <- inlier
      best_count <- count
      best_median <- median_residual
      best_pair <- pair
    }
  }
  minimum <- max(2L, ceiling(length(x) * 0.5))
  if (is.null(best) || best_count < minimum ||
      length(unique(x[best])) < 2L) {
    .stream_abort(
      "RANSAC clock fit did not retain enough distinct-time inliers",
      "PhysioStream_clock_fit_error"
    )
  }
  list(
    coefficient = .clock_ols(x[best], y[best]),
    inlier = best,
    converged = TRUE,
    threshold = threshold,
    pair = as.integer(best_pair)
  )
}

.clock_hash_payload <- function(model) {
  payload <- unclass(model)
  payload$evidence_sha256 <- NULL
  payload
}

.clock_finalize_model <- function(model) {
  class(model) <- c("StreamClockModel", "list")
  model$evidence_sha256 <- digest::digest(
    .clock_hash_payload(model), algo = "sha256", serialize = TRUE
  )
  model
}

.clock_validate_model <- function(model) {
  contains_runtime <- function(x) {
    if (is.environment(x) || is.function(x) ||
        methods::is(x, "externalptr") || inherits(x, "connection") ||
        (isS4(x) && !methods::is(x, "DataFrame"))) {
      return(TRUE)
    }
    if (is.list(x)) {
      return(any(vapply(x, contains_runtime, logical(1))))
    }
    FALSE
  }
  if (!identical(class(model), c("StreamClockModel", "list")) ||
      !is.list(model) ||
      !identical(names(model), .clock_model_fields) ||
      !identical(model$schema, .clock_schema_version) ||
      !is.character(model$method) || length(model$method) != 1L ||
      is.na(model$method) ||
      !(model$method %in% c("ols", "huber", "ransac", "identity")) ||
      !is.character(model$origin_rule) || length(model$origin_rule) != 1L ||
      is.na(model$origin_rule) ||
      !(model$origin_rule %in% c("median", "first")) ||
      !identical(model$sign_convention, "master = device + offset") ||
      !is.data.frame(model$segments) ||
      !identical(names(model$segments), .clock_segment_columns) ||
      !is.numeric(model$origin) ||
      length(model$origin) != nrow(model$segments) ||
      !is.character(model$input_domain) ||
      length(model$input_domain) != 1L || is.na(model$input_domain) ||
      !nzchar(model$input_domain) ||
      !is.character(model$evidence_sha256) ||
      length(model$evidence_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", model$evidence_sha256) ||
      !is.character(model$input_sha256) ||
      length(model$input_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", model$input_sha256) ||
      !is.list(model$quality) ||
      !identical(names(model$quality), c("converged", "rank", "warnings")) ||
      !is.list(model$inliers) ||
      length(model$inliers) != nrow(model$segments) ||
      !is.list(model$fit_details) ||
      length(model$fit_details) != nrow(model$segments) ||
      !is.list(model$parameters) ||
      contains_runtime(model) ||
      !identical(
        model$evidence_sha256,
        digest::digest(.clock_hash_payload(model), algo = "sha256",
                       serialize = TRUE)
      )) {
    .stream_abort(
      "`model` is invalid or its governed hash does not match",
      "PhysioStream_clock_model_error"
    )
  }
  numeric_columns <- setdiff(names(model$segments), "segment")
  if (any(!is.finite(as.matrix(model$segments[numeric_columns]))) ||
      any(model$segments$segment != seq_len(nrow(model$segments))) ||
      any(model$segments$device_end < model$segments$device_start) ||
      any(model$segments$n_inlier <
            if (identical(model$method, "identity")) 0 else 2) ||
      any(model$segments$n_total < model$segments$n_inlier) ||
      any(!is.finite(model$origin))) {
    .stream_abort(
      "`model` contains invalid segment coefficients or bounds",
      "PhysioStream_clock_model_error"
    )
  }
  invisible(model)
}

#' Estimate a governed stream-clock mapping
#'
#' Fits recorded clock offsets under the sign convention
#' `master_time = device_time + offset(device_time)`. Fits are centered within
#' each reset segment.
#'
#' @param clock_times Finite non-decreasing device-clock observation times.
#' @param clock_values Finite observed offsets in seconds.
#' @param method Exact fitting method.
#' @param origin Exact centering rule.
#' @param segments Optional positive labels or zero-based inclusive ranges.
#' @param huber_k Positive Huber tuning constant.
#' @param ransac_threshold Optional positive inlier threshold in seconds.
#' @param ransac_trials Positive number of deterministic RANSAC trials.
#' @param seed Non-negative deterministic RANSAC seed.
#' @return A serializable `StreamClockModel`.
#' @examples
#' device <- seq(0, 10, by = 0.5)
#' offset <- -0.4 + 0.005 * (device - median(device))
#' model <- clockOffset(device, offset, method = "ols")
#' model$segments$drift_ppm
#' @export
clockOffset <- function(clock_times, clock_values,
                        method = c("huber", "ols", "ransac"),
                        origin = c("median", "first"), segments = NULL,
                        huber_k = 1.345, ransac_threshold = NULL,
                        ransac_trials = 200L, seed = 1L) {
  if (length(method) > 1L && !identical(method, c("huber", "ols", "ransac"))) {
    .stream_abort("`method` must be one exact scalar value",
                  "PhysioStream_clock_validation_error")
  }
  method <- .clock_enum(method[[1L]], c("huber", "ols", "ransac"), "method")
  if (length(origin) > 1L && !identical(origin, c("median", "first"))) {
    .stream_abort("`origin` must be one exact scalar value",
                  "PhysioStream_clock_validation_error")
  }
  origin_rule <- .clock_enum(
    origin[[1L]], c("median", "first"), "origin"
  )
  clock_times <- .clock_numeric_vector(clock_times, "clock_times")
  clock_values <- .clock_numeric_vector(clock_values, "clock_values")
  if (length(clock_times) != length(clock_values) || length(clock_times) < 2L) {
    .stream_abort(
      "clock observations must have equal lengths of at least two",
      "PhysioStream_clock_validation_error"
    )
  }
  if (length(clock_times) * 128 > .clock_allocation_limit) {
    .stream_abort(
      "clock observations exceed the governed allocation ceiling",
      "PhysioStream_clock_resource_error"
    )
  }
  normalized <- .clock_normalize_segments(
    segments, length(clock_times), "segments"
  )
  huber_k <- .clock_scalar_number(
    huber_k, "huber_k", .Machine$double.xmin
  )
  ransac_trials <- .clock_scalar_number(
    ransac_trials, "ransac_trials", 1, .Machine$integer.max, integer = TRUE
  )
  seed <- .clock_scalar_number(
    seed, "seed", 0, .Machine$integer.max, integer = TRUE
  )

  rows <- vector("list", nrow(normalized$ranges))
  origins <- numeric(length(rows))
  inliers <- vector("list", length(rows))
  fit_details <- vector("list", length(rows))
  for (segment in seq_along(rows)) {
    index <- which(normalized$labels == segment)
    times <- clock_times[index]
    values <- clock_values[index]
    if (length(times) < 2L || length(unique(times)) < 2L) {
      .stream_abort(
        sprintf("clock segment %d needs at least two distinct times", segment),
        "PhysioStream_clock_rank_error"
      )
    }
    if (any(diff(times) < 0)) {
      .stream_abort(
        sprintf("clock times decrease within segment %d", segment),
        "PhysioStream_clock_segment_error"
      )
    }
    duplicate <- duplicated(times) | duplicated(times, fromLast = TRUE)
    if (any(duplicate)) {
      groups <- split(values[duplicate], times[duplicate])
      if (any(vapply(groups, function(value) {
        length(unique(value)) != 1L
      }, logical(1)))) {
        .stream_abort(
          sprintf("clock segment %d has contradictory duplicate times", segment),
          "PhysioStream_clock_segment_error"
        )
      }
    }
    origin_time <- if (identical(origin_rule, "median")) {
      stats::median(times)
    } else {
      times[[1L]]
    }
    centered <- times - origin_time
    fit <- switch(
      method,
      ols = list(
        coefficient = .clock_ols(centered, values),
        inlier = rep(TRUE, length(centered)),
        converged = TRUE
      ),
      huber = .clock_huber(centered, values, huber_k),
      ransac = {
        segment_seed <- (as.double(seed) + segment - 1) %%
          .Machine$integer.max
        .clock_ransac(
          centered, values, ransac_threshold, ransac_trials,
          as.integer(segment_seed)
        )
      }
    )
    residual <- values - fit$coefficient[[1L]] -
      fit$coefficient[[2L]] * centered
    assessed <- residual[fit$inlier]
    origins[[segment]] <- origin_time
    inliers[[segment]] <- as.logical(fit$inlier)
    fit_details[[segment]] <- fit[setdiff(
      names(fit), c("coefficient", "inlier")
    )]
    rows[[segment]] <- data.frame(
      segment = as.integer(segment),
      observation_start = min(index),
      observation_end = max(index),
      device_start = min(times),
      device_end = max(times),
      intercept = fit$coefficient[[1L]],
      drift = fit$coefficient[[2L]],
      drift_ppm = fit$coefficient[[2L]] * 1e6,
      rmse = sqrt(mean(assessed^2)),
      mad = stats::median(abs(assessed - stats::median(assessed))),
      max_abs = max(abs(assessed)),
      n_inlier = sum(fit$inlier),
      n_total = length(times),
      stringsAsFactors = FALSE
    )
  }
  segment_table <- do.call(rbind, rows)
  rownames(segment_table) <- NULL
  input_sha256 <- digest::digest(
    list(
      clock_times = unname(clock_times),
      clock_values = unname(clock_values),
      segments = normalized$labels,
      method = method,
      origin_rule = origin_rule,
      huber_k = huber_k,
      ransac_threshold = ransac_threshold,
      ransac_trials = ransac_trials,
      seed = seed
    ),
    algo = "sha256", serialize = TRUE
  )
  .clock_finalize_model(list(
    schema = .clock_schema_version,
    method = method,
    origin = origins,
    origin_rule = origin_rule,
    segments = segment_table,
    sign_convention = "master = device + offset",
    input_domain = "device_time",
    quality = list(
      converged = TRUE,
      rank = rep.int(2L, nrow(segment_table)),
      warnings = character()
    ),
    inliers = inliers,
    fit_details = fit_details,
    parameters = list(
      huber_k = huber_k,
      ransac_threshold = ransac_threshold,
      ransac_trials = ransac_trials,
      seed = seed
    ),
    input_sha256 = input_sha256,
    evidence_sha256 = ""
  ))
}

#' @export
print.StreamClockModel <- function(x, ...) {
  .clock_validate_model(x)
  cat("StreamClockModel ", x$schema, "\n", sep = "")
  cat("method: ", x$method, "\n", sep = "")
  cat("sign: ", x$sign_convention, "\n", sep = "")
  cat("segments: ", nrow(x$segments), "\n", sep = "")
  if (nrow(x$segments)) {
    print(x$segments[, c(
      "segment", "intercept", "drift_ppm", "rmse", "n_inlier", "n_total"
    )], row.names = FALSE)
  }
  invisible(x)
}

#' Apply a governed stream-clock model
#'
#' @param timestamps Finite device-clock timestamps.
#' @param model A valid `StreamClockModel`.
#' @param segments Optional positive labels or zero-based inclusive ranges.
#' @param extrapolate Exact extrapolation policy.
#' @param max_extrapolation_seconds Non-negative bounded extrapolation limit.
#' @return Corrected timestamps with a plain `clock_correction` attribute.
#' @examples
#' device <- seq(0, 10, by = 0.5)
#' offset <- -0.4 + 0.005 * (device - median(device))
#' model <- clockOffset(device, offset, method = "ols")
#' corrected <- driftCorrect(seq(0, 10, by = 1), model)
#' as.numeric(corrected)
#' @export
driftCorrect <- function(timestamps, model, segments = NULL,
                         extrapolate = c("bounded", "error"),
                         max_extrapolation_seconds = 30) {
  timestamps <- .clock_numeric_vector(timestamps, "timestamps")
  .clock_validate_model(model)
  if (length(extrapolate) > 1L &&
      !identical(extrapolate, c("bounded", "error"))) {
    .stream_abort("`extrapolate` must be one exact scalar value",
                  "PhysioStream_clock_validation_error")
  }
  extrapolate <- .clock_enum(
    extrapolate[[1L]], c("bounded", "error"), "extrapolate"
  )
  limit <- .clock_scalar_number(
    max_extrapolation_seconds, "max_extrapolation_seconds", 0
  )
  if (!length(timestamps)) {
    result <- numeric()
    attr(result, "clock_correction") <- list(
      schema = .clock_schema_version,
      model_sha256 = model$evidence_sha256,
      segment = integer(),
      applied_offset = numeric(),
      extrapolation_seconds = numeric()
    )
    return(result)
  }
  normalized <- .clock_normalize_segments(
    segments, length(timestamps), "segments"
  )
  if (nrow(normalized$ranges) != nrow(model$segments)) {
    .stream_abort(
      "timestamp segments do not match the clock model",
      "PhysioStream_clock_segment_error"
    )
  }
  corrected <- numeric(length(timestamps))
  applied <- numeric(length(timestamps))
  distance <- numeric(length(timestamps))
  for (segment in seq_len(nrow(model$segments))) {
    index <- which(normalized$labels == segment)
    time <- timestamps[index]
    if (any(diff(time) < 0)) {
      .stream_abort(
        sprintf("timestamps decrease within segment %d", segment),
        "PhysioStream_clock_segment_error"
      )
    }
    row <- model$segments[segment, ]
    distance[index] <- pmax(row$device_start - time, time - row$device_end, 0)
    if (identical(extrapolate, "error") && any(distance[index] > 0)) {
      .stream_abort(
        sprintf("timestamps extrapolate beyond clock segment %d", segment),
        "PhysioStream_clock_extrapolation_error"
      )
    }
    if (any(distance[index] > limit)) {
      .stream_abort(
        sprintf("timestamps exceed bounded extrapolation in segment %d", segment),
        "PhysioStream_clock_extrapolation_error"
      )
    }
    applied[index] <- row$intercept +
      row$drift * (time - model$origin[[segment]])
    corrected[index] <- time + applied[index]
    if (any(!is.finite(corrected[index])) ||
        any(diff(corrected[index]) < 0)) {
      .stream_abort(
        sprintf("corrected timestamps are invalid in segment %d", segment),
        "PhysioStream_clock_fit_error"
      )
    }
  }
  attr(corrected, "clock_correction") <- list(
    schema = .clock_schema_version,
    model_sha256 = model$evidence_sha256,
    segment = normalized$labels,
    applied_offset = applied,
    extrapolation_seconds = distance
  )
  corrected
}

.clock_dejitter <- function(timestamps, nominal_srate, segments, anchor,
                            max_residual_seconds) {
  timestamps <- .clock_numeric_vector(timestamps, "timestamps")
  if (is.factor(nominal_srate) || !is.numeric(nominal_srate) ||
      length(nominal_srate) != 1L || !is.finite(nominal_srate) ||
      nominal_srate <= 0) {
    .stream_abort(
      "`nominal_srate` must be one positive finite number",
      "PhysioStream_clock_validation_error"
    )
  }
  nominal_srate <- as.numeric(nominal_srate)
  if (length(anchor) > 1L &&
      !identical(anchor, c("least_squares", "first"))) {
    .stream_abort("`anchor` must be one exact scalar value",
                  "PhysioStream_clock_validation_error")
  }
  anchor <- .clock_enum(
    anchor[[1L]], c("least_squares", "first"), "anchor"
  )
  if (!is.numeric(max_residual_seconds) ||
      length(max_residual_seconds) != 1L ||
      is.na(max_residual_seconds) || max_residual_seconds < 0) {
    .stream_abort(
      "`max_residual_seconds` must be one non-negative number",
      "PhysioStream_clock_validation_error"
    )
  }
  maximum <- as.numeric(max_residual_seconds)
  normalized <- .clock_normalize_segments(
    segments, length(timestamps), "segments"
  )
  regular <- numeric(length(timestamps))
  residual <- numeric(length(timestamps))
  summaries <- vector("list", nrow(normalized$ranges))
  for (segment in seq_len(nrow(normalized$ranges))) {
    index <- which(normalized$labels == segment)
    time <- timestamps[index]
    if (any(diff(time) < 0)) {
      .stream_abort(
        sprintf("timestamps decrease within segment %d", segment),
        "PhysioStream_clock_segment_error"
      )
    }
    grid_index <- seq_along(time) - 1
    intercept <- if (identical(anchor, "first")) {
      time[[1L]]
    } else {
      mean(time - grid_index / nominal_srate)
    }
    regular[index] <- intercept + grid_index / nominal_srate
    residual[index] <- time - regular[index]
    effective <- if (length(time) > 1L && time[[length(time)]] > time[[1L]]) {
      (length(time) - 1) / (time[[length(time)]] - time[[1L]])
    } else {
      NA_real_
    }
    summaries[[segment]] <- data.frame(
      segment = as.integer(segment),
      rmse = sqrt(mean(residual[index]^2)),
      mad = stats::median(abs(
        residual[index] - stats::median(residual[index])
      )),
      max_abs = max(abs(residual[index])),
      effective_srate = effective,
      stringsAsFactors = FALSE
    )
  }
  summary <- if (length(summaries)) {
    do.call(rbind, summaries)
  } else {
    data.frame(
      segment = integer(), rmse = numeric(), mad = numeric(),
      max_abs = numeric(), effective_srate = numeric()
    )
  }
  rownames(summary) <- NULL
  if (nrow(summary) && any(summary$max_abs > maximum)) {
    .stream_abort(
      "timestamp jitter exceeds `max_residual_seconds`",
      "PhysioStream_clock_residual_error"
    )
  }
  attr(regular, "dejitter") <- list(
    schema = .clock_schema_version,
    raw_time = timestamps,
    regular_time = regular,
    residual = residual,
    segment = normalized$labels,
    rmse = summary$rmse,
    mad = summary$mad,
    max_abs = summary$max_abs,
    effective_srate = summary$effective_srate,
    summary = summary
  )
  regular
}

#' Regularize stream timestamps on a nominal grid
#'
#' This changes timestamps only. It never interpolates or reorders samples.
#'
#' @param timestamps Finite non-decreasing timestamps.
#' @param nominal_srate Positive nominal sampling rate.
#' @param segments Optional positive labels or zero-based inclusive ranges.
#' @param anchor Exact grid anchoring rule.
#' @param max_residual_seconds Maximum permitted absolute jitter residual.
#' @return Regular timestamps with a plain `dejitter` diagnostics attribute.
#' @examples
#' raw <- c(10.001, 10.010, 10.021, 10.029)
#' regular <- dejitter(raw, nominal_srate = 100)
#' as.numeric(regular)
#' @export
dejitter <- function(timestamps, nominal_srate, segments = NULL,
                     anchor = c("least_squares", "first"),
                     max_residual_seconds = Inf) {
  .clock_dejitter(
    timestamps, nominal_srate, segments, anchor, max_residual_seconds
  )
}
