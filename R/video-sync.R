.video_sync_assert <- function(sync) {
  if (!inherits(sync, "VideoSync") || !is.list(sync)) {
    .biofeedback_abort(
      "`sync` must be a VideoSync",
      "PhysioStream_video_sync_validation_error"
    )
  }
  payload <- unclass(sync)
  .biofeedback_validate_sealed(
    payload, .biofeedback_state_limit, "video sync"
  )
  required <- c(
    "method", "clock_domain", "frame_rate", "intercept", "rate",
    "signal_origin", "video_origin", "rate_error_ppm", "signal_anchor",
    "video_anchor", "residual_seconds", "residual_frames",
    "max_abs_residual_frames", "tolerance_frames", "anchor_sha256"
  )
  scalar <- function(x) {
    is.numeric(x) && !is.object(x) && is.null(dim(x)) &&
      length(x) == 1L && is.finite(x)
  }
  vector <- function(x) {
    is.numeric(x) && !is.object(x) && is.null(dim(x)) &&
      length(x) > 0L && all(is.finite(x))
  }
  if (!all(required %in% names(payload)) ||
      !is.character(payload$method) || length(payload$method) != 1L ||
      is.na(payload$method) ||
      !payload$method %in% c("offset", "affine") ||
      !is.character(payload$clock_domain) ||
      length(payload$clock_domain) != 1L ||
      is.na(payload$clock_domain) || !nzchar(payload$clock_domain) ||
      !all(vapply(
        payload[c(
          "frame_rate", "intercept", "rate", "signal_origin",
          "video_origin", "rate_error_ppm", "max_abs_residual_frames",
          "tolerance_frames", "max_rate_error_ppm"
        )],
        scalar, logical(1)
      )) ||
      payload$frame_rate <= 0 || payload$frame_rate > 1000 ||
      payload$rate <= 0 ||
      payload$tolerance_frames < 0 || payload$tolerance_frames > 10 ||
      payload$max_rate_error_ppm <= 0 ||
      payload$max_rate_error_ppm > 1e6 ||
      !vector(payload$signal_anchor) ||
      !vector(payload$video_anchor) ||
      !vector(payload$residual_seconds) ||
      !vector(payload$residual_frames) ||
      length(payload$signal_anchor) != length(payload$video_anchor) ||
      length(payload$residual_seconds) != length(payload$signal_anchor) ||
      length(payload$residual_frames) != length(payload$signal_anchor) ||
      (length(payload$signal_anchor) > 1L &&
       (any(diff(payload$signal_anchor) <= 0) ||
        any(diff(payload$video_anchor) <= 0))) ||
      (identical(payload$method, "affine") &&
       length(payload$signal_anchor) < 3L) ||
      !identical(
        payload$anchor_sha256,
        digest::digest(
          serialize(
            list(
              signal_time = payload$signal_anchor,
              video_time = payload$video_anchor,
              clock_domain = payload$clock_domain
            ),
            NULL, version = 3L
          ),
          algo = "sha256", serialize = FALSE
        )
      )) {
    .biofeedback_abort(
      "VideoSync payload is invalid",
      "PhysioStream_video_sync_state_error"
    )
  }
  fitted <- if (identical(payload$method, "offset")) {
    signal_origin <- stats::median(payload$signal_anchor)
    rate <- 1
    video_origin <- signal_origin +
      stats::median(payload$video_anchor - payload$signal_anchor)
    c(
      signal_origin = signal_origin,
      video_origin = video_origin,
      rate = rate
    )
  } else {
    signal_origin <- mean(range(payload$signal_anchor))
    design <- cbind(
      intercept = 1,
      centered = payload$signal_anchor - signal_origin
    )
    fit <- qr(design, LAPACK = FALSE)
    if (fit$rank != 2L) {
      .biofeedback_abort(
        "VideoSync anchors are rank deficient",
        "PhysioStream_video_sync_state_error"
      )
    }
    coefficient <- qr.coef(fit, payload$video_anchor)
    c(
      signal_origin = signal_origin,
      video_origin = unname(coefficient[[1L]]),
      rate = unname(coefficient[[2L]])
    )
  }
  predicted <- payload$video_origin +
    payload$rate * (payload$signal_anchor - payload$signal_origin)
  residual <- payload$video_anchor - predicted
  scalar_equal <- function(x, y) {
    isTRUE(all.equal(
      x, y, tolerance = 64 * .Machine$double.eps,
      check.attributes = FALSE
    ))
  }
  if (!scalar_equal(payload$signal_origin, fitted[["signal_origin"]]) ||
      !scalar_equal(payload$video_origin, fitted[["video_origin"]]) ||
      !scalar_equal(payload$rate, fitted[["rate"]]) ||
      !scalar_equal(
        payload$intercept,
        payload$video_origin - payload$rate * payload$signal_origin
      ) ||
      !scalar_equal(payload$rate_error_ppm, abs(payload$rate - 1) * 1e6) ||
      payload$rate_error_ppm > payload$max_rate_error_ppm ||
      !isTRUE(all.equal(
    residual, payload$residual_seconds,
    tolerance = 0, check.attributes = FALSE
  )) ||
      !isTRUE(all.equal(
        residual * payload$frame_rate, payload$residual_frames,
        tolerance = 0, check.attributes = FALSE
      )) ||
      !scalar_equal(
        payload$max_abs_residual_frames,
        max(abs(payload$residual_frames))
      ) ||
      max(abs(payload$residual_frames)) >
        payload$tolerance_frames + 64 * .Machine$double.eps) {
    .biofeedback_abort(
      "VideoSync residual evidence is inconsistent",
      "PhysioStream_video_sync_state_error"
    )
  }
  invisible(TRUE)
}

#' Fit a governed marker-anchored video/signal time map
#'
#' The mapping convention is
#' `video_seconds = intercept + rate * signal_seconds`. Evaluation uses an
#' equivalent centered representation to retain precision for large clocks.
#'
#' @param signal_time Strictly increasing signal-domain marker times.
#' @param video_time Strictly increasing corresponding media times.
#' @param frame_rate Positive media frames per second.
#' @param method Exact offset-only or affine fit.
#' @param clock_domain Exact signal clock-domain label.
#' @param tolerance_frames Maximum accepted anchor residual in frames.
#' @param max_rate_error_ppm Maximum affine rate deviation from one.
#' @return A sealed portable `VideoSync`.
#' @examples
#' sync <- videoSync(
#'   signal_time = c(a = 100, b = 110, c = 120),
#'   video_time = c(a = 1, b = 11, c = 21),
#'   frame_rate = 30, method = "offset", clock_domain = "demo")
#' videoSyncTime(sync, c(x = 105))
#' @export
videoSync <- function(
    signal_time,
    video_time,
    frame_rate,
    method = c("offset", "affine"),
    clock_domain,
    tolerance_frames = 1,
    max_rate_error_ppm = 5000) {
  signal_names <- names(signal_time)
  video_names <- names(video_time)
  signal_time <- .biofeedback_numeric(signal_time, "signal_time")
  video_time <- .biofeedback_numeric(video_time, "video_time")
  if (!length(signal_time) || length(signal_time) != length(video_time) ||
      (length(signal_time) > 1L && any(diff(signal_time) <= 0)) ||
      (length(video_time) > 1L && any(diff(video_time) <= 0))) {
    .biofeedback_abort(
      "marker times must be equal-length strictly increasing vectors",
      "PhysioStream_video_sync_validation_error"
    )
  }
  if (any(abs(c(signal_time, video_time)) * .Machine$double.eps > 1e-6)) {
    .biofeedback_abort(
      "video synchronization anchors cannot retain microsecond resolution",
      "PhysioStream_video_sync_resource_error"
    )
  }
  if (!is.null(signal_names) || !is.null(video_names)) {
    if (is.null(signal_names) || is.null(video_names) ||
        !identical(signal_names, video_names) ||
        anyNA(signal_names) || any(!nzchar(signal_names)) ||
        anyDuplicated(signal_names)) {
      .biofeedback_abort(
        "named marker vectors must have identical unique names",
        "PhysioStream_video_sync_validation_error"
      )
    }
  }
  frame_rate <- .biofeedback_scalar(
    frame_rate, "frame_rate", lower = 0, lower_open = TRUE, upper = 1000
  )
  if (length(method) > 1L) {
    method <- method[[1L]]
  }
  method <- .biofeedback_enum(method, c("offset", "affine"), "method")
  clock_domain <- .biofeedback_name(clock_domain, "clock_domain")
  tolerance_frames <- .biofeedback_scalar(
    tolerance_frames, "tolerance_frames", lower = 0, upper = 10
  )
  max_rate_error_ppm <- .biofeedback_scalar(
    max_rate_error_ppm, "max_rate_error_ppm",
    lower = 0, lower_open = TRUE, upper = 1e6
  )
  if (identical(method, "offset")) {
    signal_origin <- stats::median(signal_time)
    offset <- stats::median(video_time - signal_time)
    rate <- 1
    video_origin <- signal_origin + offset
  } else {
    if (length(signal_time) < 3L) {
      .biofeedback_abort(
        "affine video synchronization requires at least three anchors",
        "PhysioStream_video_sync_validation_error"
      )
    }
    signal_origin <- mean(range(signal_time))
    centered <- signal_time - signal_origin
    design <- cbind(intercept = 1, centered = centered)
    fit <- qr(design, LAPACK = FALSE)
    if (fit$rank != 2L) {
      .biofeedback_abort(
        "video synchronization anchors are rank deficient",
        "PhysioStream_video_sync_validation_error"
      )
    }
    coefficients <- qr.coef(fit, video_time)
    video_origin <- unname(coefficients[[1L]])
    rate <- unname(coefficients[[2L]])
    if (!is.finite(rate) || rate <= 0) {
      .biofeedback_abort(
        "video synchronization requires a positive finite clock rate",
        "PhysioStream_video_sync_validation_error"
      )
    }
  }
  rate_error_ppm <- abs(rate - 1) * 1e6
  if (rate_error_ppm > max_rate_error_ppm) {
    .biofeedback_abort(
      "video synchronization rate error exceeds the configured ceiling",
      "PhysioStream_video_sync_timing_error"
    )
  }
  intercept <- video_origin - rate * signal_origin
  predicted <- video_origin + rate * (signal_time - signal_origin)
  residual_seconds <- video_time - predicted
  residual_frames <- residual_seconds * frame_rate
  max_abs_residual_frames <- max(abs(residual_frames))
  if (max_abs_residual_frames >
      tolerance_frames + 64 * .Machine$double.eps) {
    .biofeedback_abort(
      "video synchronization anchor residual exceeds frame tolerance",
      "PhysioStream_video_sync_timing_error"
    )
  }
  anchor_sha256 <- digest::digest(
    serialize(
      list(
        signal_time = signal_time,
        video_time = video_time,
        clock_domain = clock_domain
      ),
      NULL, version = 3L
    ),
    algo = "sha256", serialize = FALSE
  )
  payload <- list(
    method = method,
    clock_domain = clock_domain,
    frame_rate = frame_rate,
    intercept = intercept,
    rate = rate,
    signal_origin = signal_origin,
    video_origin = video_origin,
    rate_error_ppm = rate_error_ppm,
    signal_anchor = signal_time,
    video_anchor = video_time,
    residual_seconds = residual_seconds,
    residual_frames = residual_frames,
    max_abs_residual_frames = max_abs_residual_frames,
    tolerance_frames = tolerance_frames,
    max_rate_error_ppm = max_rate_error_ppm,
    anchor_sha256 = anchor_sha256
  )
  payload <- .biofeedback_seal(
    payload, .biofeedback_state_limit, "video sync"
  )
  structure(payload, class = c("VideoSync", "list"))
}

.video_sync_map <- function(sync, values, inverse = FALSE) {
  .video_sync_assert(sync)
  value_names <- names(values)
  values <- .biofeedback_numeric(
    values, if (inverse) "video_time" else "signal_time"
  )
  if (inverse && length(values) && any(values < 0)) {
    tolerance <- 1 / unclass(sync)$frame_rate
    if (any(values < -tolerance)) {
      .biofeedback_abort(
        "video time precedes media start by more than one frame",
        "PhysioStream_video_sync_timing_error"
      )
    }
    values[values < 0] <- 0
  }
  if (length(values) &&
      any(abs(values) * .Machine$double.eps > 1e-6)) {
    .biofeedback_abort(
      "video synchronization input cannot retain microsecond resolution",
      "PhysioStream_video_sync_resource_error"
    )
  }
  payload <- unclass(sync)
  mapped <- if (inverse) {
    payload$signal_origin +
      (values - payload$video_origin) / payload$rate
  } else {
    payload$video_origin +
      payload$rate * (values - payload$signal_origin)
  }
  if (any(!is.finite(mapped))) {
    .biofeedback_abort(
      "video synchronization mapping overflowed finite numeric range",
      "PhysioStream_video_sync_resource_error"
    )
  }
  if (length(mapped) &&
      any(abs(mapped) * .Machine$double.eps > 1e-6)) {
    .biofeedback_abort(
      "video synchronization result cannot retain microsecond resolution",
      "PhysioStream_video_sync_resource_error"
    )
  }
  if (!inverse && length(mapped) && any(mapped < 0)) {
    tolerance <- 1 / payload$frame_rate
    if (any(mapped < -tolerance)) {
      .biofeedback_abort(
        "mapped video time precedes media start by more than one frame",
        "PhysioStream_video_sync_timing_error"
      )
    }
    mapped[mapped < 0] <- 0
  }
  if (length(values) > 1L) {
    increasing <- diff(values) > 0
    if (any(increasing & diff(mapped) <= 0)) {
      .biofeedback_abort(
        "video synchronization mapping lost timestamp ordering precision",
        "PhysioStream_video_sync_timing_error"
      )
    }
  }
  names(mapped) <- value_names
  mapped
}

#' Map between synchronized signal and video time
#'
#' @param sync A validated `VideoSync`.
#' @param signal_time Finite signal-domain times.
#' @param video_time Finite media times.
#' @return A numeric vector preserving names.
#' @examples
#' sync <- videoSync(c(a = 100, b = 110, c = 120), c(a = 1, b = 11, c = 21),
#'                   frame_rate = 30, method = "offset", clock_domain = "demo")
#' videoSyncTime(sync, c(x = 105, y = 115))
#' signalSyncTime(sync, c(x = 6, y = 16))
#' @name video-sync-map
NULL

#' @rdname video-sync-map
#' @export
videoSyncTime <- function(sync, signal_time) {
  .video_sync_map(sync, signal_time, inverse = FALSE)
}

#' @rdname video-sync-map
#' @export
signalSyncTime <- function(sync, video_time) {
  .video_sync_map(sync, video_time, inverse = TRUE)
}

.video_config <- function(video, sync) {
  .video_sync_assert(sync)
  video <- .biofeedback_name(video, "video")
  if (identical(video, "webcam")) {
    return(list(
      kind = "webcam",
      source = NULL,
      sync = unclass(sync)
    ))
  }
  if (grepl("^https://", video)) {
    if (nchar(video, type = "bytes") > 2048L ||
        grepl("[[:space:]<>\"']", video)) {
      .biofeedback_abort(
        "`video` HTTPS URL is malformed or too long",
        "PhysioStream_video_sync_validation_error"
      )
    }
    return(list(
      kind = "https",
      source = video,
      sync = unclass(sync)
    ))
  }
  info <- file.info(video)
  if (!file.exists(video) || isTRUE(info$isdir) ||
      file.access(video, 4L) != 0L ||
      nzchar(Sys.readlink(video))) {
    .biofeedback_abort(
      "local `video` must be a regular non-symlink file",
      "PhysioStream_video_sync_validation_error"
    )
  }
  extension <- tolower(tools::file_ext(video))
  if (!extension %in% c("mp4", "webm", "ogg")) {
    .biofeedback_abort(
      "local `video` must have an mp4, webm, or ogg extension",
      "PhysioStream_video_sync_validation_error"
    )
  }
  if (!is.finite(info$size) || info$size <= 0 ||
      info$size > .biofeedback_materialization_limit) {
    .biofeedback_abort(
      "local `video` size is empty, invalid, or above 512 MiB",
      "PhysioStream_video_sync_resource_error"
    )
  }
  list(
    kind = "local",
    source = normalizePath(video, winslash = "/", mustWork = TRUE),
    extension = extension,
    size = as.numeric(info$size),
    sha256 = digest::digest(
      file = video, algo = "sha256", serialize = FALSE
    ),
    sync = unclass(sync)
  )
}

.video_prepare_for_shiny <- function(config) {
  if (is.null(config)) {
    return(NULL)
  }
  if (!identical(config$kind, "local")) {
    config$cleanup <- NULL
    return(config)
  }
  root <- tempfile("physiostream-video-")
  if (!dir.create(root, mode = "0700")) {
    .biofeedback_abort(
      "could not create an isolated video resource directory",
      "PhysioStream_video_sync_resource_error"
    )
  }
  destination <- file.path(root, paste0("media.", config$extension))
  copied <- file.copy(config$source, destination, overwrite = FALSE,
                      copy.mode = FALSE, copy.date = FALSE)
  if (!isTRUE(copied)) {
    unlink(root, recursive = TRUE, force = TRUE)
    .biofeedback_abort(
      "could not copy local video into isolated resources",
      "PhysioStream_video_sync_resource_error"
    )
  }
  copied_info <- file.info(destination)
  copied_hash <- digest::digest(
    file = destination, algo = "sha256", serialize = FALSE
  )
  if (!identical(as.numeric(copied_info$size), config$size) ||
      !identical(copied_hash, config$sha256)) {
    unlink(root, recursive = TRUE, force = TRUE)
    .biofeedback_abort(
      "isolated local video copy failed integrity verification",
      "PhysioStream_video_sync_resource_error"
    )
  }
  prefix <- paste0(
    "physiostream-video-",
    substr(
      digest::digest(
        paste(root, config$sha256, sep = "|"),
        algo = "sha256", serialize = FALSE
      ),
      1L, 20L
    )
  )
  shiny::addResourcePath(prefix, root)
  list(
    kind = "local",
    source = paste0("/", prefix, "/", basename(destination)),
    sync = config$sync,
    cleanup = list(prefix = prefix, root = root)
  )
}

.video_cleanup_shiny <- function(config) {
  if (is.null(config) || is.null(config$cleanup)) {
    return(invisible(NULL))
  }
  try(shiny::removeResourcePath(config$cleanup$prefix), silent = TRUE)
  unlink(config$cleanup$root, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

#' Launch a synchronized video and signal viewer
#'
#' `launch = FALSE` returns a side-effect-free `BiofeedbackScope` carrying a
#' validated video descriptor. Local media are copied and exposed only when the
#' Shiny viewer starts.
#'
#' @inheritParams biofeedbackScope
#' @param video Local mp4/webm/ogg path, HTTPS URL, or `"webcam"`.
#' @param sync A `VideoSync` in the source clock domain.
#' @return A `BiofeedbackScope` or the result of `shiny::runApp()`.
#' @examples
#' \dontrun{
#' # Requires a Shiny session and a local video file for display.
#' info <- streamInfo("demo", type = "EEG",
#'                    channel_names = c("left", "right"), nominal_srate = 100,
#'                    channel_units = c("uV", "uV"))
#' source <- streamOpen(loopbackSource(info, capacity = 256L))
#' sync <- videoSync(c(a = 100, b = 110, c = 120), c(a = 1, b = 11, c = 21),
#'                   frame_rate = 30, method = "offset", clock_domain = "local")
#' viewer <- videoSyncViewer(source, "session.mp4", sync, launch = FALSE)
#' }
#' @export
videoSyncViewer <- function(
    source,
    video,
    sync,
    pipeline = NULL,
    channels = NULL,
    window_seconds = 10,
    update_hz = 20,
    max_points = 2000L,
    source_lifecycle = c("borrow", "own"),
    launch = interactive(),
    host = "127.0.0.1",
    port = NULL,
    browser = interactive(),
    clock = NULL) {
  .video_sync_assert(sync)
  if (!methods::is(source, "StreamSource") ||
      !identical(unclass(sync)$clock_domain, streamInfo(source)@clock_domain)) {
    .biofeedback_abort(
      "`sync` clock domain must exactly match the source",
      "PhysioStream_video_sync_validation_error"
    )
  }
  launch <- .biofeedback_logical(launch, "launch")
  scope <- biofeedbackScope(
    source = source,
    pipeline = pipeline,
    channels = channels,
    derived = list(),
    window_seconds = window_seconds,
    update_hz = update_hz,
    max_points = max_points,
    source_lifecycle = source_lifecycle,
    launch = FALSE,
    host = host,
    port = port,
    browser = browser,
    clock = clock
  )
  scope$video <- .video_config(video, sync)
  class(scope) <- c(
    "VideoSyncViewer", "BiofeedbackScope", "BiofeedbackRuntime"
  )
  if (launch) {
    return(.biofeedback_launch(scope))
  }
  scope
}

#' @export
print.VideoSync <- function(x, ...) {
  .video_sync_assert(x)
  payload <- unclass(x)
  cat(sprintf(
    "<VideoSync: method=%s, anchors=%d, rate=%.9f, max residual=%.6f frames>\n",
    payload$method, length(payload$signal_anchor), payload$rate,
    payload$max_abs_residual_frames
  ))
  invisible(x)
}
