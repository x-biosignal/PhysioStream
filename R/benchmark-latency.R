.pipeline_latency_frame <- function(latency) {
  data.frame(
    sequence_start = latency$sequence_start,
    sequence_end = latency$sequence_end,
    ingest_ns = latency$ingest_ns,
    process_start_ns = latency$process_start_ns,
    process_end_ns = latency$process_end_ns,
    emit_ns = latency$emit_ns,
    queue_wait_ms = latency$queue_wait_ms,
    processing_ms = latency$processing_ms,
    end_to_end_ms = latency$end_to_end_ms,
    budget_exceeded = latency$budget_exceeded,
    stringsAsFactors = FALSE
  )
}

.pipeline_latency_summary <- function(latency, budget_ms, counters) {
  values <- latency$end_to_end_ms
  if (!length(values)) {
    return(list(
      count = 0,
      minimum_ms = numeric(),
      mean_ms = numeric(),
      p50_ms = numeric(),
      p95_ms = numeric(),
      p99_ms = numeric(),
      maximum_ms = numeric(),
      budget_ms = budget_ms,
      exceeded = 0,
      exceed_rate = 0,
      dropped_chunks = counters$dropped_chunks,
      dropped_samples = counters$dropped_samples
    ))
  }
  quantiles <- stats::quantile(
    values, probs = c(0.5, 0.95, 0.99), names = FALSE, type = 7
  )
  list(
    count = length(values),
    minimum_ms = min(values),
    mean_ms = mean(values),
    p50_ms = quantiles[[1L]],
    p95_ms = quantiles[[2L]],
    p99_ms = quantiles[[3L]],
    maximum_ms = max(values),
    budget_ms = budget_ms,
    exceeded = sum(values > budget_ms),
    exceed_rate = mean(values > budget_ms),
    dropped_chunks = counters$dropped_chunks,
    dropped_samples = counters$dropped_samples
  )
}

.pipeline_hardware <- function() {
  info <- Sys.info()
  scalar_or_na <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      NA_character_
    } else {
      as.character(x)
    }
  }
  cpu_model <- NA_character_
  if (file.exists("/proc/cpuinfo")) {
    lines <- tryCatch(readLines("/proc/cpuinfo", warn = FALSE),
                      error = function(e) character())
    hit <- grep("^model name[[:space:]]*:", lines, value = TRUE)
    if (length(hit)) {
      cpu_model <- sub("^[^:]+:[[:space:]]*", "", hit[[1L]])
    }
  }
  package_version <- tryCatch(
    as.character(utils::packageVersion("PhysioStream")),
    error = function(e) NA_character_
  )
  compiler <- tryCatch(
    scalar_or_na(system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "config", "CXX17"),
      stdout = TRUE, stderr = FALSE
    )[[1L]]),
    error = function(e) NA_character_
  )
  list(
    sysname = scalar_or_na(unname(info[["sysname"]])),
    release = scalar_or_na(unname(info[["release"]])),
    machine = scalar_or_na(unname(info[["machine"]])),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    compiler = compiler,
    cpu_model = cpu_model,
    container = file.exists("/.dockerenv"),
    package_version = package_version,
    clock = "std::chrono::steady_clock"
  )
}

.pipeline_reference_sos <- function(sampling_rate) {
  PhysioPreprocess::sosDesign(
    low = sampling_rate * 0.04, high = sampling_rate * 0.195,
    order = 4L, type = "pass", sr = sampling_rate
  )
}

.pipeline_benchmark_input <- function(n_samples, n_channels, sampling_rate,
                                      seed) {
  .dsp_with_seed(seed, {
    time <- (seq_len(n_samples) - 1) / sampling_rate
    samples <- matrix(
      stats::rnorm(n_samples * n_channels, sd = 0.15),
      nrow = n_samples, ncol = n_channels
    )
    for (channel in seq_len(n_channels)) {
      samples[, channel] <- samples[, channel] +
        sin(2 * pi * (25 + channel %% 12) * time) +
        0.25 * sin(2 * pi * 90 * time + channel / 10)
    }
    colnames(samples) <- sprintf("channel_%02d", seq_len(n_channels))
    samples
  })
}

#' Measure governed ingest-to-emit pipeline latency
#'
#' The default benchmark processes 60 seconds of deterministic 32-channel,
#' 512-Hz data in 32-sample chunks through the compiled causal bandpass plus
#' rolling-RMS operation. Input generation and warmup are outside the measured
#' interval. Results describe empirical performance on the recorded hardware,
#' not a hard real-time guarantee.
#'
#' @param pipeline Optional empty `StreamPipeline`. `NULL` constructs the
#'   reference bandpass-plus-RMS graph.
#' @param n_channels Exact positive channel count.
#' @param sampling_rate Finite positive sampling rate.
#' @param hop_samples Exact positive samples per chunk.
#' @param duration_s Finite positive duration whose sample count is divisible
#'   by `hop_samples`.
#' @param warmup_chunks Exact non-negative warmup chunk count.
#' @param seed Exact non-negative local random seed.
#' @return A plain benchmark result with summary, latency rows, pipeline state,
#'   hardware, configuration, and reference hash.
#' @examples
#' # Tiny deterministic benchmark; sampling_rate * duration_s must divide
#' # evenly by hop_samples.
#' bench <- measureLatency(n_channels = 2L, sampling_rate = 100,
#'                         hop_samples = 10L, duration_s = 0.5,
#'                         warmup_chunks = 1L, seed = 1L)
#' bench$summary$count
#' @export
measureLatency <- function(
    pipeline = NULL,
    n_channels = 32L,
    sampling_rate = 512,
    hop_samples = 32L,
    duration_s = 60,
    warmup_chunks = 64L,
    seed = 1L) {
  n_channels <- .dsp_scalar(
    n_channels, "n_channels", lower = 1, upper = 65536, integer = TRUE
  )
  sampling_rate <- .dsp_scalar(
    sampling_rate, "sampling_rate", lower = 0, lower_open = TRUE
  )
  hop_samples <- .dsp_scalar(
    hop_samples, "hop_samples", lower = 1, upper = 1048576, integer = TRUE
  )
  duration_s <- .dsp_scalar(
    duration_s, "duration_s", lower = 0, lower_open = TRUE
  )
  warmup_chunks <- .dsp_scalar(
    warmup_chunks, "warmup_chunks", lower = 0,
    upper = .Machine$integer.max, integer = TRUE
  )
  seed <- .dsp_scalar(
    seed, "seed", lower = 0, upper = .Machine$integer.max, integer = TRUE
  )
  total_samples <- sampling_rate * duration_s
  if (!is.finite(total_samples) || total_samples != floor(total_samples) ||
      total_samples > 2^31 - 1 ||
      total_samples %% hop_samples != 0) {
    .pipeline_abort(
      "`sampling_rate * duration_s` must be an exact sample count divisible by `hop_samples`",
      "PhysioStream_pipeline_validation_error"
    )
  }
  total_samples <- as.integer(total_samples)
  n_chunks <- as.integer(total_samples / hop_samples)
  warmup_samples <- as.double(warmup_chunks) * hop_samples
  if (total_samples + warmup_samples > .Machine$integer.max) {
    .pipeline_abort(
      "benchmark sample count exceeds R matrix dimension limits",
      "PhysioStream_pipeline_resource_error"
    )
  }
  bytes <- (as.double(total_samples) + warmup_samples) *
    as.double(n_channels) * 8
  if (!is.finite(bytes) || bytes > .dsp_allocation_limit) {
    .pipeline_abort(
      "benchmark input exceeds the 512 MiB allocation ceiling",
      "PhysioStream_pipeline_resource_error"
    )
  }

  if (is.null(pipeline)) {
    pipeline <- streamPipeline(
      chunk_size = hop_samples,
      queue_capacity = 2L,
      backpressure = "error",
      latency_budget_ms = 50
    )
    onChunk(
      pipeline,
      bandpassRmsOp(
        .pipeline_reference_sos(sampling_rate),
        window_samples = 128L,
        warmup = "partial"
      )
    )
  } else {
    .pipeline_assert(pipeline)
    if (length(pipeline$state$queue)) {
      .pipeline_abort(
        "benchmark pipeline queue must be empty",
        "PhysioStream_pipeline_validation_error"
      )
    }
    if (!length(pipeline$state$operations)) {
      .pipeline_abort(
        "benchmark pipeline must contain at least one operation",
        "PhysioStream_pipeline_validation_error"
      )
    }
    if (pipeline$state$configuration$chunk_size < hop_samples) {
      .pipeline_abort(
        "benchmark hop exceeds the supplied pipeline chunk size",
        "PhysioStream_pipeline_validation_error"
      )
    }
  }

  all_samples <- .pipeline_benchmark_input(
    total_samples + warmup_samples, n_channels, sampling_rate, seed
  )
  if (warmup_chunks > 0L) {
    for (chunk_index in seq_len(warmup_chunks)) {
      rows <- ((chunk_index - 1L) * hop_samples + 1L):
        (chunk_index * hop_samples)
      pipelineEnqueue(pipeline, all_samples[rows, , drop = FALSE])
      pipelineStep(pipeline, 1L)
    }
  }
  pipelineReset(pipeline, keep_operations = TRUE)
  offset <- warmup_samples
  for (chunk_index in seq_len(n_chunks)) {
    rows <- offset + ((chunk_index - 1L) * hop_samples + 1L):
      (chunk_index * hop_samples)
    pipelineEnqueue(pipeline, all_samples[rows, , drop = FALSE])
    pipelineStep(pipeline, 1L)
  }
  state <- pipelineState(pipeline)
  summary <- .pipeline_latency_summary(
    state$latency, state$configuration$latency_budget_ms, state$counters
  )
  configuration <- list(
    n_channels = n_channels,
    sampling_rate = sampling_rate,
    hop_samples = hop_samples,
    duration_s = duration_s,
    n_chunks = n_chunks,
    samples_per_channel = total_samples,
    warmup_chunks = warmup_chunks,
    seed = seed
  )
  reference_sha256 <- digest::digest(
    serialize(
      list(configuration = configuration,
           operations = lapply(state$operations, function(operation) {
             list(
               name = operation$name,
               kind = operation$kind,
               type = operation$type,
               configuration = operation$configuration
             )
           })),
      NULL, version = 3L
    ),
    algo = "sha256",
    serialize = FALSE
  )
  list(
    summary = summary,
    latencies = .pipeline_latency_frame(state$latency),
    pipeline_state = state,
    hardware = .pipeline_hardware(),
    configuration = configuration,
    reference_sha256 = reference_sha256,
    schema = .pipeline_schema_version
  )
}
