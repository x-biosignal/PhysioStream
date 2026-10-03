sync_test_pe <- function(times, rate = 100, values = NULL,
                         clock_times = NULL, clock_values = NULL,
                         segments = NULL) {
  if (is.null(values)) {
    values <- matrix(seq_along(times), ncol = 1L)
  }
  values <- as.matrix(values)
  row_data <- S4Vectors::DataFrame(
    xdf_time = as.numeric(times),
    xdf_time_raw = as.numeric(times)
  )
  if (!is.null(segments)) {
    row_data$xdf_segment <- as.integer(segments)
  }
  metadata <- list()
  if (!is.null(clock_times)) {
    metadata$xdf <- list(
      clock_times = as.numeric(clock_times),
      clock_values = as.numeric(clock_values),
      segments = if (is.null(segments)) {
        matrix(c(0L, length(times) - 1L), nrow = 1L)
      } else {
        runs <- rle(segments)$lengths
        ends <- cumsum(runs) - 1L
        cbind(c(0L, head(ends, -1L) + 1L), ends)
      }
    )
  }
  PhysioCore::PhysioExperiment(
    assays = list(raw = values),
    rowData = row_data,
    metadata = metadata,
    samplingRate = if (is.finite(rate) && rate > 0) rate else NA_real_
  )
}

sync_test_container <- function(streams, rate = 100) {
  PhysioCore::MultiRatePhysioExperiment(
    streams = streams, t0 = 0, reference_rate = rate,
    offsets = stats::setNames(rep(0, length(streams)), names(streams))
  )
}
