xdf_fixture <- function(name) {
  system.file("extdata", paste0("xdf-", name, ".xdf"),
              package = "PhysioStream", mustWork = TRUE)
}

skip_without_pyxdf <- function() {
  available <- isTRUE(tryCatch(
    xdfAvailable(initialize = TRUE),
    error = function(e) FALSE,
    interrupt = function(e) FALSE
  ))
  testthat::skip_if_not(available, "pyxdf backend unavailable")
}

xdf_test_pe <- function(values, format, timestamps = NULL, rate = 10,
                        name = "test-stream", type = "test") {
  values <- as.matrix(values)
  n <- nrow(values)
  if (is.null(colnames(values))) {
    colnames(values) <- paste0("channel_", seq_len(ncol(values)))
  }
  if (is.null(timestamps)) {
    timestamps <- if (n) seq(1, by = 1 / rate, length.out = n) else numeric()
  }
  PhysioCore::PhysioExperiment(
    assays = list(xdf = values),
    rowData = S4Vectors::DataFrame(xdf_time = timestamps),
    colData = S4Vectors::DataFrame(
      label = colnames(values),
      unit = rep("unit", ncol(values)),
      type = rep(type, ncol(values))
    ),
    metadata = list(xdf = list(
      name = name,
      type = type,
      source_id = paste0("source-", name),
      nominal_srate = rate,
      channel_format = format
    )),
    samplingRate = if (rate > 0) rate else NA_real_
  )
}
