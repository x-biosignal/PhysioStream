make_stream_info <- function(dtype = "float64", channels = c("C3", "C4"),
                             rate = 100) {
  streamInfo(
    "test-stream",
    type = "EEG",
    channel_names = channels,
    nominal_srate = rate,
    dtype = dtype,
    source_id = "fixture",
    channel_units = rep("uV", length(channels)),
    metadata = list(site = "synthetic", gain = 1)
  )
}

make_samples <- function(n, channels = 2L, offset = 0) {
  matrix(
    as.double(seq_len(n * channels) + offset),
    nrow = n,
    ncol = channels
  )
}

float32_roundtrip <- function(x) {
  con <- rawConnection(raw(), "w+b")
  on.exit(close(con), add = TRUE)
  writeBin(as.double(x), con, size = 4L, endian = .Platform$endian)
  seek(con, 0L)
  readBin(con, "double", n = length(x), size = 4L,
          endian = .Platform$endian)
}
