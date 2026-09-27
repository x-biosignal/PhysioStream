# Read an Extensible Data Format file

XDF timestamps are authoritative in `rowData(x)$xdf_time`; the regular
`streamTimeIndex()` grid of the returned multi-rate container is only a
nominal convenience for jittered or irregular streams. Files and
converted payloads are also bounded by a private 512 MiB in-memory
ceiling even when `max_file_bytes` is larger.

## Usage

``` r
readXDF(
  path,
  streams = NULL,
  synchronize = TRUE,
  dejitter = TRUE,
  keep_raw_timestamps = TRUE,
  max_file_bytes = 2^31 - 1,
  backend = getOption("PhysioStream.xdf_backend", "auto")
)
```

## Arguments

- path:

  Existing local `.xdf` file.

- streams:

  Optional unique stream ids or unambiguous stream names.

- synchronize:

  Whether pyxdf clock synchronization is applied.

- dejitter:

  Whether pyxdf timestamp de-jittering is applied.

- keep_raw_timestamps:

  Whether a second uncorrected pass is retained.

- max_file_bytes:

  Exact positive file-size ceiling.

- backend:

  Exact configured backend.

## Value

A valid
[PhysioCore::MultiRatePhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/MultiPhysioExperiment.html).
