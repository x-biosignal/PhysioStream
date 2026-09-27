# Write an Extensible Data Format file

The writer implements the governed XDF 1.0 subset natively and does not
require Python. Output is scanned before an atomic same-directory
rename.

## Usage

``` r
writeXDF(
  x,
  path,
  overwrite = FALSE,
  chunk_samples = 256L,
  timestamps = c("output", "raw")
)
```

## Arguments

- x:

  One
  [PhysioCore::MultiRatePhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/MultiPhysioExperiment.html)
  or
  [PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html).

- path:

  Destination `.xdf` path in an existing directory.

- overwrite:

  Whether an existing regular destination may be replaced.

- chunk_samples:

  Exact positive samples per XDF Samples chunk.

- timestamps:

  Exact timestamp domain, `"output"` or `"raw"`.

## Value

The normalized output path, invisibly.
