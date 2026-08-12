# Snapshot an open numeric stream

Original timestamps and monotonic stream sequence identity are retained.
Selection is non-consuming unless `consume = TRUE`; consuming a newest
subset while older unread rows remain is rejected.

## Usage

``` r
streamSnapshot(
  x,
  n = NULL,
  duration_seconds = NULL,
  consume = FALSE,
  assay_name = "stream"
)
```

## Arguments

- x:

  An open buffered `StreamSource`.

- n:

  Optional number of newest rows.

- duration_seconds:

  Optional duration of the newest closed timestamp interval.

- consume:

  Whether to consume the selected oldest contiguous unread rows.

- assay_name:

  Non-empty assay name.

## Value

A valid
[PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioCore/reference/PhysioExperiment.html).
