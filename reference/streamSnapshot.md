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
[PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html).

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
src <- streamOpen(loopbackSource(info, capacity = 32L))
loopbackFeed(src, matrix(sin(seq_len(10)), 5, 2),
             c(0.01, 0.02, 0.03, 0.04, 0.05))
pe <- streamSnapshot(src)
dim(pe)
#> [1] 5 2
```
