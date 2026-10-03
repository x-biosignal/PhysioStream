# Pull from a configured source and process synchronously

The source must already be open. The function never starts a background
thread and stops on an empty pull rather than busy-spinning.

## Usage

``` r
pipelineRun(pipeline, max_chunks = Inf, timeout = 0)
```

## Arguments

- pipeline:

  A source-backed `StreamPipeline`.

- max_chunks:

  Maximum chunks to process; `Inf` is allowed.

- timeout:

  Finite non-negative overall run time in seconds. Zero means no
  wall-time limit for the current call.

## Value

A plain list of processed results and stop diagnostics.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = "C3", nominal_srate = 100)
src <- streamOpen(loopbackSource(info, capacity = 64L))
loopbackFeed(src, matrix(sin(seq_len(16)), 16, 1), seq_len(16) / 100)
sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
              dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
pipeline <- streamPipeline(source = src, chunk_size = 8L)
onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
run <- pipelineRun(pipeline)
run$n_processed
#> [1] 2
```
