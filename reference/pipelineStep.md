# Process queued chunks synchronously

Process queued chunks synchronously

## Usage

``` r
pipelineStep(pipeline, n = 1L)
```

## Arguments

- pipeline:

  A `StreamPipeline`.

- n:

  Maximum whole chunks to process.

## Value

A plain list containing per-chunk results and counts. Each result
includes `event_sources`, a plain operation identity parallel to
`events`.

## Examples

``` r
sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
              dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
pipeline <- streamPipeline(chunk_size = 8L)
onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
x <- matrix(sin(seq_len(16)), 8, 2, dimnames = list(NULL, c("C3", "C4")))
pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
pipelineStep(pipeline)$n_processed
#> [1] 1
```
