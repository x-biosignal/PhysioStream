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
