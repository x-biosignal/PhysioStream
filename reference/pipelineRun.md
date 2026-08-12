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
