# Enqueue one bounded input chunk

Full-queue behavior is governed by the pipeline's exact backpressure
policy. Empty chunks are no-ops and consume neither capacity nor
sequence identity.

## Usage

``` r
pipelineEnqueue(pipeline, samples, timestamps = NULL, ingest_time_ns = NULL)
```

## Arguments

- pipeline:

  A `StreamPipeline`.

- samples:

  Finite sample-by-channel values.

- timestamps:

  Optional strictly increasing row-matched timestamps.

- ingest_time_ns:

  Optional process-local monotonic nanosecond stamp.

## Value

`pipeline`, invisibly.
