# Construct a governed synchronous stream-processing pipeline

A pipeline owns a bounded FIFO of whole chunks and an ordered callback
graph. Processing occurs synchronously on the caller's R thread. The
runtime environment is not portable; use
[`pipelineState()`](https://x-biosignal.github.io/PhysioStream/reference/pipeline-state.md)
for a hashed plain-state snapshot.

## Usage

``` r
streamPipeline(
  source = NULL,
  chunk_size = 32L,
  queue_capacity = 64L,
  backpressure = c("error", "drop_oldest", "drop_newest"),
  latency_budget_ms = 50
)
```

## Arguments

- source:

  Optional `StreamSource`. Caller-driven pipelines use `NULL`.

- chunk_size:

  Maximum samples per enqueued chunk.

- queue_capacity:

  Maximum number of whole chunks in the ingress FIFO.

- backpressure:

  Exact full-queue policy.

- latency_budget_ms:

  Finite positive empirical latency budget.

## Value

A `StreamPipeline` environment.
