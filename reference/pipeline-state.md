# Inspect or reset governed pipeline state

Inspect or reset governed pipeline state

## Usage

``` r
pipelineState(pipeline)

pipelineReset(pipeline, keep_operations = TRUE)
```

## Arguments

- pipeline:

  A `StreamPipeline`.

- keep_operations:

  Whether reset retains the registered graph and resets each operation
  to its initial state.

## Value

`pipelineState()` returns a deep plain list; `pipelineReset()` returns
the pipeline invisibly.
