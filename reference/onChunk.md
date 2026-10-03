# Register a causal per-chunk operation

The callback is invoked as `callback(chunk, state, context)` and must
return a list containing `output`, `state`, `events`, and `diagnostics`.
Operation states are committed only after the full graph succeeds.

## Usage

``` r
onChunk(pipeline, callback, state = NULL, name = NULL, kind = "filter")
```

## Arguments

- pipeline:

  A `StreamPipeline`.

- callback:

  A callback function or a `PipelineOperation` descriptor.

- state:

  Initial bounded plain-list state for a custom callback.

- name:

  Unique non-empty operation name.

- kind:

  Exact operation kind.

## Value

`pipeline`, invisibly.

## Examples

``` r
sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
              dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
pipeline <- streamPipeline(chunk_size = 8L)
onChunk(pipeline, bandpassRmsOp(sos, window_samples = 4L))
length(pipelineState(pipeline)$operations)
#> [1] 1
```
