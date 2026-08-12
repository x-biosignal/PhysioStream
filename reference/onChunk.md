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
