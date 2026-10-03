# Construct the compiled causal SOS plus rolling-RMS operation

The operation uses direct-form-II-transposed second-order sections
followed by a per-channel rolling root-mean-square. It is a descriptor
for
[`onChunk()`](https://x-biosignal.github.io/PhysioStream/reference/onChunk.md)
and performs no processing until registered and stepped.

## Usage

``` r
bandpassRmsOp(
  sos,
  window_samples,
  warmup = c("partial", "complete"),
  name = "bandpass_rms"
)
```

## Arguments

- sos:

  Finite second-order-section matrix with columns `b0,b1,b2,a0,a1,a2`.

- window_samples:

  Exact positive RMS window length.

- warmup:

  Whether to emit partial-window RMS values or only rows after a
  complete window exists.

- name:

  Default operation name.

## Value

A `PipelineOperation` descriptor.

## Examples

``` r
# A pass-through SOS keeps the example self-contained; use
# PhysioPreprocess::sosDesign() for a real band definition.
sos <- matrix(c(1, 0, 0, 1, 0, 0), 1L, 6L,
              dimnames = list(NULL, c("b0", "b1", "b2", "a0", "a1", "a2")))
op <- bandpassRmsOp(sos, window_samples = 4L)
pipeline <- streamPipeline(chunk_size = 8L)
onChunk(pipeline, op)
x <- matrix(sin(seq_len(16)), 8, 2, dimnames = list(NULL, c("C3", "C4")))
pipelineEnqueue(pipeline, x, ingest_time_ns = 0)
pipelineStep(pipeline)$results[[1]]$output$samples
#>         C3_rms    C4_rms
#> [1,] 0.8414710 0.4121185
#> [2,] 0.8760409 0.4825975
#> [3,] 0.7199097 0.6989948
#> [4,] 0.7293079 0.6621351
#> [5,] 0.7646931 0.6633985
#> [6,] 0.6305303 0.7819422
#> [7,] 0.7074585 0.6834889
#> [8,] 0.7758979 0.6449042
```
