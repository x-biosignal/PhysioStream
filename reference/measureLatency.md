# Measure governed ingest-to-emit pipeline latency

The default benchmark processes 60 seconds of deterministic 32-channel,
512-Hz data in 32-sample chunks through the compiled causal bandpass
plus rolling-RMS operation. Input generation and warmup are outside the
measured interval. Results describe empirical performance on the
recorded hardware, not a hard real-time guarantee.

## Usage

``` r
measureLatency(
  pipeline = NULL,
  n_channels = 32L,
  sampling_rate = 512,
  hop_samples = 32L,
  duration_s = 60,
  warmup_chunks = 64L,
  seed = 1L
)
```

## Arguments

- pipeline:

  Optional empty `StreamPipeline`. `NULL` constructs the reference
  bandpass-plus-RMS graph.

- n_channels:

  Exact positive channel count.

- sampling_rate:

  Finite positive sampling rate.

- hop_samples:

  Exact positive samples per chunk.

- duration_s:

  Finite positive duration whose sample count is divisible by
  `hop_samples`.

- warmup_chunks:

  Exact non-negative warmup chunk count.

- seed:

  Exact non-negative local random seed.

## Value

A plain benchmark result with summary, latency rows, pipeline state,
hardware, configuration, and reference hash.

## Examples

``` r
# Tiny deterministic benchmark; sampling_rate * duration_s must divide
# evenly by hop_samples.
bench <- measureLatency(n_channels = 2L, sampling_rate = 100,
                        hop_samples = 10L, duration_s = 0.5,
                        warmup_chunks = 1L, seed = 1L)
bench$summary$count
#> [1] 5
```
