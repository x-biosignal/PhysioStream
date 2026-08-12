# Online recursive independent-component analysis

The processor combines exponentially weighted whitening with a natural
gradient update and symmetric decorrelation. Reported components have a
deterministic order and sign, but physiological source identity is not
inferred. Only complete update blocks are emitted; an incomplete block
is retained with its timestamps and sequence identity until a later
update.

## Usage

``` r
onlineICA(
  n_components = NULL,
  n_features = NULL,
  learning_rate = 0.01,
  forgetting = 0.995,
  nonlinearity = c("tanh", "extended"),
  block_size = 1L,
  orthogonalize_every = 1L,
  seed = 1L
)
```

## Arguments

- n_components:

  Optional number of sources; defaults to all features.

- n_features:

  Optional feature count to bind at construction.

- learning_rate:

  Positive natural-gradient step size.

- forgetting:

  Per-sample whitening forgetting factor in `(0, 1]`.

- nonlinearity:

  Exact `tanh` or `extended` score rule.

- block_size:

  Exact positive update block size.

- orthogonalize_every:

  Exact positive number of blocks between symmetric decorrelations.

- seed:

  Exact integer used only for deterministic initialization.

## Value

A mutable `OnlineICA` streaming processor.
