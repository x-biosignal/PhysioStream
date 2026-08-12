# Push samples into a ring buffer

The push is transactional. When unread rows are overwritten, the oldest
rows are dropped and counted. Matrices are always sample by channel.

## Usage

``` r
ringPush(x, samples, timestamps)
```

## Arguments

- x:

  A live `RingBuffer`.

- samples:

  A finite real matrix, sample by channel.

- timestamps:

  Strictly increasing finite timestamps.

## Value

Updated serializable ring statistics, invisibly.
