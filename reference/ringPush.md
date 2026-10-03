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

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 8L)
ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
ringFill(buffer)
#> [1] 2
```
