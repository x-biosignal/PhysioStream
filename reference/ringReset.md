# Reset unread ring-buffer state

Reset clears unread rows and the last-timestamp constraint while
preserving lifetime counters and monotonic sequence identity.

## Usage

``` r
ringReset(x)
```

## Arguments

- x:

  A live `RingBuffer`.

## Value

Updated serializable ring statistics, invisibly.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 8L)
ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
ringReset(buffer)
ringFill(buffer)
#> [1] 0
```
