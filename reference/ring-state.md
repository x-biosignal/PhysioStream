# Ring-buffer capacity and occupancy

Ring-buffer capacity and occupancy

## Usage

``` r
ringCapacity(x)

ringFill(x)

ringStats(x)
```

## Arguments

- x:

  A live `RingBuffer`.

## Value

`ringCapacity()` and `ringFill()` return integer scalars. `ringStats()`
returns a serializable named list.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 8L)
ringPush(buffer, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
ringCapacity(buffer)
#> [1] 8
ringFill(buffer)
#> [1] 3
ringStats(buffer)$fill
#> [1] 3
```
