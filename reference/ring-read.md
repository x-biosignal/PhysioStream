# Pull or inspect buffered samples

Pull or inspect buffered samples

## Usage

``` r
ringPull(x, n = ringFill(x))

ringPeek(x, n = ringFill(x), from = "oldest")
```

## Arguments

- x:

  A live `RingBuffer`.

- n:

  Non-negative exact number of rows. Requests larger than the current
  fill return all available rows.

- from:

  Exact peek side, either `"oldest"` or `"latest"`.

## Value

A named list with `samples`, `timestamps`, `sequence`, `count`, and
`stats_after`.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 8L)
ringPush(buffer, matrix(as.double(1:6), 3, 2), c(0.01, 0.02, 0.03))
ringPeek(buffer, 2L)$samples
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
ringPull(buffer)$sequence
#> [1] 0 1 2
```
