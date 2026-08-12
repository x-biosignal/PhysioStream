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
