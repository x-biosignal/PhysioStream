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
