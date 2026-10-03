# Live bounded ring buffer

Copying a `RingBuffer` wrapper aliases the same native buffer. The
native queue is not serialized; a restored wrapper fails on its first
operation.

## Slots

- `ptr`:

  Protected native pointer.

- `info`:

  Immutable stream metadata.

- `capacity`:

  Fixed sample capacity.

- `schema_version`:

  Wrapper schema.

## Examples

``` r
info <- streamInfo("demo", type = "EEG",
                   channel_names = c("C3", "C4"), nominal_srate = 100)
buffer <- ringBuffer(info, capacity = 8L)
ringPush(buffer, matrix(as.double(1:4), 2, 2), c(0.01, 0.02))
ringFill(buffer)
#> [1] 2
```
