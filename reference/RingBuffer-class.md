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
