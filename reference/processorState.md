# Inspect or reset governed streaming processor state

`processorState()` returns a deep, portable copy of the numeric
processor state. `processorReset()` clears learned and delay state,
increments the reset counter, and can retain the bound channel identity.

## Usage

``` r
processorState(object, ...)

processorReset(object, keep_channels = TRUE, ...)
```

## Arguments

- object:

  A `StreamProcessor`.

- ...:

  Reserved for methods.

- keep_channels:

  Whether to retain channel identity across reset.

## Value

`processorState()` returns a plain list. `processorReset()` returns
`object` invisibly.
