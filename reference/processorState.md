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

## Examples

``` r
filt <- lmsFilter(n_taps = 3L, step_size = 0.05)
update(filt, matrix(1:8, 4L, 2L), reference = matrix(1:4, 4L, 1L))
#> $output
#>      channel_1 channel_2
#> [1,]     1.000     5.000
#> [2,]     1.900     5.500
#> [3,]     2.090     4.050
#> [4,]     0.665    -0.075
#> 
#> $timestamps
#> NULL
#> 
#> $n_input
#> [1] 4
#> 
#> $n_emitted
#> [1] 4
#> 
#> $sequence_start
#> [1] 1
#> 
#> $sequence_end
#> [1] 4
#> 
#> $state_sha256
#> [1] "060535cb615ce7f8c07938d7632f8f7a51ecb2cd2bf10bd884adb49d89dedd51"
#> 
#> $diagnostics
#> $diagnostics$signal
#>      [,1] [,2]
#> [1,]    1    5
#> [2,]    2    6
#> [3,]    3    7
#> [4,]    4    8
#> 
#> $diagnostics$estimate
#>      channel_1 channel_2
#> [1,]     0.000     0.000
#> [2,]     0.100     0.500
#> [3,]     0.910     2.950
#> [4,]     3.335     8.075
#> 
#> $diagnostics$residual
#>      channel_1 channel_2
#> [1,]     1.000     5.000
#> [2,]     1.900     5.500
#> [3,]     2.090     4.050
#> [4,]     0.665    -0.075
#> 
#> $diagnostics$weights
#>         [,1]    [,2]
#> [1,] 0.68650 1.39250
#> [2,] 0.40375 0.66875
#> [3,] 0.17100 0.19500
#> 
#> $diagnostics$coefficient_norm
#> [1] 0.814578 1.557019
#> 
#> $diagnostics$reference_power
#> [1] 7.5 7.5
#> 
#> $diagnostics$residual_power
#> channel_1 channel_2 
#>  2.355081 17.914531 
#> 
#> $diagnostics$converged
#> [1] TRUE TRUE
#> 
#> 
#> $schema
#> [1] "1.0.0"
#> 
processorState(filt)$n_samples
#> [1] 4
processorReset(filt)
processorState(filt)$n_samples
#> [1] 0
```
