# Inspect governed trigger state

The returned deep plain-list snapshot contains no connection, Python
object, callback, or credential. Runtime environments are not portable.

## Usage

``` r
triggerState(trigger)
```

## Arguments

- trigger:

  A `TriggerBackend`.

## Value

A sealed plain list.

## Examples

``` r
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
triggerOpen(trigger)
triggerState(trigger)$arm$status
#> [1] "disarmed"
```
