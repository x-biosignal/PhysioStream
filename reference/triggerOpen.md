# Open or close a governed stimulation trigger

Trigger constructors are side-effect free. `triggerOpen()` explicitly
opens the configured transport and leaves it disarmed. `triggerClose()`
performs a bounded best-effort stop/deassert before closing. Software
interlocks are additional safeguards only; independently fail-safe
stimulation hardware is required.

## Usage

``` r
triggerOpen(trigger)

triggerClose(trigger)
```

## Arguments

- trigger:

  A `TriggerBackend`.

## Value

The trigger, invisibly.

## Examples

``` r
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
triggerOpen(trigger)
triggerState(trigger)$lifecycle$status
#> [1] "open"
triggerClose(trigger)
```
