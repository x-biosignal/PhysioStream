# Construct a deterministic governed loopback trigger

The loopback backend records commands and timing without opening a
device or network connection. It exercises the same arm, dead-man,
maximum, duplicate ID, and refractory gates as live transports.

## Usage

``` r
loopbackTrigger(
  allowed_channels,
  max_intensity,
  intensity_unit,
  max_duration_ms,
  refractory_ms,
  deadman_ms,
  audit_capacity = 4096L,
  clock = NULL
)
```

## Arguments

- allowed_channels:

  Unique allowed channel identifiers.

- max_intensity:

  Positive scalar or exactly channel-named maxima.

- intensity_unit:

  Explicit device intensity unit; no conversion occurs.

- max_duration_ms:

  Positive scalar or channel-named duration maxima.

- refractory_ms:

  Non-negative scalar or channel-named refractory times.

- deadman_ms:

  Positive heartbeat expiry interval.

- audit_capacity:

  Bounded command/audit capacity.

- clock:

  Optional deterministic monotonic clock for tests.

## Value

A closed, disarmed `LoopbackTrigger`.

## Examples

``` r
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
triggerState(trigger)$lifecycle$status
#> [1] "closed"
```
