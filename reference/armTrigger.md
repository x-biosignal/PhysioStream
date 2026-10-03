# Arm, heartbeat, disarm, or emergency-stop a trigger

Every trigger opens disarmed. Dead-man expiry disarms locally and
requires a new explicit arm with a different session ID within the open
lifecycle. Emergency stop latches locally even if the transport stop
attempt fails. These software gates do not replace device hardware
limits or an independent emergency stop.

## Usage

``` r
armTrigger(trigger, session_id, now_ns = NULL)

triggerHeartbeat(trigger, now_ns = NULL)

disarmTrigger(trigger, reason = "caller", now_ns = NULL)

emergencyStop(trigger, reason = "caller", now_ns = NULL)
```

## Arguments

- trigger:

  A `TriggerBackend`.

- session_id:

  Unique non-empty session identifier.

- now_ns:

  Optional exact process-local monotonic nanosecond value.

- reason:

  Bounded non-sensitive reason code.

## Value

The trigger, invisibly.

## Examples

``` r
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
triggerOpen(trigger)
armTrigger(trigger, "session-1", now_ns = 0)
triggerState(trigger)$arm$status
#> [1] "armed"
```
