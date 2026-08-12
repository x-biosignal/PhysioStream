# Send a governed stimulation command

Validation, arm/dead-man, channel maximum, duration, duplicate-ID, and
refractory gates run before transport invocation. Once invocation
begins, command identity and refractory time are conservatively reserved
even when acknowledgement is unknown. Commands are never retried
automatically.

## Usage

``` r
sendStim(trigger, intensity, channel, duration_ms, command_id, now_ns = NULL)
```

## Arguments

- trigger:

  A `TriggerBackend`.

- intensity:

  Finite non-negative intensity in the configured unit.

- channel:

  Exact configured channel identifier.

- duration_ms:

  Finite positive command duration in milliseconds.

- command_id:

  Unique bounded command identifier.

- now_ns:

  Optional exact process-local monotonic nanosecond value.

## Value

A deep plain-list command receipt.

## Details

MQTT acknowledgement means broker/client acceptance; TTL acknowledgement
means adapter completion. Neither status proves physical stimulation.
Receipts and audit records bind lifecycle and arm generations,
validation, attempt, and acknowledgement times, and bounded error
class/code. A caller interrupt after transport invocation first
finalizes an `unknown` audit record and then re-signals the interrupt.
