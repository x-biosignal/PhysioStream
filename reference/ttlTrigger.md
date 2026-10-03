# Construct a governed TTL stimulation-command trigger

TTL transport is an adapter boundary, not a vendor stimulator protocol.
Serial mode writes a versioned ASCII frame to a cooperating
independently fail-safe adapter. Callback mode requires one atomic timed
pulse operation; PhysioStream does not synthesize pulse timing with an R
sleep or busy loop. Loopback mode records the exact frame without
hardware.

## Usage

``` r
ttlTrigger(
  transport = c("serial", "callback", "loopback"),
  allowed_channels,
  max_intensity,
  intensity_unit,
  max_duration_ms,
  refractory_ms,
  deadman_ms,
  port = NULL,
  baud = 115200L,
  line = 1L,
  pulse_width_ms = 5,
  write_timeout_ms = 100,
  writer = NULL,
  audit_capacity = 4096L,
  clock = NULL
)
```

## Arguments

- transport:

  Exact `"serial"`, `"callback"`, or `"loopback"`.

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

- port:

  Serial port path for serial mode.

- baud:

  Positive integer serial baud rate.

- line:

  Positive integer adapter line identifier.

- pulse_width_ms:

  Positive pulse width requested from the adapter.

- write_timeout_ms:

  Positive bounded serial write timeout.

- writer:

  Callback implementing atomic `"pulse"` and fail-safe `"stop"` actions
  in callback mode.

- audit_capacity:

  Bounded command/audit capacity.

- clock:

  Optional deterministic monotonic clock for tests.

## Value

A closed, disarmed `TtlTrigger`.

## Details

Opening some serial ports can momentarily affect RTS/DTR control lines.
Hardware must remain fail-safe despite port open, process failure,
malformed frames, and communication loss.

## Examples

``` r
if (FALSE) { # \dontrun{
# Serial transport requires a device exposing a TTL line; see
# loopbackTrigger() for an offline equivalent with the same interlocks.
trigger <- ttlTrigger(transport = "serial", port = "/dev/ttyUSB0",
                      allowed_channels = "left", max_intensity = 20,
                      intensity_unit = "mA", max_duration_ms = 500,
                      refractory_ms = 0, deadman_ms = 1000)
} # }
```
