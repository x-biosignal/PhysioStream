# Construct a governed MQTT stimulation-command trigger

Uses MQTT v5, Paho callback API VERSION2, a caller-thread manual network
loop, QoS 0 or 1, and `retain = FALSE`. QoS 1 can duplicate a command,
so the receiver must deduplicate exact command IDs. No exactly-once or
physical delivery claim is made.

## Usage

``` r
mqttTrigger(
  host,
  topic,
  client_id,
  allowed_channels,
  max_intensity,
  intensity_unit,
  max_duration_ms,
  refractory_ms,
  deadman_ms,
  port = 8883L,
  qos = 1L,
  tls = c("required", "disabled"),
  allow_insecure_localhost = FALSE,
  username = NULL,
  password = NULL,
  connect_timeout_ms = 5000,
  publish_timeout_ms = 1000,
  audit_capacity = 4096L,
  clock = NULL
)
```

## Arguments

- host:

  Broker hostname or IP literal.

- topic:

  Concrete publish topic without wildcards.

- client_id:

  Bounded MQTT client identifier.

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

  Integer broker port.

- qos:

  Exact MQTT QoS, 0 or 1.

- tls:

  Exact `"required"` or `"disabled"`.

- allow_insecure_localhost:

  Permit plaintext only for a loopback host.

- username:

  Optional broker username, excluded from portable state.

- password:

  Optional broker password, excluded from portable state.

- connect_timeout_ms:

  Positive bounded connect timeout.

- publish_timeout_ms:

  Positive bounded acknowledgement timeout.

- audit_capacity:

  Bounded command/audit capacity.

- clock:

  Optional deterministic monotonic clock for tests.

## Value

A closed, disarmed `MqttTrigger`.

## Details

TLS is required by default. Plaintext is restricted to an explicitly
acknowledged loopback-only test broker.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires a reachable MQTT broker; see loopbackTrigger() for an
# offline equivalent with the same interlocks.
trigger <- mqttTrigger(host = "127.0.0.1", port = 1883,
                       topic = "stim/commands",
                       allowed_channels = "left", max_intensity = 20,
                       intensity_unit = "mA", max_duration_ms = 500,
                       refractory_ms = 0, deadman_ms = 1000)
} # }
```
