# Test or describe the configured MQTT backend

The conservative availability probe does not select or install Python.
Initialization is explicit and uses only the interpreter already
configured for reticulate.

## Usage

``` r
mqttAvailable(initialize = FALSE)

mqttBackendInfo()
```

## Arguments

- initialize:

  Whether the configured Python interpreter may be initialized to verify
  pinned Paho.

## Value

`mqttAvailable()` returns one logical value. `mqttBackendInfo()` returns
a plain capability list.

## Examples

``` r
# Reports FALSE unless the pinned Paho MQTT backend is installed.
mqttAvailable()
#> [1] FALSE
```
