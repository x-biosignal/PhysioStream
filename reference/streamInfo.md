# Construct or retrieve stream metadata

With a character first argument, constructs validated stream metadata.
With a stream endpoint or ring buffer, retrieves its immutable
`StreamInfo`.

## Usage

``` r
streamInfo(name, ...)

# S4 method for class 'character'
streamInfo(
  name,
  type,
  channel_names,
  nominal_srate,
  dtype = c("float64", "float32", "int32", "int16", "int8", "string"),
  source_id = "",
  clock_domain = "local",
  channel_units = NULL,
  metadata = list(),
  ...
)

# S4 method for class 'StreamInfo'
streamInfo(name, ...)

# S4 method for class 'StreamEndpoint'
streamInfo(name, ...)

# S4 method for class 'RingBuffer'
streamInfo(name, ...)
```

## Arguments

- name:

  A non-empty stream name, or a stream object when used as an accessor.

- ...:

  Reserved for methods.

- type:

  A non-empty stream type.

- channel_names:

  Unique non-empty channel labels.

- nominal_srate:

  A finite nominal sampling rate greater than or equal to zero.

- dtype:

  Exact storage type.

- source_id:

  Optional stable source identifier.

- clock_domain:

  Non-empty clock-domain label.

- channel_units:

  Optional units matching `channel_names`.

- metadata:

  Named recursively serializable metadata, at most 1 MiB.

## Value

A `StreamInfo` object, or an endpoint's `StreamInfo`.

## Examples

``` r
info <- streamInfo("eeg-demo", type = "EEG",
                   channel_names = c("C3", "Cz", "C4"),
                   nominal_srate = 250, channel_units = rep("uV", 3))
info
#> StreamInfo<1.0.0>: eeg-demo
#>   type: EEG; channels: 3; rate: 250 Hz; dtype: float64
streamChannels(info)
#> [1] "C3" "Cz" "C4"
```
