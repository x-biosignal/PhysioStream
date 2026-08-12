# Validated stream metadata

Validated stream metadata

## Usage

``` r
# S4 method for class 'StreamInfo'
show(object)
```

## Arguments

- object:

  A `StreamInfo` object to display.

## Slots

- `name,type,source_id,clock_domain`:

  Scalar stream identity strings.

- `n_channels`:

  Exact positive channel count.

- `channel_names,channel_units`:

  Channel labels and optional units.

- `nominal_srate`:

  Nominal samples per second; zero denotes an irregular stream reserved
  for later transports.

- `dtype`:

  Exact storage type.

- `metadata`:

  Named recursively serializable metadata.

- `schema_version`:

  Metadata schema identifier.
