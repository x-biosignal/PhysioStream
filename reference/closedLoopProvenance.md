# Append a stopped closed-loop session to PhysioExperiment provenance

The bounded log records detection, queue, and acknowledgement evidence.
Acknowledgement is explicitly not a physical-delivery claim. Raw samples
and trigger runtime values are excluded.

## Usage

``` r
closedLoopProvenance(controller, x, input_assay = NA_character_)
```

## Arguments

- controller:

  A stopped `ClosedLoopController`.

- x:

  A `PhysioExperiment`.

- input_assay:

  Exact existing assay name or `NA_character_`.

## Value

A modified `PhysioExperiment` with one provenance activity.
