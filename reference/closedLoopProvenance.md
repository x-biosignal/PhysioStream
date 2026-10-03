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

## Examples

``` r
pipeline <- streamPipeline(chunk_size = 2048L)
trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
                           intensity_unit = "mA", max_duration_ms = 500,
                           refractory_ms = 0, deadman_ms = 1000)
detector <- emgOnsetOp("emg", sampling_rate = 1000,
                       baseline_samples = 30, rms_window_samples = 5)
controller <- closedLoop(pipeline, trigger, detector, intensity = 2,
                         stim_channel = "left", duration_ms = 10)
closedLoopStart(controller, "demo-session", now_ns = 0)
closedLoopStep(controller,
  matrix(rnorm(100, sd = 0.08), ncol = 1, dimnames = list(NULL, "emg")),
  now_ns = 1e6)
#> $pipeline
#> $pipeline$results
#> $pipeline$results[[1]]
#> $pipeline$results[[1]]$output
#> $pipeline$results[[1]]$output$samples
#>                 emg
#>   [1,] -0.049629334
#>   [2,]  0.003369270
#>   [3,] -0.072873732
#>   [4,]  0.012642302
#>   [5,] -0.052366772
#>   [6,]  0.141382982
#>   [7,]  0.057336598
#>   [8,]  0.072813938
#>   [9,]  0.030734829
#>  [10,]  0.134574086
#>  [11,] -0.050858916
#>  [12,] -0.036931578
#>  [13,]  0.114582579
#>  [14,] -0.052055708
#>  [15,] -0.016590459
#>  [16,] -0.031424634
#>  [17,] -0.025599429
#>  [18,] -0.022329064
#>  [19,]  0.039535067
#>  [20,] -0.014186439
#>  [21,] -0.040476597
#>  [22,]  0.107443106
#>  [23,] -0.017166353
#>  [24,] -0.014364522
#>  [25,] -0.008015259
#>  [26,]  0.057013305
#>  [27,] -0.005885152
#>  [28,] -0.003010734
#>  [29,] -0.054532838
#>  [30,] -0.025941622
#>  [31,]  0.004812835
#>  [32,] -0.047111559
#>  [33,]  0.042519695
#>  [34,] -0.121471527
#>  [35,]  0.024524629
#>  [36,] -0.122915986
#>  [37,] -0.024078090
#>  [38,] -0.042262392
#>  [39,] -0.052167582
#>  [40,] -0.004551742
#>  [41,] -0.153148754
#>  [42,]  0.094126665
#>  [43,] -0.133197795
#>  [44,] -0.037082432
#>  [45,] -0.089273608
#>  [46,] -0.060065520
#>  [47,]  0.166973324
#>  [48,]  0.001391650
#>  [49,] -0.102904042
#>  [50,] -0.131248443
#>  [51,]  0.036014968
#>  [52,] -0.001484787
#>  [53,] -0.025445470
#>  [54,] -0.074348972
#>  [55,] -0.118996825
#>  [56,] -0.086015384
#>  [57,]  0.080002304
#>  [58,] -0.049701336
#>  [59,] -0.110754148
#>  [60,]  0.149543250
#>  [61,]  0.034008030
#>  [62,] -0.019091768
#>  [63,]  0.084678644
#>  [64,]  0.070913812
#>  [65,] -0.049539444
#>  [66,]  0.176488197
#>  [67,] -0.020402162
#>  [68,] -0.113959572
#>  [69,] -0.011551968
#>  [70,]  0.016603067
#>  [71,]  0.184638272
#>  [72,]  0.008464189
#>  [73,]  0.036559904
#>  [74,] -0.006172235
#>  [75,] -0.026720067
#>  [76,] -0.002778082
#>  [77,]  0.063011168
#>  [78,]  0.166019601
#>  [79,]  0.082191395
#>  [80,]  0.096632672
#>  [81,] -0.098505874
#>  [82,]  0.078711646
#>  [83,]  0.017593984
#>  [84,] -0.117380002
#>  [85,]  0.041681819
#>  [86,] -0.012700368
#>  [87,]  0.117166985
#>  [88,] -0.061286560
#>  [89,] -0.034416940
#>  [90,] -0.074088760
#>  [91,] -0.014168317
#>  [92,]  0.032160942
#>  [93,] -0.058539854
#>  [94,]  0.066429853
#>  [95,] -0.096646623
#>  [96,] -0.083838753
#>  [97,]  0.115292617
#>  [98,] -0.081267797
#>  [99,]  0.032957977
#> [100,] -0.030486084
#> 
#> $pipeline$results[[1]]$output$sequence
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100
#> 
#> $pipeline$results[[1]]$output$ingest_time_ns
#> [1] 2277083384
#> 
#> $pipeline$results[[1]]$output$schema
#> [1] "1.0.0"
#> 
#> 
#> $pipeline$results[[1]]$events
#> list()
#> 
#> $pipeline$results[[1]]$event_sources
#> list()
#> 
#> $pipeline$results[[1]]$diagnostics
#> $pipeline$results[[1]]$diagnostics$closed_loop_emg_onset
#> $pipeline$results[[1]]$diagnostics$closed_loop_emg_onset$detector
#> [1] "emg_onset"
#> 
#> $pipeline$results[[1]]$diagnostics$closed_loop_emg_onset$calibrated
#> [1] TRUE
#> 
#> $pipeline$results[[1]]$diagnostics$closed_loop_emg_onset$active
#> [1] FALSE
#> 
#> $pipeline$results[[1]]$diagnostics$closed_loop_emg_onset$score
#>  [1]  0.34367186  1.30762365  1.21481880  1.21356873  0.44547892  0.40336316
#>  [7]  0.96041519  1.43686751  2.22267369  2.16143231  2.51229589  1.58096910
#> [13]  2.47695968  1.64730165  2.09675839  2.50822009  2.40870055  0.99429560
#> [19]  1.03336738  0.71449765  0.50520362  0.90927224  1.28755051  1.38867275
#> [25]  1.74188387  2.13604865  1.83932608  1.53194471  1.77319196  1.39557895
#> [31]  0.09170189  1.91287449  1.91537513  2.18801168  1.95964461  1.85421183
#> [37]  1.99607468  1.97969432  1.38601128  1.38078229  1.40476111 -1.58268207
#> [43] -0.93201882  1.18258911  1.55245404  1.98326932  2.42325675  2.51884626
#> [49]  1.18311414  1.56895154  1.14957393  0.54358544  1.03049119  1.23242872
#> [55]  0.42888669  0.68918716  0.69181264 -0.30656126 -0.33843434 -0.04324241
#> [61]  0.26876118  0.75018176  1.47236795  1.63930253  1.46436130  0.98361647
#> 
#> 
#> 
#> $pipeline$results[[1]]$latency
#> $pipeline$results[[1]]$latency$sequence_start
#> [1] 1
#> 
#> $pipeline$results[[1]]$latency$sequence_end
#> [1] 100
#> 
#> $pipeline$results[[1]]$latency$ingest_ns
#> [1] 2277083384
#> 
#> $pipeline$results[[1]]$latency$process_start_ns
#> [1] 2278683497
#> 
#> $pipeline$results[[1]]$latency$process_end_ns
#> [1] 2280138408
#> 
#> $pipeline$results[[1]]$latency$emit_ns
#> [1] 2280571528
#> 
#> $pipeline$results[[1]]$latency$queue_wait_ms
#> [1] 1.600113
#> 
#> $pipeline$results[[1]]$latency$processing_ms
#> [1] 1.454911
#> 
#> $pipeline$results[[1]]$latency$end_to_end_ms
#> [1] 3.488144
#> 
#> $pipeline$results[[1]]$latency$budget_exceeded
#> [1] FALSE
#> 
#> 
#> $pipeline$results[[1]]$state_sha256
#> [1] "54feab96bd3cbc6370ae0e5e5668542bd12d1a4102c189a30e175feb7dbb9761"
#> 
#> $pipeline$results[[1]]$schema
#> [1] "1.0.0"
#> 
#> 
#> 
#> $pipeline$n_processed
#> [1] 1
#> 
#> $pipeline$n_remaining
#> [1] 0
#> 
#> $pipeline$state_sha256
#> [1] "54feab96bd3cbc6370ae0e5e5668542bd12d1a4102c189a30e175feb7dbb9761"
#> 
#> $pipeline$schema
#> [1] "1.0.0"
#> 
#> 
#> $detections
#> list()
#> 
#> $actions
#> list()
#> 
#> $state_sha256
#> [1] "2a9f5a40c32c362a5b333ce9823fcc42f142f2d04ff9e83138e099c7d7a1ccb1"
#> 
#> $schema
#> [1] "physiostream.closed-loop/1.0.0"
#> 
closedLoopStop(controller, now_ns = 2e6)
pe <- PhysioExperiment(assays = list(raw = matrix(as.double(1:20), 10, 2)),
                       samplingRate = 100)
result <- closedLoopProvenance(controller, pe, "raw")
length(S4Vectors::metadata(result)$provenance)
#> [1] 1
```
