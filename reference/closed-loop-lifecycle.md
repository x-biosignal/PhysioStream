# Run a governed closed-loop session

The controller is synchronous and terminal. Start explicitly opens and
arms its trigger. Step processes one committed chunk and then dispatches
due actions. Flush dispatches due delayed actions without ingesting
samples. Stop and emergency stop suppress pending actions and close the
trigger.

## Usage

``` r
closedLoopStart(controller, session_id, now_ns = NULL)

closedLoopStep(controller, samples, timestamps = NULL, now_ns = NULL)

closedLoopFlush(controller, now_ns = NULL)

closedLoopStop(controller, reason = "caller", now_ns = NULL)

closedLoopEmergencyStop(controller, reason = "caller", now_ns = NULL)
```

## Arguments

- controller:

  A `ClosedLoopController`.

- session_id:

  New bounded trigger session identifier.

- now_ns:

  Optional exact test monotonic nanosecond value.

- samples:

  Finite sample-by-channel chunk.

- timestamps:

  Optional signal-domain timestamps.

- reason:

  Bounded non-sensitive stop reason.

## Value

Start/stop functions return `controller` invisibly. Step and flush
return bounded plain result lists.
