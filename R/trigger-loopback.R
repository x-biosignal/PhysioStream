#' Construct a deterministic governed loopback trigger
#'
#' The loopback backend records commands and timing without opening a device or
#' network connection. It exercises the same arm, dead-man, maximum, duplicate
#' ID, and refractory gates as live transports.
#'
#' @param allowed_channels Unique allowed channel identifiers.
#' @param max_intensity Positive scalar or exactly channel-named maxima.
#' @param intensity_unit Explicit device intensity unit; no conversion occurs.
#' @param max_duration_ms Positive scalar or channel-named duration maxima.
#' @param refractory_ms Non-negative scalar or channel-named refractory times.
#' @param deadman_ms Positive heartbeat expiry interval.
#' @param audit_capacity Bounded command/audit capacity.
#' @param clock Optional deterministic monotonic clock for tests.
#' @return A closed, disarmed `LoopbackTrigger`.
#' @examples
#' trigger <- loopbackTrigger(allowed_channels = "left", max_intensity = 20,
#'                            intensity_unit = "mA", max_duration_ms = 500,
#'                            refractory_ms = 0, deadman_ms = 1000)
#' triggerState(trigger)$lifecycle$status
#' @export
loopbackTrigger <- function(
    allowed_channels,
    max_intensity,
    intensity_unit,
    max_duration_ms,
    refractory_ms,
    deadman_ms,
    audit_capacity = 4096L,
    clock = NULL) {
  configuration <- .trigger_configuration(
    backend = "loopback",
    transport = "loopback",
    allowed_channels = allowed_channels,
    max_intensity = max_intensity,
    intensity_unit = intensity_unit,
    max_duration_ms = max_duration_ms,
    refractory_ms = refractory_ms,
    deadman_ms = deadman_ms,
    audit_capacity = audit_capacity
  )
  .trigger_new(
    "LoopbackTrigger", configuration, clock = clock
  )
}
