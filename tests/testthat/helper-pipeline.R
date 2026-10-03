pipeline_state_raw <- function(x) {
  serialize(pipelineState(x), NULL, version = 3L)
}

pipeline_identity_callback <- function(chunk, state, context) {
  state$count <- state$count + 1
  list(
    output = chunk,
    state = state,
    events = list(),
    diagnostics = list(operation = context$operation_name)
  )
}

pipeline_reference_sos_rms <- function(samples, sos, window_samples,
                                       state = NULL) {
  if (is.null(state)) {
    state <- list(
      zi = array(0, c(nrow(sos), 2L, ncol(samples))),
      buffer = matrix(0, window_samples, ncol(samples)),
      sums = numeric(ncol(samples)),
      cursor = 1L,
      filled = 0L
    )
  }
  output <- matrix(0, nrow(samples), ncol(samples))
  available <- logical(nrow(samples))
  for (row in seq_len(nrow(samples))) {
    next_filled <- min(window_samples, state$filled + 1L)
    available[[row]] <- next_filled == window_samples
    for (channel in seq_len(ncol(samples))) {
      value <- samples[row, channel]
      for (section in seq_len(nrow(sos))) {
        filtered <- sos[section, 1L] * value +
          state$zi[section, 1L, channel]
        next_z1 <- sos[section, 2L] * value -
          sos[section, 5L] * filtered +
          state$zi[section, 2L, channel]
        next_z2 <- sos[section, 3L] * value -
          sos[section, 6L] * filtered
        state$zi[section, 1L, channel] <- next_z1
        state$zi[section, 2L, channel] <- next_z2
        value <- filtered
      }
      square <- value^2
      old <- state$buffer[state$cursor, channel]
      state$sums[[channel]] <- state$sums[[channel]] - old + square
      state$buffer[state$cursor, channel] <- square
      output[row, channel] <- sqrt(
        max(state$sums[[channel]], 0) / next_filled
      )
    }
    state$cursor <- state$cursor %% window_samples + 1L
    state$filled <- next_filled
  }
  list(rms = output, available = available, state = state)
}

pipeline_test_sos <- function() {
  PhysioPreprocess::sosDesign(
    low = 20, high = 100, order = 2L, type = "pass", sr = 512
  )
}
