.pca_reset <- function(current, keep_channels) {
  features <- if (keep_channels && !is.null(current$channel_names)) {
    length(current$channel_names)
  } else {
    current$config$n_features
  }
  channels <- if (keep_channels) current$channel_names else NULL
  list(
    algorithm = "incremental_pca",
    config = current$config,
    channel_names = channels,
    n_samples = 0,
    n_chunks = 0,
    reset_count = current$reset_count + 1,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    effective_n = 0,
    mean = if (is.null(features)) NULL else rep(0, features),
    M2 = if (is.null(features)) NULL else matrix(0, features, features),
    components = NULL,
    explained_variance = NULL,
    explained_ratio = NULL,
    singular_values = NULL,
    degenerate = FALSE
  )
}

#' Incremental principal-component analysis
#'
#' For `forgetting = 1`, the processor retains Chan-Golub-LeVeque sufficient
#' statistics and is batch-equivalent without retaining samples. Smaller
#' forgetting factors produce an explicitly exponentially weighted covariance.
#'
#' @param n_components Number of retained components.
#' @param n_features Optional feature count to bind at construction.
#' @param forgetting Per-sample covariance forgetting factor in `(0, 1]`.
#' @param center Whether to estimate and subtract a running mean.
#' @return A mutable `IncrementalPCA` streaming processor.
#' @examples
#' set.seed(1)
#' latent <- matrix(rnorm(300), ncol = 3)
#' samples <- latent %*% matrix(rnorm(12), 3, 4)
#' pca <- incrementalPCA(n_components = 2L)
#' result <- update(pca, samples)
#' result$diagnostics$explained_variance
#' @export
incrementalPCA <- function(n_components, n_features = NULL, forgetting = 1,
                           center = TRUE) {
  n_components <- .dsp_scalar(
    n_components, "n_components", 1, .Machine$integer.max, integer = TRUE
  )
  if (!is.null(n_features)) {
    n_features <- .dsp_scalar(
      n_features, "n_features", 1, .Machine$integer.max, integer = TRUE
    )
    if (n_components > n_features) {
      .dsp_abort(
        "`n_components` cannot exceed `n_features`",
        "PhysioStream_dsp_validation_error"
      )
    }
    bytes <- 8 * as.double(n_features)^2
    if (!is.finite(bytes) || bytes > .dsp_state_limit / 2) {
      .dsp_abort(
        "incremental PCA covariance exceeds the governed state ceiling",
        "PhysioStream_dsp_resource_error"
      )
    }
  }
  forgetting <- .dsp_scalar(
    forgetting, "forgetting", 0, 1, lower_open = TRUE
  )
  center <- .dsp_logical(center, "center")
  config <- list(
    n_components = n_components,
    n_features = n_features,
    forgetting = forgetting,
    center = center
  )
  state <- list(
    algorithm = "incremental_pca",
    config = config,
    channel_names = NULL,
    n_samples = 0,
    n_chunks = 0,
    reset_count = 0,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    effective_n = 0,
    mean = if (is.null(n_features)) NULL else rep(0, n_features),
    M2 = if (is.null(n_features)) NULL else matrix(0, n_features, n_features),
    components = NULL,
    explained_variance = NULL,
    explained_ratio = NULL,
    singular_values = NULL,
    degenerate = FALSE
  )
  .dsp_new_processor("IncrementalPCA", state, .pca_reset)
}

.pca_orient <- function(vectors) {
  for (j in seq_len(ncol(vectors))) {
    anchor <- which.max(abs(vectors[, j]))
    if (vectors[anchor, j] < 0) {
      vectors[, j] <- -vectors[, j]
    }
  }
  vectors
}

.pca_decompose <- function(state) {
  if (state$n_samples < 2 || state$effective_n <= 1) {
    state$components <- NULL
    state$explained_variance <- NULL
    state$explained_ratio <- NULL
    state$singular_values <- NULL
    state$degenerate <- FALSE
    return(state)
  }
  denominator <- if (state$config$forgetting == 1) {
    state$n_samples - 1
  } else {
    state$effective_n - 1
  }
  covariance <- (state$M2 + t(state$M2)) / (2 * denominator)
  decomposition <- eigen(covariance, symmetric = TRUE)
  scale <- max(1, max(abs(decomposition$values)))
  tolerance <- 1024 * .Machine$double.eps * scale
  if (min(decomposition$values) < -tolerance) {
    .dsp_abort(
      "incremental PCA covariance is not positive semidefinite",
      "PhysioStream_dsp_numeric_error"
    )
  }
  values <- pmax(decomposition$values, 0)
  vectors <- .pca_orient(decomposition$vectors)
  k <- state$config$n_components
  state$components <- vectors[, seq_len(k), drop = FALSE]
  state$explained_variance <- values[seq_len(k)]
  total <- sum(values)
  state$explained_ratio <- if (total > 0) {
    values[seq_len(k)] / total
  } else {
    rep(0, k)
  }
  state$singular_values <- sqrt(
    pmax(0, state$explained_variance * denominator)
  )
  if (length(values) > 1L) {
    gaps <- abs(diff(values))
    pair_scale <- pmax(1, abs(head(values, -1L)), abs(tail(values, -1L)))
    state$degenerate <- any(gaps <= 1024 * .Machine$double.eps * pair_scale)
  } else {
    state$degenerate <- FALSE
  }
  state
}

#' @export
update.IncrementalPCA <- function(object, samples, reference = NULL,
                                  timestamps = NULL, ...) {
  .dsp_assert_processor(object)
  if (!is.null(reference)) {
    .dsp_abort(
      "`reference` is not used by incremental PCA",
      "PhysioStream_dsp_validation_error"
    )
  }
  old <- .dsp_deep_copy(object$state)
  chunk <- .dsp_validate_chunk(old, samples, timestamps)
  if (chunk$empty) {
    return(.dsp_empty_result(
      old, if (is.null(old$components)) 0L else ncol(old$components)
    ))
  }
  state <- old
  features <- ncol(chunk$samples)
  if (is.null(state$channel_names)) {
    if (!is.null(state$config$n_features) &&
        features != state$config$n_features) {
      .dsp_abort(
        "first chunk does not match configured `n_features`",
        "PhysioStream_dsp_channel_error"
      )
    }
    if (state$config$n_components > features) {
      .dsp_abort(
        "`n_components` cannot exceed the bound feature count",
        "PhysioStream_dsp_channel_error"
      )
    }
    bytes <- 8 * as.double(features)^2
    if (!is.finite(bytes) || bytes > .dsp_state_limit / 2) {
      .dsp_abort(
        "incremental PCA covariance exceeds the governed state ceiling",
        "PhysioStream_dsp_resource_error"
      )
    }
    state$channel_names <- chunk$names
    state$mean <- rep(0, features)
    state$M2 <- matrix(0, features, features)
  }
  n_chunk <- nrow(chunk$samples)
  if (state$config$forgetting == 1) {
    if (state$config$center) {
      chunk_mean <- colMeans(chunk$samples)
      centered <- sweep(chunk$samples, 2L, chunk_mean)
      chunk_M2 <- crossprod(centered)
      if (state$n_samples == 0) {
        state$mean <- chunk_mean
        state$M2 <- chunk_M2
      } else {
        delta <- chunk_mean - state$mean
        total <- state$n_samples + n_chunk
        state$M2 <- state$M2 + chunk_M2 +
          tcrossprod(delta) * state$n_samples * n_chunk / total
        state$mean <- state$mean + delta * n_chunk / total
      }
    } else {
      state$mean[] <- 0
      state$M2 <- state$M2 + crossprod(chunk$samples)
    }
    state$effective_n <- state$n_samples + n_chunk
  } else {
    lambda <- state$config$forgetting
    for (i in seq_len(n_chunk)) {
      x <- chunk$samples[i, ]
      old_weight <- state$effective_n
      new_weight <- lambda * old_weight + 1
      if (state$config$center) {
        delta <- x - state$mean
        new_mean <- state$mean + delta / new_weight
        state$M2 <- lambda * state$M2 +
          tcrossprod(delta, x - new_mean)
        state$mean <- new_mean
      } else {
        state$M2 <- lambda * state$M2 + tcrossprod(x)
        state$mean[] <- 0
      }
      state$effective_n <- new_weight
    }
  }
  state$n_samples <- state$n_samples + n_chunk
  state$n_chunks <- state$n_chunks + 1
  supplied_timestamps <- !is.null(chunk$timestamps)
  if (is.null(state$timestamp_mode)) {
    state$timestamp_mode <- supplied_timestamps
  } else if (!identical(state$timestamp_mode, supplied_timestamps)) {
    .dsp_abort(
      "timestamp presence cannot change after the first nonempty chunk",
      "PhysioStream_dsp_timestamp_error"
    )
  }
  if (supplied_timestamps) {
    state$last_timestamp <- tail(chunk$timestamps, 1L)
  }
  state <- .pca_decompose(state)
  if (is.null(state$components)) {
    scores <- matrix(numeric(), nrow = n_chunk, ncol = 0L)
    centered <- if (state$config$center) {
      sweep(chunk$samples, 2L, state$mean)
    } else {
      chunk$samples
    }
    reconstruction <- sqrt(rowSums(centered^2))
  } else {
    centered <- if (state$config$center) {
      sweep(chunk$samples, 2L, state$mean)
    } else {
      chunk$samples
    }
    scores <- centered %*% state$components
    reconstruction <- sqrt(rowSums(
      (centered - scores %*% t(state$components))^2
    ))
    colnames(scores) <- sprintf("PC%d", seq_len(ncol(scores)))
  }
  diagnostics <- list(
    components = state$components,
    explained_variance = state$explained_variance,
    explained_ratio = state$explained_ratio,
    singular_values = state$singular_values,
    mean = state$mean,
    sample_count = state$n_samples,
    effective_count = state$effective_n,
    reconstruction_residual = reconstruction,
    degenerate = state$degenerate,
    batch_equivalent = state$config$forgetting == 1
  )
  .dsp_commit(
    object, old, state, scores, chunk$timestamps,
    n_input = n_chunk, n_emitted = n_chunk, diagnostics = diagnostics
  )
}

.dsp_with_seed <- function(seed, expression) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expression)
}

.ica_initial_unmixing <- function(n_components, n_features, seed) {
  .dsp_with_seed(seed, {
    candidate <- matrix(
      stats::rnorm(n_features * n_components),
      nrow = n_features, ncol = n_components
    )
    t(qr.Q(qr(candidate), complete = FALSE))
  })
}

.ica_reset <- function(current, keep_channels) {
  features <- if (keep_channels && !is.null(current$channel_names)) {
    length(current$channel_names)
  } else {
    current$config$n_features
  }
  channels <- if (keep_channels) current$channel_names else NULL
  components <- if (is.null(features)) NULL else
    if (is.null(current$config$n_components)) features
    else current$config$n_components
  list(
    algorithm = "online_ica",
    config = current$config,
    channel_names = channels,
    n_samples = 0,
    n_chunks = 0,
    reset_count = current$reset_count + 1,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    effective_n = 0,
    mean = if (is.null(features)) NULL else rep(0, features),
    covariance = if (is.null(features)) NULL else diag(1, features),
    whitening = if (is.null(features)) NULL else diag(1, features),
    unmixing = if (is.null(features)) NULL else
      .ica_initial_unmixing(components, features, current$config$seed),
    source_m2 = if (is.null(components)) NULL else rep(0, components),
    source_m4 = if (is.null(components)) NULL else rep(0, components),
    source_effective = 0,
    source_sign = if (is.null(components)) NULL else rep(1, components),
    block_count = 0,
    pending = if (is.null(features)) NULL else matrix(numeric(), 0L, features),
    pending_sequences = numeric(),
    pending_timestamps = NULL,
    whitening_floor = 0,
    whitening_condition = 1
  )
}

#' Online recursive independent-component analysis
#'
#' The processor combines exponentially weighted whitening with a natural
#' gradient update and symmetric decorrelation. Reported components have a
#' deterministic order and sign, but physiological source identity is not
#' inferred. Only complete update blocks are emitted; an incomplete block is
#' retained with its timestamps and sequence identity until a later update.
#'
#' @param n_components Optional number of sources; defaults to all features.
#' @param n_features Optional feature count to bind at construction.
#' @param learning_rate Positive natural-gradient step size.
#' @param forgetting Per-sample whitening forgetting factor in `(0, 1]`.
#' @param nonlinearity Exact `tanh` or `extended` score rule.
#' @param block_size Exact positive update block size.
#' @param orthogonalize_every Exact positive number of blocks between symmetric
#'   decorrelations.
#' @param seed Exact integer used only for deterministic initialization.
#' @return A mutable `OnlineICA` streaming processor.
#' @examples
#' set.seed(1)
#' sources <- cbind(sin(seq_len(200) / 5), sign(sin(seq_len(200) / 7)))
#' samples <- sources %*% t(matrix(c(1, 0.4, 0.3, 1), 2, 2))
#' ica <- onlineICA(n_components = 2L, block_size = 10L)
#' result <- update(ica, samples)
#' dim(result$output)
#' @export
onlineICA <- function(n_components = NULL, n_features = NULL,
                      learning_rate = 0.01, forgetting = 0.995,
                      nonlinearity = c("tanh", "extended"),
                      block_size = 1L, orthogonalize_every = 1L,
                      seed = 1L) {
  if (!is.null(n_components)) {
    n_components <- .dsp_scalar(
      n_components, "n_components", 1, .Machine$integer.max, integer = TRUE
    )
  }
  if (!is.null(n_features)) {
    n_features <- .dsp_scalar(
      n_features, "n_features", 1, .Machine$integer.max, integer = TRUE
    )
    if (!is.null(n_components) && n_components > n_features) {
      .dsp_abort(
        "`n_components` cannot exceed `n_features`",
        "PhysioStream_dsp_validation_error"
      )
    }
    if (8 * as.double(n_features)^2 > .dsp_state_limit / 3) {
      .dsp_abort(
        "online ICA covariance exceeds the governed state ceiling",
        "PhysioStream_dsp_resource_error"
      )
    }
  }
  learning_rate <- .dsp_scalar(
    learning_rate, "learning_rate", 0, Inf, lower_open = TRUE
  )
  forgetting <- .dsp_scalar(
    forgetting, "forgetting", 0, 1, lower_open = TRUE
  )
  nonlinearity <- .dsp_enum(nonlinearity[[1L]], c("tanh", "extended"),
                            "nonlinearity")
  block_size <- .dsp_scalar(
    block_size, "block_size", 1, .Machine$integer.max, integer = TRUE
  )
  orthogonalize_every <- .dsp_scalar(
    orthogonalize_every, "orthogonalize_every", 1,
    .Machine$integer.max, integer = TRUE
  )
  seed <- .dsp_scalar(
    seed, "seed", -.Machine$integer.max, .Machine$integer.max, integer = TRUE
  )
  config <- list(
    n_components = n_components,
    n_features = n_features,
    learning_rate = learning_rate,
    forgetting = forgetting,
    nonlinearity = nonlinearity,
    block_size = block_size,
    orthogonalize_every = orthogonalize_every,
    seed = seed
  )
  state <- list(
    algorithm = "online_ica",
    config = config,
    channel_names = NULL,
    n_samples = 0,
    n_chunks = 0,
    reset_count = 0,
    last_timestamp = NULL,
    timestamp_mode = NULL,
    effective_n = 0,
    mean = if (is.null(n_features)) NULL else rep(0, n_features),
    covariance = if (is.null(n_features)) NULL else diag(1, n_features),
    whitening = if (is.null(n_features)) NULL else diag(1, n_features),
    unmixing = NULL,
    source_m2 = NULL,
    source_m4 = NULL,
    source_effective = 0,
    source_sign = NULL,
    block_count = 0,
    pending = if (is.null(n_features)) NULL else
      matrix(numeric(), 0L, n_features),
    pending_sequences = numeric(),
    pending_timestamps = NULL,
    whitening_floor = 0,
    whitening_condition = 1
  )
  if (!is.null(n_features)) {
    components <- if (is.null(n_components)) n_features else n_components
    state$unmixing <- .ica_initial_unmixing(components, n_features, seed)
    state$source_m2 <- rep(0, components)
    state$source_m4 <- rep(0, components)
    state$source_sign <- rep(1, components)
  }
  .dsp_new_processor("OnlineICA", state, .ica_reset)
}

.ica_whitening <- function(covariance) {
  eig <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
  largest <- max(eig$values, .Machine$double.eps)
  floor <- max(largest * 1e-10, .Machine$double.eps)
  adjusted <- pmax(eig$values, floor)
  list(
    matrix = diag(1 / sqrt(adjusted), length(adjusted)) %*% t(eig$vectors),
    floor = max(pmax(floor - eig$values, 0)) / largest,
    condition = max(adjusted) / min(adjusted)
  )
}

.ica_row_rank <- function(unmixing) {
  gram <- tcrossprod(unmixing)
  eig <- eigen((gram + t(gram)) / 2, symmetric = TRUE)
  tolerance <- 1024 * .Machine$double.eps *
    max(1, max(abs(eig$values)))
  if (min(eig$values) <= tolerance) {
    .dsp_abort(
      "online ICA unmixing matrix lost row rank",
      "PhysioStream_dsp_numeric_error"
    )
  }
  eig
}

.ica_decorrelate <- function(unmixing) {
  eig <- .ica_row_rank(unmixing)
  eig$vectors %*% diag(1 / sqrt(eig$values), length(eig$values)) %*%
    t(eig$vectors) %*% unmixing
}

.ica_reporting <- function(sources, unmixing, m2, m4, effective) {
  normalized_m2 <- m2 / max(effective, .Machine$double.eps)
  normalized_m4 <- m4 / max(effective, .Machine$double.eps)
  excess <- normalized_m4 /
    pmax(normalized_m2^2, .Machine$double.eps) - 3
  order <- order(abs(excess), decreasing = TRUE, method = "radix")
  sources <- sources[, order, drop = FALSE]
  unmixing <- unmixing[order, , drop = FALSE]
  signs <- rep(1, nrow(unmixing))
  for (i in seq_len(nrow(unmixing))) {
    anchor <- which.max(abs(unmixing[i, ]))
    if (unmixing[i, anchor] < 0) {
      signs[[i]] <- -1
    }
  }
  list(
    sources = sweep(sources, 2L, signs, `*`),
    unmixing = unmixing * signs,
    order = order,
    signs = signs,
    excess_kurtosis = excess[order]
  )
}

#' @export
update.OnlineICA <- function(object, samples, reference = NULL,
                             timestamps = NULL, ...) {
  .dsp_assert_processor(object)
  if (!is.null(reference)) {
    .dsp_abort(
      "`reference` is not used by online ICA",
      "PhysioStream_dsp_validation_error"
    )
  }
  old <- .dsp_deep_copy(object$state)
  chunk <- .dsp_validate_chunk(old, samples, timestamps)
  if (chunk$empty) {
    return(.dsp_empty_result(
      old, if (is.null(old$unmixing)) 0L else nrow(old$unmixing)
    ))
  }
  state <- old
  features <- ncol(chunk$samples)
  if (is.null(state$channel_names)) {
    if (!is.null(state$config$n_features) &&
        features != state$config$n_features) {
      .dsp_abort(
        "first chunk does not match configured `n_features`",
        "PhysioStream_dsp_channel_error"
      )
    }
    components <- if (is.null(state$config$n_components)) {
      features
    } else {
      state$config$n_components
    }
    if (components > features) {
      .dsp_abort(
        "`n_components` cannot exceed the bound feature count",
        "PhysioStream_dsp_channel_error"
      )
    }
    if (8 * as.double(features)^2 > .dsp_state_limit / 3) {
      .dsp_abort(
        "online ICA covariance exceeds the governed state ceiling",
        "PhysioStream_dsp_resource_error"
      )
    }
    state$channel_names <- chunk$names
    state$mean <- rep(0, features)
    state$covariance <- diag(1, features)
    state$whitening <- diag(1, features)
    state$unmixing <- .ica_initial_unmixing(
      components, features, state$config$seed
    )
    state$source_m2 <- rep(0, components)
    state$source_m4 <- rep(0, components)
    state$source_effective <- 0
    state$source_sign <- rep(1, components)
    state$block_count <- 0
    state$pending <- matrix(numeric(), 0L, features)
    state$pending_sequences <- numeric()
    state$pending_timestamps <- NULL
    state$whitening_condition <- 1
  }
  supplied_timestamps <- !is.null(chunk$timestamps)
  if (is.null(state$timestamp_mode)) {
    state$timestamp_mode <- supplied_timestamps
  } else if (!identical(state$timestamp_mode, supplied_timestamps)) {
    .dsp_abort(
      "timestamp presence cannot change after the first nonempty chunk",
      "PhysioStream_dsp_timestamp_error"
    )
  }
  new_sequences <- old$n_samples + seq_len(nrow(chunk$samples))
  all_blocks <- rbind(state$pending, chunk$samples)
  all_sequences <- c(state$pending_sequences, new_sequences)
  all_timestamps <- if (supplied_timestamps) {
    c(state$pending_timestamps, chunk$timestamps)
  } else {
    NULL
  }
  block_size <- state$config$block_size
  n_blocks <- nrow(all_blocks) %/% block_size
  raw_sources <- vector("list", n_blocks)
  emitted_sequences <- numeric()
  emitted_timestamps <- if (supplied_timestamps) numeric() else NULL
  whitening <- list(
    matrix = state$whitening,
    floor = state$whitening_floor,
    condition = 1
  )
  lambda <- state$config$forgetting
  if (n_blocks) {
    for (block in seq_len(n_blocks)) {
      index <- seq.int((block - 1L) * block_size + 1L, block * block_size)
      raw_block <- all_blocks[index, , drop = FALSE]
      for (i in seq_len(block_size)) {
        x <- raw_block[i, ]
        weight <- lambda * state$effective_n + 1
        delta <- x - state$mean
        new_mean <- state$mean + delta / weight
        state$covariance <- lambda * state$covariance +
          tcrossprod(delta, x - new_mean)
        state$mean <- new_mean
        state$effective_n <- weight
      }
      covariance <- state$covariance / max(state$effective_n - 1, 1)
      whitening <- .ica_whitening(covariance)
      raw_unmixing <- state$unmixing %*% state$whitening
      rebased <- tryCatch(
        raw_unmixing %*% solve(whitening$matrix),
        error = function(e) NULL
      )
      if (is.null(rebased) || any(!is.finite(rebased))) {
        .dsp_abort(
          "online ICA could not rebase its whitening coordinates",
          "PhysioStream_dsp_numeric_error"
        )
      }
      .ica_row_rank(rebased)
      state$unmixing <- rebased
      state$whitening <- whitening$matrix
      state$whitening_floor <- whitening$floor
      state$whitening_condition <- whitening$condition
      z <- sweep(raw_block, 2L, state$mean) %*% t(state$whitening)
      y <- z %*% t(state$unmixing)
      if (state$config$nonlinearity == "tanh") {
        g <- tanh(y)
      } else {
        g <- sweep(tanh(y), 2L, state$source_sign, `*`)
      }
      gradient <- diag(nrow(state$unmixing)) -
        crossprod(g, y) / block_size
      state$unmixing <- state$unmixing +
        state$config$learning_rate * gradient %*% state$unmixing
      .ica_row_rank(state$unmixing)
      state$block_count <- state$block_count + 1
      if (state$block_count %% state$config$orthogonalize_every == 0L) {
        state$unmixing <- .ica_decorrelate(state$unmixing)
      }
      raw_sources[[block]] <- z %*% t(state$unmixing)
      emitted_sequences <- c(emitted_sequences, all_sequences[index])
      if (supplied_timestamps) {
        emitted_timestamps <- c(
          emitted_timestamps, all_timestamps[index]
        )
      }
      m2 <- colMeans(y^2)
      m4 <- colMeans(y^4)
      block_decay <- lambda^block_size
      state$source_m2 <- block_decay * state$source_m2 + m2
      state$source_m4 <- block_decay * state$source_m4 + m4
      state$source_effective <- block_decay * state$source_effective + 1
      if (state$config$nonlinearity == "extended") {
        excess <- m4 - 3 * m2^2
        state$source_sign <- ifelse(excess >= 0, 1, -1)
      }
      if (any(!is.finite(state$unmixing))) {
        .dsp_abort(
          "online ICA update became non-finite",
          "PhysioStream_dsp_numeric_error"
        )
      }
    }
  }
  consumed <- n_blocks * block_size
  state$pending <- if (consumed < nrow(all_blocks)) {
    all_blocks[seq.int(consumed + 1L, nrow(all_blocks)), , drop = FALSE]
  } else {
    matrix(numeric(), 0L, features)
  }
  state$pending_sequences <- if (consumed < length(all_sequences)) {
    all_sequences[seq.int(consumed + 1L, length(all_sequences))]
  } else {
    numeric()
  }
  state$pending_timestamps <- if (supplied_timestamps &&
                                  consumed < length(all_timestamps)) {
    all_timestamps[seq.int(consumed + 1L, length(all_timestamps))]
  } else {
    NULL
  }
  sources <- if (n_blocks) {
    do.call(rbind, raw_sources)
  } else {
    matrix(numeric(), 0L, nrow(state$unmixing))
  }
  raw_unmixing <- state$unmixing %*% state$whitening
  reported <- .ica_reporting(
    sources, raw_unmixing, state$source_m2, state$source_m4,
    state$source_effective
  )
  colnames(reported$sources) <- sprintf(
    "IC%d", seq_len(ncol(reported$sources))
  )
  state$n_samples <- state$n_samples + nrow(chunk$samples)
  state$n_chunks <- state$n_chunks + 1
  if (supplied_timestamps) {
    state$last_timestamp <- tail(chunk$timestamps, 1L)
  }
  gram <- tcrossprod(state$unmixing)
  diagnostics <- list(
    whitening = state$whitening,
    unmixing = reported$unmixing,
    raw_adaptive_unmixing = state$unmixing,
    nonlinearity_state = state$source_sign,
    condition_number = state$whitening_condition,
    whitening_floor_relative = state$whitening_floor,
    decorrelation_residual = max(abs(
      gram - diag(nrow(state$unmixing))
    )),
    effective_count = state$effective_n,
    reporting_order = reported$order,
    reporting_sign = reported$signs,
    excess_kurtosis = reported$excess_kurtosis,
    pending_samples = nrow(state$pending)
  )
  .dsp_commit(
    object, old, state, reported$sources, emitted_timestamps,
    n_input = nrow(chunk$samples), n_emitted = nrow(reported$sources),
    diagnostics = diagnostics,
    sequence_start = if (length(emitted_sequences)) {
      emitted_sequences[[1L]]
    } else {
      numeric()
    },
    sequence_end = if (length(emitted_sequences)) {
      tail(emitted_sequences, 1L)
    } else {
      numeric()
    }
  )
}
