test_that("incremental PCA agrees with batch prcomp across chunks", {
  set.seed(73)
  latent <- matrix(rnorm(2400), 800L, 3L)
  mixing <- matrix(c(
    2, 0.4, 0.1, 0,
    0.2, 1.4, 0.3, 0.1,
    0, 0.2, 0.7, 0.2
  ), 3L, 4L, byrow = TRUE)
  samples <- latent %*% mixing
  colnames(samples) <- paste0("x", 1:4)
  processor <- incrementalPCA(3L)
  update(processor, samples[1:137, ])
  update(processor, samples[138:503, ])
  result <- update(processor, samples[504:800, ])
  expected <- stats::prcomp(samples, center = TRUE, scale. = FALSE)
  observed <- result$diagnostics$components
  singular <- svd(t(expected$rotation[, 1:3]) %*% observed)$d
  angle <- max(acos(pmin(1, pmax(-1, singular)))) * 180 / pi
  expect_lt(angle, 5)
  expect_equal(
    result$diagnostics$explained_variance,
    expected$sdev[1:3]^2,
    tolerance = 1e-11
  )
})

test_that("batch-equivalent PCA state is chunk-order invariant", {
  set.seed(74)
  samples <- matrix(rnorm(1000), 200L, 5L)
  whole <- incrementalPCA(3L)
  split <- incrementalPCA(3L)
  whole_result <- update(whole, samples)
  update(split, samples[1:61, ])
  update(split, samples[62:149, ])
  split_result <- update(split, samples[150:200, ])
  expect_equal(
    whole_result$diagnostics$components,
    split_result$diagnostics$components,
    tolerance = 2e-13
  )
  expect_equal(
    processorState(whole)$M2, processorState(split)$M2,
    tolerance = 2e-12
  )
})

test_that("PCA reports degenerate eigenspaces", {
  samples <- rbind(diag(2), -diag(2))
  result <- update(incrementalPCA(2L), samples)
  expect_true(result$diagnostics$degenerate)
})
