test_that("installed Shiny app exposes bounded scope controls and assets", {
  skip_if_not_installed("shiny")
  source <- biofeedback_test_source()
  scope <- biofeedbackScope(
    source,
    derived = list(target = list(
      type = "external", unit = "uM", target_range = c(-1, 1)
    )),
    launch = FALSE,
    clock = biofeedback_test_clock()
  )
  biofeedbackStart(scope)
  paths <- PhysioStream:::.biofeedback_shiny_paths()
  expect_true(all(file.exists(unlist(paths))))
  # node --check validates JS syntax; assert it EXITS 0 rather than requiring
  # silent output. macOS/Windows CI runners' node emits deprecation/experimental
  # warnings on stderr that are orthogonal to syntax validity, so expect_silent
  # is not portable here.
  node_bin <- Sys.which("node")
  skip_if(!nzchar(node_bin), "node is unavailable")
  expect_identical(
    suppressWarnings(system2(node_bin, c("--check", paths$js),
                             stdout = FALSE, stderr = FALSE)),
    0L
  )

  app_env <- new.env(parent = asNamespace("PhysioStream"))
  sys.source(paths$app, envir = app_env)
  css <- paste(readLines(paths$css, warn = FALSE), collapse = "\n")
  js <- paste(readLines(paths$js, warn = FALSE), collapse = "\n")
  app <- app_env$.physiostream_biofeedback_app(scope, css, js)
  ui <- get("ui", envir = environment(app$httpHandler), inherits = FALSE)
  html <- htmltools::renderTags(ui)$html
  expect_match(html, "scope-canvas", fixed = TRUE)
  expect_match(html, "visible_traces", fixed = TRUE)
  expect_match(html, "target-canvas", fixed = TRUE)
  expect_false(grepl("https://.*\\.js", html))

  biofeedback_test_feed(source, 10L)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      paused = FALSE,
      visible_traces = names(biofeedbackState(scope)$configuration$trace_meta),
      display_gain = 1,
      display_window = 10,
      display_threshold = NA_real_
    )
    session$flushReact()
    expect_gte(biofeedbackState(scope)$counters$frames, 1)
  })
})
