.physiostream_biofeedback_app <- function(scope, css, js, video = NULL) {
  scope_state <- biofeedbackState(scope)
  trace_meta <- scope_state$configuration$trace_meta
  trace_choices <- stats::setNames(
    names(trace_meta),
    vapply(trace_meta, function(trace) {
      if (nzchar(trace$channel)) trace$channel else trace$name
    }, character(1))
  )
  video_json <- if (is.null(video)) {
    "null"
  } else {
    jsonlite::toJSON(
      list(kind = video$kind, source = video$source, sync = video$sync),
      auto_unbox = TRUE, digits = 17, null = "null"
    )
  }
  video_panel <- if (is.null(video)) {
    NULL
  } else {
    shiny::tags$section(
      class = "video-panel",
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h2("Synchronized video"),
        shiny::tags$span(id = "video-drift", class = "metric-value", "0.0 fr")
      ),
      shiny::tags$video(
        id = "physiostream-video",
        class = "video-surface",
        controls = TRUE,
        preload = "metadata",
        src = if (identical(video$kind, "webcam")) NULL else video$source,
        `data-kind` = video$kind
      ),
      if (identical(video$kind, "webcam")) {
        shiny::tags$button(
          id = "start-webcam",
          type = "button",
          class = "command-button",
          "Camera"
        )
      } else {
        NULL
      }
    )
  }

  ui <- shiny::fluidPage(
    class = "physiostream-app",
    shiny::tags$head(
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$style(shiny::HTML(css)),
      shiny::tags$script(
        shiny::HTML(paste0(
          "window.PHYSIOSTREAM_VIDEO = ", video_json, ";\n", js
        ))
      )
    ),
    shiny::tags$header(
      class = "topbar",
      shiny::tags$div(
        class = "brand-block",
        shiny::tags$strong("PhysioStream"),
        shiny::tags$span(id = "scope-status", class = "status-label", "Running")
      ),
      shiny::tags$div(
        class = "status-strip",
        shiny::tags$span(
          class = "status-item",
          shiny::tags$span("Latency"),
          shiny::tags$strong(id = "latency-value", "0.0 ms")
        ),
        shiny::tags$span(
          class = "status-item",
          shiny::tags$span("Loss"),
          shiny::tags$strong(id = "loss-value", "0")
        ),
        shiny::tags$span(
          class = "status-item",
          shiny::tags$span("Frames"),
          shiny::tags$strong(id = "frame-value", "0")
        )
      )
    ),
    shiny::tags$main(
      class = if (is.null(video)) "workspace" else "workspace with-video",
      shiny::tags$section(
        class = "scope-panel",
        shiny::tags$div(
          class = "panel-heading",
          shiny::tags$h1("Live scope"),
          shiny::tags$div(id = "trace-legend", class = "trace-legend")
        ),
        shiny::tags$div(
          class = "canvas-wrap",
          shiny::tags$canvas(
            id = "scope-canvas",
            width = "1600",
            height = "900",
            role = "img",
            `aria-label` = "Live physiological signal traces"
          )
        )
      ),
      shiny::tags$aside(
        class = "control-panel",
        shiny::tags$section(
          class = "target-panel",
          shiny::tags$div(
            class = "panel-heading",
            shiny::tags$h2("Target"),
            shiny::tags$strong(id = "target-value", "--")
          ),
          shiny::tags$canvas(
            id = "target-canvas",
            width = "320",
            height = "160",
            role = "img",
            `aria-label` = "Current target value"
          )
        ),
        shiny::tags$section(
          class = "controls",
          shiny::checkboxInput("paused", "Pause display", FALSE),
          shiny::selectizeInput(
            "visible_traces", "Visible traces",
            choices = trace_choices,
            selected = names(trace_meta),
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::sliderInput(
            "display_gain", "Gain", min = 0.1, max = 5,
            value = 1, step = 0.1
          ),
          shiny::selectInput(
            "display_window", "Window",
            choices = c("2 s" = 2, "5 s" = 5, "10 s" = 10, "20 s" = 20),
            selected = 10
          ),
          shiny::numericInput(
            "display_threshold", "Threshold", value = NA_real_, step = 0.1
          )
        )
      ),
      video_panel
    )
  )

  server <- function(input, output, session) {
    timer <- shiny::reactiveTimer(
      1000 / biofeedbackState(scope)$configuration$update_hz,
      session = session
    )
    shiny::observe({
      timer()
      result <- tryCatch(
        biofeedbackStep(
          scope,
          max_chunks = biofeedbackState(scope)$configuration$max_chunks_default
        ),
        error = function(e) {
          session$sendCustomMessage(
            "physiostream-error",
            list(code = class(e)[[1L]])
          )
          NULL
        }
      )
      if (!is.null(result) && isTRUE(result$updated) &&
          !isTRUE(input$paused)) {
        session$sendCustomMessage(
          "physiostream-frame",
          biofeedbackFrame(scope)
        )
      }
    })
    shiny::observe({
      session$sendCustomMessage(
        "physiostream-controls",
        list(
          paused = isTRUE(input$paused),
          visible_traces = input$visible_traces,
          gain = input$display_gain,
          window = as.numeric(input$display_window),
          threshold = input$display_threshold
        )
      )
    })
    session$onSessionEnded(function() {
      if (identical(biofeedbackState(scope)$lifecycle, "running")) {
        try(biofeedbackStop(scope), silent = TRUE)
      }
    })
  }
  shiny::shinyApp(ui, server)
}
