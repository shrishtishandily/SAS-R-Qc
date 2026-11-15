copilot_bridge <- function(request) {
  python <- Sys.getenv("ADAM_QC_PYTHON", "python")
  script <- normalizePath(file.path("backend", "bridge.py"), mustWork = TRUE)
  response <- tryCatch(
    suppressWarnings(system2(
      python,
      args = shQuote(script),
      input = jsonlite::toJSON(request, auto_unbox = TRUE),
      stdout = TRUE,
      stderr = TRUE,
      timeout = 120
    )),
    error = function(error) character()
  )
  parsed <- tryCatch(
    jsonlite::fromJSON(paste(response, collapse = "\n")),
    error = function(error) NULL
  )
  if (is.null(parsed) || is.null(parsed$ok)) {
    stop(
      "Python backend unavailable. Install Python and dependencies, then set ADAM_QC_PYTHON if needed.",
      call. = FALSE
    )
  }
  if (!isTRUE(parsed$ok)) {
    stop(parsed$error, call. = FALSE)
  }
  parsed$result
}

copilot_panel_ui <- function() {
  shiny::tags$details(
    shiny::tags$summary("Copilot synthetic test panel"),
    shiny::p(
      "Copilot is a hosted service. Use entirely synthetic text only; prompts may consume your Copilot allowance or incur charges."
    ),
    shiny::selectInput(
      "copilot_provider",
      "Provider",
      choices = c("GitHub Copilot" = "copilot")
    ),
    shiny::checkboxInput(
      "copilot_synthetic",
      "I confirm the prompt is entirely synthetic and authorize sending it to GitHub Copilot.",
      FALSE
    ),
    shiny::actionButton("copilot_check", "Check Copilot connection"),
    shiny::uiOutput("copilot_models"),
    shiny::textAreaInput("copilot_prompt", "Synthetic test prompt", rows = 4),
    shiny::actionButton("copilot_run", "Run synthetic test"),
    shiny::verbatimTextOutput("copilot_result")
  )
}

copilot_panel_server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    models = data.frame(),
    result = "Not connected."
  )
  output$copilot_models <- shiny::renderUI({
    if (!nrow(state$models)) {
      return(shiny::p("No models loaded."))
    }
    shiny::selectInput(
      "copilot_model",
      "Model",
      choices = stats::setNames(state$models$id, state$models$name)
    )
  })
  output$copilot_result <- shiny::renderText(state$result)
  shiny::observeEvent(input$copilot_check, {
    state$models <- data.frame()
    if (!isTRUE(input$copilot_synthetic)) {
      state$result <- "Confirm synthetic-only use before connecting."
      return()
    }
    tryCatch(
      {
        result <- copilot_bridge(list(
          action = "models",
          provider = input$copilot_provider,
          synthetic_confirmed = TRUE
        ))
        if (
          !is.data.frame(result$models) ||
            !all(c("id", "name") %in% names(result$models))
        ) {
          stop(
            "No Copilot models are available to the signed-in account.",
            call. = FALSE
          )
        }
        state$models <- result$models
        state$result <- sprintf(
          "Copilot connection succeeded; %d model(s) available.",
          nrow(state$models)
        )
      },
      error = function(error) state$result <- conditionMessage(error)
    )
  })
  shiny::observeEvent(input$copilot_run, {
    if (
      !isTRUE(input$copilot_synthetic) ||
        !nrow(state$models) ||
        !(input$copilot_model %in% state$models$id)
    ) {
      state$result <- "Confirm synthetic-only use and check the connection before running."
      return()
    }
    tryCatch(
      {
        result <- copilot_bridge(list(
          action = "generate",
          provider = input$copilot_provider,
          synthetic_confirmed = TRUE,
          model = input$copilot_model,
          prompt = input$copilot_prompt
        ))
        state$result <- paste0(
          "Model: ",
          result$model,
          "\nLatency: ",
          result$latency_ms,
          " ms\nUsage: ",
          if (is.null(result$usage)) {
            "not available"
          } else {
            jsonlite::toJSON(result$usage, auto_unbox = TRUE)
          },
          "\n\n",
          result$text
        )
      },
      error = function(error) state$result <- conditionMessage(error)
    )
  })
}
