source("app.R")

calls <- list()
copilot_bridge <- function(request) {
  calls[[length(calls) + 1L]] <<- request
  if (request$action == "models") {
    return(list(
      models = data.frame(id = "synthetic-model", name = "Synthetic model")
    ))
  }
  list(
    model = request$model,
    latency_ms = 25,
    usage = NULL,
    text = "Synthetic response"
  )
}

shiny::testServer(copilot_panel_server, {
  session$setInputs(copilot_provider = "copilot", copilot_check = 1)
  stopifnot(length(calls) == 0L)
  session$setInputs(copilot_synthetic = TRUE, copilot_check = 2)
  stopifnot(length(calls) == 1L, calls[[1]]$action == "models")
  session$setInputs(
    copilot_model = "synthetic-model",
    copilot_prompt = "Synthetic question",
    copilot_run = 1
  )
  stopifnot(length(calls) == 2L, calls[[2]]$prompt == "Synthetic question")
  stopifnot(grepl("Synthetic response", output$copilot_result, fixed = TRUE))
  session$setInputs(copilot_synthetic = FALSE, copilot_run = 2)
  stopifnot(length(calls) == 2L)
})
