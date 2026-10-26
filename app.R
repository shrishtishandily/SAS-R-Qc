source("R/workflow.R", local = TRUE)
source("R/data.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

shiny::shinyApp(ui = app_ui, server = app_server)
