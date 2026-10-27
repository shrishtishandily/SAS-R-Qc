source_files <- c(
  "utils", "workflow", "audit", "io", "compare", "findings", "exec", "data",
  "mod_intake", "mod_code", "mod_compare", "mod_findings", "mod_approval", "mod_audit",
  "ui", "server"
)
for (f in source_files) source(file.path("R", paste0(f, ".R")), local = TRUE)

options(shiny.maxRequestSize = MAX_DATA_BYTES)

shiny::shinyApp(ui = app_ui, server = app_server)
