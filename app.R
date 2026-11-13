source_files <- c(
  "utils",
  "audit",
  "io",
  "compare",
  "findings",
  "data",
  "pipeline_ui",
  "pipeline_server"
)
for (f in source_files) {
  source(file.path("R", paste0(f, ".R")), local = TRUE)
}

options(shiny.maxRequestSize = MAX_DATA_BYTES)

shiny::shinyApp(ui = pipeline_ui, server = pipeline_server)
