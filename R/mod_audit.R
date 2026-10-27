mod_audit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "panel",
    shiny::div(class = "section-head", shiny::h2("Audit history"), shiny::span(class = "pill neutral", "SESSION LOG")),
    shiny::p(class = "field-help", "Append-only for this session. Records actions, file names, row counts and checksums, not data values. It is not a validated, durable, or tamper-evident audit trail, and is lost when the session ends unless downloaded."),
    shiny::downloadButton(ns("dl"), "Download audit log (CSV)", class = "btn-ghost"),
    shiny::uiOutput(ns("table"))
  )
}

mod_audit_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store
    output$table <- shiny::renderUI({
      log <- store$audit
      log <- log[rev(seq_len(nrow(log))), c("seq", "time", "actor", "state", "event", "detail"), drop = FALSE]
      html_table(log, max_rows = 500L, class = "data-table audit-table")
    })
    output$dl <- shiny::downloadHandler(
      filename = function() "qc_audit_log.csv",
      content = function(file) utils::write.csv(store$audit, file, row.names = FALSE)
    )
  })
}
