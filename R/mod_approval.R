mod_approval_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("body"))
}

mod_approval_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store
    ns <- session$ns

    code_review_required <- function() !is.null(store$sas_code) || !is.null(store$r_code)

    checks <- shiny::reactive({
      all_disp <- !is.null(store$comparison) && length(store$dispositions) == length(store$findings)
      list(
        "Reviewer name entered" = nzchar(store$reviewer),
        "Datasets loaded and comparison run" = !is.null(store$comparison),
        "Every finding has a human disposition" = all_disp && store$state %in% c("dispositioned", "approved"),
        "Program review recorded (or no programs uploaded)" = !code_review_required() || !is.null(store$code_review)
      )
    })

    output$body <- shiny::renderUI({
      ck <- checks()
      ready <- all(unlist(ck))
      items <- lapply(names(ck), function(n) {
        shiny::tags$li(class = if (ck[[n]]) "check-ok" else "check-open", shiny::span(class = "check-mark", if (ck[[n]]) "✓" else "○"), n)
      })

      approved <- if (!is.null(store$approval)) {
        shiny::div(
          class = "approved-card",
          PILL_HUMAN(),
          shiny::strong("Human-approved disposition recorded"),
          shiny::p(sprintf("%s · %s", store$approval$reviewer, store$approval$time)),
          shiny::p(store$approval$statement),
          shiny::p(class = "field-help", "Recorded in this session only. This is not a validated electronic signature.")
        )
      }

      shiny::div(
        class = "panel",
        shiny::div(class = "section-head", shiny::h2("Human approval")),
        shiny::tags$ul(class = "checklist", items),
        if (!is.null(approved)) {
          approved
        } else if (store$state == "dispositioned" && ready) {
          shiny::tagList(
            shiny::textAreaInput(ns("statement"), "Approval statement (required)", rows = 2,
                                 placeholder = "Summarize the QC conclusion for this task."),
            shiny::checkboxInput(ns("attest"), "I reviewed the comparator evidence and my dispositions, and I approve this QC outcome.", FALSE),
            shiny::actionButton(ns("approve"), "Approve QC outcome", class = "btn-primary-wide")
          )
        } else {
          shiny::p(class = "field-help", "Approval becomes available when every item above is complete.")
        },
        shiny::div(
          class = "download-row",
          shiny::downloadButton(ns("dl_findings"), "Download findings and dispositions (CSV)", class = "btn-ghost"),
          shiny::downloadButton(ns("dl_audit"), "Download audit log (CSV)", class = "btn-ghost")
        )
      )
    })

    shiny::observeEvent(input$approve, {
      shiny::req(store$state == "dispositioned", all(unlist(checks())))
      if (!isTRUE(input$attest) || !nzchar(trimws(input$statement %||% ""))) {
        return(shiny::showNotification("Enter a statement and confirm the attestation.", type = "warning"))
      }
      store$approval <- list(reviewer = store$reviewer, time = now_stamp(), statement = trimws(input$statement))
      ctx$transition("approved")
      ctx$log("Human approval recorded", sprintf("statement=%s", trimws(input$statement)))
    })

    output$dl_findings <- shiny::downloadHandler(
      filename = function() "qc_findings_dispositions.csv",
      content = function(file) utils::write.csv(findings_export(store$findings, store$dispositions), file, row.names = FALSE)
    )
    output$dl_audit <- shiny::downloadHandler(
      filename = function() "qc_audit_log.csv",
      content = function(file) utils::write.csv(store$audit, file, row.names = FALSE)
    )
  })
}
