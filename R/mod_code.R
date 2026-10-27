CODE_REVIEW_ITEMS <- c(
  "Candidate logic reads the correct source datasets",
  "Derivation matches the specification / SAP rule",
  "Missing-value handling reviewed",
  "Program does not read the reference (SAS) output, so QC stays independent",
  "No hard-coded values or manual overrides"
)

mod_code_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "panel",
      shiny::div(
        class = "section-head",
        shiny::h2("Program review"),
        shiny::span(class = "pill neutral", "READ-ONLY · NEVER EXECUTED HERE")
      ),
      shiny::div(
        class = "code-grid",
        shiny::div(shiny::div(class = "code-title", "SAS program"), shiny::uiOutput(ns("sas_view"))),
        shiny::div(shiny::div(class = "code-title", "Candidate R QC program"), shiny::uiOutput(ns("r_view")))
      )
    ),
    shiny::div(
      class = "panel",
      shiny::div(class = "section-head", shiny::h2("Reviewer checklist")),
      shiny::uiOutput(ns("review_ui"))
    )
  )
}

mod_code_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store

    code_view <- function(text, name) {
      if (is.null(text)) {
        return(shiny::div(class = "empty-note", "No program uploaded."))
      }
      shiny::tagList(
        shiny::div(class = "code-meta", shiny::span(name), shiny::span(sprintf("%d lines", length(strsplit(text, "\n")[[1]])))),
        shiny::tags$pre(class = "code-block", shiny::tags$code(text))
      )
    }
    output$sas_view <- shiny::renderUI(code_view(store$sas_code, store$sas_code_name))
    output$r_view <- shiny::renderUI(code_view(store$r_code, store$r_code_name))

    output$review_ui <- shiny::renderUI({
      ns <- session$ns
      rec <- store$code_review
      if (!is.null(rec)) {
        return(shiny::div(
          class = "approved-card",
          shiny::strong("Code review recorded"),
          shiny::p(sprintf("%s · %d of %d checklist items confirmed.", rec$time, length(rec$items), length(CODE_REVIEW_ITEMS))),
          if (nzchar(rec$note)) shiny::p(rec$note)
        ))
      }
      if (store$state == "intake") {
        return(shiny::div(class = "empty-note", "Confirm intake first."))
      }
      shiny::tagList(
        if (is.null(store$r_code) && is.null(store$sas_code)) {
          shiny::p(class = "field-help", "No programs were uploaded. You may record that no program review applies.")
        },
        shiny::checkboxGroupInput(ns("items"), NULL, choices = CODE_REVIEW_ITEMS),
        shiny::textAreaInput(ns("note"), "Review notes (optional)", rows = 2),
        shiny::actionButton(ns("record"), "Record code review", class = "btn-secondary-wide")
      )
    })

    shiny::observeEvent(input$record, {
      shiny::req(store$state != "intake")
      store$code_review <- list(items = input$items %||% character(), note = trimws(input$note %||% ""), time = now_stamp())
      ctx$log("Code review recorded", sprintf("items_confirmed=%d/%d; programs=%s", length(input$items), length(CODE_REVIEW_ITEMS),
                                               paste(c(if (!is.null(store$sas_code)) "SAS", if (!is.null(store$r_code)) "R"), collapse = "+")))
      ctx$goto("compare")
    })
  })
}
