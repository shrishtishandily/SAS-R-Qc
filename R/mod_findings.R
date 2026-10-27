mod_findings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("body"))
}

mod_findings_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store
    ns <- session$ns

    status_label <- function(f) if (is.null(store$dispositions[[f$id]])) "pending" else "recorded"

    output$body <- shiny::renderUI({
      if (is.null(store$comparison)) {
        return(shiny::div(class = "panel", shiny::div(class = "empty-note", "Run a comparison to see findings.")))
      }
      fs <- store$findings
      if (!length(fs)) {
        return(shiny::div(
          class = "panel",
          shiny::div(class = "section-head", shiny::h2("Findings"), PILL_DETERMINISTIC()),
          shiny::div(class = "ok-note", "The comparator found no differences."),
          if (store$state == "compared") {
            shiny::tagList(
              shiny::p(class = "field-help", "Confirm that you reviewed the comparator result and have no findings to disposition."),
              shiny::actionButton(ns("confirm_none"), "Confirm: no findings", class = "btn-secondary-wide")
            )
          } else {
            shiny::div(class = "ok-note", "Review confirmed.")
          }
        ))
      }
      choices <- stats::setNames(
        vapply(fs, function(f) f$id, character(1)),
        vapply(fs, function(f) sprintf("%s · %s · %s [%s]", f$id, f$title, f$severity, status_label(f)), character(1))
      )
      shiny::div(
        class = "findings-grid",
        shiny::div(
          class = "panel",
          shiny::div(class = "section-head", shiny::h2("Findings")),
          shiny::radioButtons(ns("sel"), NULL, choices = choices, selected = shiny::isolate(input$sel) %||% choices[[1]]),
          shiny::p(class = "field-help", sprintf("%d of %d dispositioned.", length(store$dispositions), length(fs)))
        ),
        shiny::uiOutput(ns("detail"))
      )
    })

    current <- shiny::reactive({
      fs <- store$findings
      id <- input$sel %||% if (length(fs)) fs[[1]]$id else NULL
      match_f <- Filter(function(f) identical(f$id, id), fs)
      if (length(match_f)) match_f[[1]] else NULL
    })

    output$detail <- shiny::renderUI({
      f <- current()
      if (is.null(f)) return(NULL)
      disp <- store$dispositions[[f$id]]
      editable <- store$state %in% c("compared", "dispositioned")
      shiny::div(
        class = "panel finding-panel",
        shiny::div(
          class = "section-head",
          shiny::h2(paste(f$id, f$title)),
          shiny::span(class = paste("severity", tolower(f$severity)), f$severity)
        ),
        shiny::div(class = "label-row", PILL_DETERMINISTIC()),
        shiny::p(f$summary),
        shiny::h3(class = "sub-head", "Evidence"),
        html_table(f$evidence, max_rows = 100L),
        if (!is.na(f$hint)) {
          shiny::div(
            class = "hint-card",
            PILL_HINT(),
            shiny::p(f$hint),
            shiny::p(class = "field-help", "Computed by fixed pattern checks over the differing records. It is not a model output and does not establish a root cause.")
          )
        },
        shiny::div(class = "label-row", PILL_HUMAN()),
        if (!is.null(disp)) {
          shiny::div(
            class = "approved-card",
            shiny::strong(disp$decision),
            shiny::p(disp$rationale),
            shiny::p(class = "field-help", sprintf("%s · %s", disp$reviewer, disp$time))
          )
        },
        if (editable) {
          shiny::tagList(
            shiny::selectInput(ns("decision"), if (is.null(disp)) "Disposition" else "Revise disposition", choices = c("Select..." = "", DISPOSITION_CHOICES),
                               selected = disp$decision %||% ""),
            shiny::textAreaInput(ns("rationale"), "Rationale (required)", rows = 3, value = disp$rationale %||% ""),
            shiny::actionButton(ns("record"), if (is.null(disp)) "Record human disposition" else "Update disposition", class = "btn-secondary-wide")
          )
        }
      )
    })

    shiny::observeEvent(input$record, {
      f <- current()
      shiny::req(f, store$state %in% c("compared", "dispositioned"))
      if (!nzchar(input$decision %||% "") || !nzchar(trimws(input$rationale %||% ""))) {
        return(shiny::showNotification("Select a disposition and enter a rationale.", type = "warning"))
      }
      store$dispositions[[f$id]] <- list(
        decision = input$decision, rationale = trimws(input$rationale),
        reviewer = if (nzchar(store$reviewer)) store$reviewer else "unidentified reviewer", time = now_stamp()
      )
      ctx$log("Human disposition recorded", sprintf("finding=%s; decision=%s; rationale=%s", f$id, input$decision, trimws(input$rationale)))
      if (length(store$dispositions) == length(store$findings) && store$state == "compared") {
        ctx$transition("dispositioned")
        ctx$log("All findings dispositioned", sprintf("count=%d", length(store$findings)))
        ctx$goto("approval")
      }
    })

    shiny::observeEvent(input$confirm_none, {
      shiny::req(store$state == "compared", !length(store$findings))
      ctx$transition("dispositioned")
      ctx$log("No-findings result confirmed by reviewer", "Comparator reported no differences.")
      ctx$goto("approval")
    })
  })
}
