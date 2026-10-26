app_server <- function(input, output, session) {
  state <- shiny::reactiveVal("setup")
  events <- shiny::reactiveVal(initial_audit_events())
  disposition <- shiny::reactiveVal(NULL)
  finding <- synthetic_finding()

  append_event <- function(event, detail) {
    current <- events()
    next_row <- data.frame(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      actor = "Reviewer / prototype",
      event = event,
      detail = detail,
      stringsAsFactors = FALSE
    )
    events(rbind(next_row, current))
  }

  advance <- function(to, event, detail) {
    state(transition_workflow(state(), to))
    append_event(event, detail)
  }

  output$workflow_steps <- shiny::renderUI({
    current <- state()
    current_index <- match(current, WORKFLOW_STATES)
    shiny::div(class = "workflow-strip",
      lapply(seq_along(WORKFLOW_STATES), function(i) {
        step_state <- if (i < current_index) "complete" else if (i == current_index) "active" else "pending"
        shiny::div(class = paste("workflow-step", step_state),
          shiny::div(class = "step-dot", if (i < current_index) "✓" else sprintf("%02d", i)),
          shiny::span(WORKFLOW_LABELS[[WORKFLOW_STATES[[i]]]])
        )
      })
    )
  })

  output$state_summary <- shiny::renderUI({
    current <- state()
    index <- match(current, WORKFLOW_STATES)
    shiny::tagList(
      shiny::div(class = "current-state", WORKFLOW_LABELS[[current]]),
      shiny::p(class = "state-description", switch(current,
        setup = "Confirm the synthetic task context before reviewing candidate logic.",
        review = "Candidate logic is shown as a read-only illustrative preview.",
        ready_to_compare = "Comparison is ready; the next action reveals fixed fixture values.",
        compared = "The deterministic comparator finding is available for human review.",
        dispositioned = "A human disposition is recorded; approval remains a separate action.",
        approved = "Human approval recorded for this session."
      )),
      shiny::div(class = "state-progress", shiny::div(style = sprintf("width:%d%%", round(index / length(WORKFLOW_STATES) * 100))))
    )
  })

  output$run_action <- shiny::renderUI({
    current <- state()
    if (current == "setup") {
      shiny::actionButton("begin_review", "Confirm setup & review code", class = "btn-primary-wide")
    } else if (current == "review") {
      shiny::actionButton("ready_compare", "Mark review complete", class = "btn-primary-wide")
    } else if (current == "ready_to_compare") {
      shiny::actionButton("run_compare", "Run simulated comparison", class = "btn-primary-wide")
    } else {
      shiny::div(class = "run-complete", shiny::span("✓"), "Simulated comparison complete — fixture only")
    }
  })

  shiny::observeEvent(input$begin_review, {
    advance("review", "Task setup confirmed", "Synthetic task context reviewed; no inputs collected.")
  })
  shiny::observeEvent(input$ready_compare, {
    advance("ready_to_compare", "Candidate review marked complete", "Read-only pseudocode preview; no execution.")
  })
  shiny::observeEvent(input$run_compare, {
    advance("compared", "Simulated comparison revealed", "Deterministic fixture: SAS BASE 66; R BASE 55; difference +11.")
  })

  output$finding_panel <- shiny::renderUI({
    if (!state() %in% c("compared", "dispositioned", "approved")) return(NULL)
    evidence_rows <- lapply(seq_len(nrow(finding$evidence)), function(i) {
      row <- finding$evidence[i, ]
      shiny::tags$tr(shiny::tags$td(row$source), shiny::tags$td(row$variable),
                     shiny::tags$td(class = "value-cell", row$value))
    })
    shiny::div(class = "panel finding-panel",
      shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "!"), shiny::h2("Finding · F-001")),
                 shiny::span(class = "finding-status", "REVIEW REQUIRED")),
      shiny::div(class = "finding-title-row", shiny::h3(finding$title), shiny::span(class = "severity", finding$severity)),
      shiny::p(class = "finding-rule", finding$rule),
      shiny::div(class = "evidence-label", finding$comparator),
      shiny::tags$table(class = "evidence-table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("SOURCE"), shiny::tags$th("VARIABLE"), shiny::tags$th("VALUE"))),
        shiny::tags$tbody(evidence_rows)),
      shiny::div(class = "delta-note", shiny::strong("+11"), " synthetic value difference  ·  comparator fixture only"),
      shiny::div(class = "explanation-card", shiny::div(class = "explanation-heading", shiny::span(class = "proposed-pill", "AI-PROPOSED · SIMULATED")),
                 shiny::p(finding$explanation)),
      if (state() == "compared") shiny::div(class = "review-next", "Review the evidence and explanation, then record a human disposition below.")
    )
  })

  output$disposition_panel <- shiny::renderUI({
    if (!state() %in% c("compared", "dispositioned", "approved")) return(NULL)
    if (state() == "compared") {
      shiny::div(class = "panel disposition-panel",
        shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "05"), shiny::h2("Human disposition")),
                   shiny::span(class = "human-required", "REVIEWER ACTION REQUIRED")),
        shiny::radioButtons("decision", label = NULL,
          choices = c("Accept finding for follow-up" = "accept", "Mark as expected / explain" = "expected"),
          selected = character(), inline = FALSE),
        shiny::textAreaInput("review_note", "Reviewer rationale (required)", rows = 2,
                             placeholder = "Add a concise rationale for this synthetic finding…"),
        shiny::actionButton("record_disposition", "Record human disposition", class = "btn-secondary-wide")
      )
    } else {
      shiny::div(class = "panel disposition-panel",
        shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "05"), shiny::h2("Human disposition")),
                   shiny::span(class = "human-approved", if (state() == "approved") "HUMAN APPROVED" else "HUMAN RECORDED")),
        shiny::div(class = "approved-card", shiny::strong("Human-approved disposition"),
          shiny::p(paste(disposition()$label, "—", disposition()$note))),
        if (state() == "dispositioned") shiny::actionButton("approve", "Approve disposition", class = "btn-secondary-wide")
      )
    }
  })

  shiny::observeEvent(input$record_disposition, {
    shiny::req(input$decision, nzchar(trimws(input$review_note)))
    label <- if (identical(input$decision, "accept")) "Accept finding for follow-up" else "Mark as expected / explain"
    disposition(list(label = label, note = trimws(input$review_note)))
    advance("dispositioned", "Human disposition recorded", paste(label, "Rationale:", trimws(input$review_note)))
  })
  shiny::observeEvent(input$approve, {
    shiny::req(!is.null(disposition()))
    advance("approved", "Human approval recorded", paste("Approved:", disposition()$label))
  })

  output$audit_table <- shiny::renderUI({
    log <- events()
    rows <- lapply(seq_len(nrow(log)), function(i) {
      shiny::tags$tr(shiny::tags$td(class = "audit-time", log$time[i]),
        shiny::tags$td(class = "audit-actor", log$actor[i]),
        shiny::tags$td(shiny::strong(log$event[i]), shiny::div(class = "audit-detail", log$detail[i])))
    })
    shiny::tags$table(class = "audit-table",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("TIME"), shiny::tags$th("ACTOR"), shiny::tags$th("EVENT / DETAIL"))),
      shiny::tags$tbody(rows))
  })
}
