app_server <- function(input, output, session) {
  store <- shiny::reactiveValues(
    state = "intake", reviewer = "", task = "", reset_n = 0L,
    ref = NULL, qc = NULL, ref_meta = NULL, qc_meta = NULL,
    sas_code = NULL, r_code = NULL, sas_code_name = NULL, r_code_name = NULL, code_review = NULL, demo_keys = NULL,
    comparison = NULL, findings = NULL, dispositions = list(), approval = NULL,
    audit = new_audit_log()
  )

  log_event <- function(event, detail = "") {
    reviewer <- shiny::isolate(store$reviewer)
    store$audit <- append_audit(
      shiny::isolate(store$audit),
      actor = if (nzchar(reviewer)) reviewer else "unidentified reviewer",
      state = shiny::isolate(store$state), event = event, detail = detail
    )
  }

  transition <- function(to) {
    store$state <- transition_workflow(shiny::isolate(store$state), to)
  }

  # Any change to inputs discards results, dispositions, and approval.
  invalidate <- function(reason, to = "intake") {
    had_results <- !is.null(shiny::isolate(store$comparison)) || length(shiny::isolate(store$dispositions)) > 0 ||
      !is.null(shiny::isolate(store$approval))
    store$comparison <- NULL
    store$findings <- NULL
    store$dispositions <- list()
    store$approval <- NULL
    if (shiny::isolate(store$state) != to) transition(to)
    if (had_results) log_event("Results invalidated", reason)
  }

  goto <- function(tab) shiny::updateTabsetPanel(session, "tabs", selected = tab)

  ctx <- list(store = store, log = log_event, transition = transition, invalidate = invalidate, goto = goto)

  shiny::observeEvent(input$new_task, {
    log_event("New task started", "Session data cleared; audit history retained.")
    for (nm in c("ref", "qc", "ref_meta", "qc_meta", "sas_code", "r_code", "sas_code_name", "r_code_name", "code_review", "demo_keys")) {
      store[[nm]] <- NULL
    }
    invalidate("New task", to = "intake")
    store$reset_n <- shiny::isolate(store$reset_n) + 1L
    goto("intake")
  })

  output$workflow_steps <- shiny::renderUI({
    current_index <- match(store$state, WORKFLOW_STATES)
    shiny::tags$ol(
      class = "workflow-strip", `aria-label` = "Workflow progress",
      lapply(seq_along(WORKFLOW_STATES), function(i) {
        kind <- if (i < current_index) "complete" else if (i == current_index) "active" else "pending"
        shiny::tags$li(
          class = paste("workflow-step", kind), `aria-current` = if (kind == "active") "step",
          shiny::div(class = "step-dot", if (kind == "complete") "✓" else i),
          shiny::span(WORKFLOW_LABELS[[WORKFLOW_STATES[[i]]]])
        )
      })
    )
  })

  mod_intake_server("intake", ctx)
  mod_code_server("code", ctx)
  mod_compare_server("compare", ctx)
  mod_findings_server("findings", ctx)
  mod_approval_server("approval", ctx)
  mod_audit_server("audit", ctx)

  log_event("Session started", "No data loaded.")
}
