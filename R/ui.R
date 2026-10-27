app_ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$title("ADaM QC Workbench | Synthetic prototype"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shiny::div(
    class = "app-shell",
    shiny::div(
      class = "topbar",
      shiny::div(class = "brand-mark", "QC"),
      shiny::div(class = "brand-name", "ADaM QC", shiny::span("WORKBENCH")),
      shiny::div(class = "topbar-right", shiny::span(class = "synthetic-badge", "●  SYNTHETIC PROTOTYPE"),
                 shiny::span(class = "user-chip", "QC"))
    ),
    shiny::div(
      class = "page-wrap",
      shiny::div(class = "eyebrow", "INDEPENDENT QUALITY CONTROL  /  SAS → R MIGRATION"),
      shiny::div(class = "page-heading",
                 shiny::div(shiny::h1("Review with confidence."),
                            shiny::p("A bounded, human-led workspace for comparing migration outputs.")),
                shiny::div(class = "task-tag", shiny::span("TASK  "), "MIG-024  ·  ADT BASELINE")),
      shiny::uiOutput("workflow_steps"),
      shiny::div(class = "prototype-banner",
                 shiny::div(class = "banner-icon", "i"),
                 shiny::div(shiny::strong("Prototype mode — simulated actions only"),
                            shiny::p("No SAS code is executed, no R candidate is run, and no external AI or service is connected."))),
      shiny::div(
        class = "main-grid",
        shiny::div(
          class = "primary-column",
          shiny::div(
            class = "panel setup-panel",
            shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "01"), shiny::h2("Task setup")),
                       shiny::span(class = "status-pill", "SYNTHETIC CASE")),
            shiny::div(class = "setup-grid",
                       shiny::div(class = "field-group", shiny::span(class = "field-label", "DATASET"), shiny::div(class = "field-value", "ADSL · synthetic aggregate")),
                       shiny::div(class = "field-group", shiny::span(class = "field-label", "MIGRATION PAIR"), shiny::div(class = "field-value", "SAS reference  ↔  R candidate")),
                       shiny::div(class = "field-group", shiny::span(class = "field-label", "QC RULE"), shiny::div(class = "field-value", "BASE derivation · missing ADT"))),
            shiny::div(class = "panel-note", "Illustrative baseline values only. No participant-level rows or source files are present.")
          ),
          shiny::div(
            class = "panel code-panel",
            shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "02"), shiny::h2("Candidate QC logic")),
                       shiny::span(class = "review-label", "READ-ONLY PREVIEW")),
            shiny::div(class = "code-meta", shiny::span("R candidate · illustrative pseudocode"), shiny::span("rev 0.3")),
            shiny::tags$pre(class = "code-block",
                            shiny::tags$code(shiny::HTML(paste0(
                              '<span class="code-comment"># Synthetic example — not executable QC code</span>\n',
                              '<span class="code-keyword">if</span> (is.na(ADT)) {\n',
                              '  BASE <span class="code-op">&lt;-</span> <span class="code-number">55</span>  <span class="code-comment"># candidate fixture</span>\n',
                              '} <span class="code-keyword">else</span> {\n',
                              '  BASE <span class="code-op">&lt;-</span> derive_base(ADT)\n',
                              '}')))),
            shiny::div(class = "code-foot", "Review the visible snippet; execution is disabled in this prototype.")
          ),
          shiny::div(
            class = "panel compare-panel",
            shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "03"), shiny::h2("Run & compare")),
                       shiny::span(class = "deterministic-tag", "DETERMINISTIC FIXTURE")),
            shiny::p(class = "panel-copy", "Advance the workflow to reveal the pre-seeded comparator finding. This button does not execute SAS or R."),
            shiny::uiOutput("run_action")
          ),
          shiny::uiOutput("finding_panel"),
          shiny::uiOutput("disposition_panel")
        ),
        shiny::div(
          class = "side-column",
          shiny::div(class = "panel state-panel",
                     shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "↗"), shiny::h2("Workflow state"))),
                     shiny::uiOutput("state_summary")),
          shiny::div(class = "panel provenance-panel",
                     shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "◎"), shiny::h2("Evidence labels"))),
                     shiny::div(class = "legend-row", shiny::span(class = "legend-dot deterministic"), shiny::div(shiny::strong("Deterministic comparator"), shiny::p("Fixed synthetic values; reproducible fixture."))),
                     shiny::div(class = "legend-row", shiny::span(class = "legend-dot proposed"), shiny::div(shiny::strong("AI-proposed explanation"), shiny::p("Static illustrative text; no AI model is connected."))),
                     shiny::div(class = "legend-row", shiny::span(class = "legend-dot human"), shiny::div(shiny::strong("Human-approved disposition"), shiny::p("Only recorded by an explicit reviewer action.")))),
          shiny::div(class = "panel limits-panel",
                     shiny::div(class = "limits-icon", "盾"),
                     shiny::strong("Bounded by design"),
                     shiny::p("No file upload, network calls, credentials, SAS runtime, or clinical data. Prototype actions update local session state only."),
                     shiny::a("Least-privilege design notes ↓", href = "#design-notes"))
        )
      ),
      shiny::div(class = "panel audit-panel",
                 shiny::div(class = "section-head", shiny::div(shiny::span(class = "section-index", "04"), shiny::h2("Audit history")),
                            shiny::span(class = "audit-live", "●  SESSION LOG")),
                 shiny::div(class = "audit-caption", "Append-only event list for this browser session. It is not a validated or durable audit trail."),
                 shiny::uiOutput("audit_table")),
      shiny::div(id = "design-notes", class = "design-notes",
                 shiny::strong("Least-privilege design notes"),
                 shiny::span("Prototype stores only transient session state; no uploads, persistence, external calls, or credentials. Production use would require validated identity and authorization, protected storage, controlled execution, data governance, audit retention, and independent validation.")),
      shiny::div(class = "footer", "ADaM QC Workbench · Synthetic-only prototype", shiny::span("NOT FOR CLINICAL OR REGULATORY USE"))
    )
  )
)
