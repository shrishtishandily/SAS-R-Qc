app_ui <- function(request) {
  shiny::fluidPage(
    lang = "en",
    shiny::tags$head(
      shiny::tags$title("ADaM QC Workbench"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    shiny::div(
      class = "app-shell",
      shiny::div(
        class = "topbar",
        shiny::div(class = "brand-mark", "QC"),
        shiny::div(class = "brand-name", "ADaM QC", shiny::span("WORKBENCH")),
        shiny::div(
          class = "topbar-right",
          shiny::span(class = "synthetic-badge", "PROTOTYPE · LOCAL SESSION"),
          shiny::actionButton("new_task", "New task", class = "btn-ghost")
        )
      ),
      shiny::div(
        class = "page-wrap",
        shiny::div(class = "eyebrow", "INDEPENDENT QUALITY CONTROL  /  SAS TO R MIGRATION"),
        shiny::div(
          class = "page-heading",
          shiny::h1("Compare, explain, and approve."),
          shiny::p("Upload a SAS reference dataset and an R QC dataset, review the programs, and record human dispositions.")
        ),
        shiny::uiOutput("workflow_steps"),
        shiny::div(
          class = "prototype-banner",
          shiny::div(class = "banner-icon", "i"),
          shiny::div(
            shiny::strong("Bounded prototype"),
            shiny::p("Comparisons are deterministic and run locally in this R session. No AI model, SAS runner, external service, or credentials are used. Upload only data you are authorized to handle; files are processed in memory and temporary storage and are not persisted by the app.")
          )
        ),
        shiny::tabsetPanel(
          id = "tabs", type = "pills",
          shiny::tabPanel("1 · Intake", value = "intake", mod_intake_ui("intake")),
          shiny::tabPanel("2 · Code review", value = "code", mod_code_ui("code")),
          shiny::tabPanel("3 · Run & compare", value = "compare", mod_compare_ui("compare")),
          shiny::tabPanel("4 · Findings", value = "findings", mod_findings_ui("findings")),
          shiny::tabPanel("5 · Approval", value = "approval", mod_approval_ui("approval")),
          shiny::tabPanel("6 · Audit", value = "audit", mod_audit_ui("audit"))
        ),
        shiny::div(
          id = "design-notes", class = "design-notes",
          shiny::strong("Least-privilege design"),
          shiny::span("Read-only handling of uploads (no execution of SAS, no network calls, no stored credentials, no persistence). Allowed upload types and sizes are restricted. Local R execution is off unless explicitly enabled with ADAM_QC_ALLOW_R_EXEC=true and is not a sandbox. A production deployment would additionally need authenticated identity, role-based authorization, isolated execution, governed storage, tamper-evident audit retention, and validation.")
        ),
        shiny::div(class = "footer", "ADaM QC Workbench prototype", shiny::span("NOT A VALIDATED SYSTEM · NOT FOR REGULATORY SUBMISSION"))
      )
    )
  )
}
