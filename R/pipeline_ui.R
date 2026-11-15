PIPELINE_STEPS <- c(
  "workspace",
  "requirements",
  "production_r",
  "trace_r",
  "sas_review",
  "sas_output",
  "trace_sas",
  "compare",
  "triage",
  "cdisc",
  "signoff"
)

PIPELINE_LABELS <- c(
  workspace = "Study inputs",
  requirements = "Requirements baseline",
  production_r = "Production R",
  trace_r = "R traceability",
  sas_review = "Independent SAS QC",
  sas_output = "SAS QC output",
  trace_sas = "SAS traceability",
  compare = "Deterministic comparison",
  triage = "Difference review",
  cdisc = "CDISC checks",
  signoff = "Report & sign-off"
)

pipeline_ui <- function(request) {
  shiny::fluidPage(
    lang = "en",
    shiny::tags$head(
      shiny::tags$title("ADaM QC Workbench"),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "pipeline.css"
      )
    ),
    shiny::div(
      class = "app-shell",
      shiny::div(
        class = "topbar",
        shiny::div(class = "brand-mark", "QC"),
        shiny::div(class = "brand-name", "ADaM QC", shiny::span("WORKBENCH")),
        shiny::div(
          class = "topbar-right",
          shiny::span(class = "synthetic-badge", "HUMAN-GATED PROTOTYPE")
        )
      ),
      shiny::div(
        class = "page-wrap",
        shiny::div(
          class = "eyebrow",
          "INDEPENDENT ADaM QUALITY CONTROL  /  SAS + R"
        ),
        shiny::div(
          class = "page-heading",
          shiny::h1(
            "Trace requirements. Review evidence. Approve deliberately."
          ),
          shiny::p(
            "Sequential, human-gated workflow for production R, independent SAS QC, standards checks, comparison, and sign-off."
          )
        ),
        shiny::div(
          class = "prototype-banner",
          shiny::div(class = "banner-icon", "i"),
          shiny::div(
            shiny::strong("Prototype — not a validated system"),
            shiny::p(
              "Synthetic data only. The optional Copilot test panel sends entered text to a hosted service. AI suggestions never approve, edit, or execute code. R and SAS runs and CDISC checks happen externally. This single Shiny process cannot technically enforce SAS/R blinding; session approvals/audit are not validated controls."
            )
          )
        ),
        copilot_panel_ui(),
        shiny::uiOutput("workflow_steps"),
        shiny::uiOutput("phase_ui"),
        shiny::div(
          class = "footer",
          "ADaM QC Workbench · local prototype",
          shiny::span("NOT VALIDATED · NO REGULATORY / PRODUCTION USE")
        )
      )
    )
  )
}
