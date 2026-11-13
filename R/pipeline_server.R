new_pipeline_store <- function() {
  shiny::reactiveValues(
    phase = "workspace",
    reviewer = "",
    task = "",
    study_id = "",
    docs = data.frame(
      name = character(),
      category = character(),
      version = character(),
      citation_note = character(),
      md5 = character()
    ),
    sdtm = list(),
    sdtm_meta = list(),
    artifacts = list(),
    requirements = NULL,
    requirements_approved = FALSE,
    r_program = NULL,
    r_meta = NULL,
    r_approved = FALSE,
    r_output = NULL,
    r_output_meta = NULL,
    r_trace = NULL,
    r_trace_approved = FALSE,
    sas_program = NULL,
    sas_meta = NULL,
    sas_review = "",
    sas_approved = FALSE,
    sas_output = NULL,
    sas_output_meta = NULL,
    sas_trace = NULL,
    sas_trace_approved = FALSE,
    comparison = NULL,
    comparison_profile = NULL,
    findings = list(),
    triage = list(),
    cdisc_reports = data.frame(name = character(), md5 = character()),
    cdisc_notes = "",
    cdisc_approved = FALSE,
    final_approval = NULL,
    audit = new_audit_log()
  )
}

pipeline_next <- c(
  workspace = "requirements",
  requirements = "production_r",
  production_r = "trace_r",
  trace_r = "sas_review",
  sas_review = "sas_output",
  sas_output = "trace_sas",
  trace_sas = "compare",
  compare = "triage",
  triage = "cdisc",
  cdisc = "signoff"
)

pipeline_transition <- function(store, to, ctx) {
  from <- shiny::isolate(store$phase)
  if (!identical(unname(pipeline_next[[from]]), to)) {
    stop(
      sprintf("Invalid workflow transition: %s -> %s", from, to),
      call. = FALSE
    )
  }
  store$phase <- to
  ctx$log("Workflow phase advanced", paste(from, "->", to))
  invisible(to)
}

pipeline_server <- function(input, output, session) {
  s <- new_pipeline_store()
  ctx <- list(store = s, log = function(event, detail = "") {
    audit <- shiny::isolate(s$audit)
    reviewer <- shiny::isolate(s$reviewer)
    phase <- shiny::isolate(s$phase)
    s$audit <- append_audit(
      audit,
      actor = if (nzchar(reviewer)) reviewer else "unidentified reviewer",
      state = phase,
      event = event,
      detail = detail
    )
  })

  clear_comparison <- function() {
    s$comparison <- NULL
    s$comparison_profile <- NULL
    s$findings <- list()
    s$triage <- list()
    s$final_approval <- NULL
  }

  reset_after <- function(boundary, reason) {
    if (boundary == "workspace") {
      s$requirements <- NULL
      s$requirements_approved <- FALSE
      s$r_program <- NULL
      s$r_meta <- NULL
      s$r_approved <- FALSE
      s$r_output <- NULL
      s$r_output_meta <- NULL
      s$r_trace <- NULL
      s$r_trace_approved <- FALSE
      s$sas_program <- NULL
      s$sas_meta <- NULL
      s$sas_review <- ""
      s$sas_approved <- FALSE
      s$sas_output <- NULL
      s$sas_output_meta <- NULL
      s$sas_trace <- NULL
      s$sas_trace_approved <- FALSE
      s$cdisc_reports <- data.frame(name = character(), md5 = character())
      s$cdisc_notes <- ""
      s$cdisc_approved <- FALSE
      clear_comparison()
    } else if (boundary == "requirements") {
      s$r_program <- NULL
      s$r_meta <- NULL
      s$r_approved <- FALSE
      s$r_output <- NULL
      s$r_output_meta <- NULL
      s$r_trace <- NULL
      s$r_trace_approved <- FALSE
      s$sas_program <- NULL
      s$sas_meta <- NULL
      s$sas_review <- ""
      s$sas_approved <- FALSE
      s$sas_output <- NULL
      s$sas_output_meta <- NULL
      s$sas_trace <- NULL
      s$sas_trace_approved <- FALSE
      s$cdisc_reports <- data.frame(name = character(), md5 = character())
      s$cdisc_notes <- ""
      s$cdisc_approved <- FALSE
      clear_comparison()
    } else if (boundary == "production_r") {
      s$r_approved <- FALSE
      s$r_output <- NULL
      s$r_output_meta <- NULL
      s$r_trace <- NULL
      s$r_trace_approved <- FALSE
      s$cdisc_reports <- data.frame(name = character(), md5 = character())
      s$cdisc_notes <- ""
      s$cdisc_approved <- FALSE
      clear_comparison()
    } else if (boundary == "trace_r") {
      s$r_trace <- NULL
      s$r_trace_approved <- FALSE
      s$final_approval <- NULL
    } else if (boundary == "sas_review") {
      s$sas_approved <- FALSE
      s$sas_output <- NULL
      s$sas_output_meta <- NULL
      s$sas_trace <- NULL
      s$sas_trace_approved <- FALSE
      s$cdisc_reports <- data.frame(name = character(), md5 = character())
      s$cdisc_notes <- ""
      s$cdisc_approved <- FALSE
      clear_comparison()
    } else if (boundary == "sas_output") {
      s$sas_output <- NULL
      s$sas_output_meta <- NULL
      s$sas_trace <- NULL
      s$sas_trace_approved <- FALSE
      s$cdisc_reports <- data.frame(name = character(), md5 = character())
      s$cdisc_notes <- ""
      s$cdisc_approved <- FALSE
      clear_comparison()
    } else if (boundary == "trace_sas") {
      s$sas_trace <- NULL
      s$sas_trace_approved <- FALSE
      s$final_approval <- NULL
    }
    current <- match(shiny::isolate(s$phase), PIPELINE_STEPS)
    target <- match(boundary, PIPELINE_STEPS)
    if (!is.na(current) && !is.na(target) && current > target) {
      s$phase <- boundary
    }
    ctx$log(
      "Downstream review invalidated",
      paste(boundary, reason, sep = "; ")
    )
  }

  output$workflow_steps <- shiny::renderUI({
    current <- match(s$phase, PIPELINE_STEPS)
    shiny::tags$ol(
      class = "workflow-strip",
      `aria-label` = "Sequential human-gated workflow",
      lapply(seq_along(PIPELINE_STEPS), function(i) {
        state <- if (i < current) {
          "complete"
        } else if (i == current) {
          "active"
        } else {
          "pending"
        }
        shiny::tags$li(
          class = paste("workflow-step", state),
          shiny::span(class = "step-dot", if (state == "complete") "✓" else i),
          shiny::span(PIPELINE_LABELS[[PIPELINE_STEPS[[i]]]])
        )
      })
    )
  })

  output$phase_ui <- shiny::renderUI({
    phase <- s$phase
    ns <- session$ns
    panel <- function(title, subtitle, ...) {
      shiny::tagList(
        shiny::div(
          class = "phase-heading",
          shiny::div(class = "eyebrow", "WORKFLOW PHASE"),
          shiny::h2(title),
          shiny::p(subtitle)
        ),
        shiny::div(class = "panel", ...)
      )
    }
    advance <- function(label, back = NULL, disabled = FALSE) {
      shiny::div(
        class = "phase-actions",
        if (!is.null(back)) {
          shiny::actionButton(
            ns("back"),
            paste0("← ", back),
            class = "btn-ghost"
          )
        },
        shiny::actionButton(
          ns("advance"),
          label,
          class = "btn-primary-wide",
          disabled = if (disabled) NA
        )
      )
    }

    switch(
      phase,
      workspace = panel(
        "Study workspace & source inputs",
        "Register source material. Files are read for this local session only; original files are not changed.",
        shiny::div(
          class = "form-row",
          shiny::textInput(
            ns("reviewer"),
            "Reviewer name (prototype only; unauthenticated)"
          ),
          shiny::textInput(ns("task"), "Task / ADaM dataset"),
          shiny::textInput(
            ns("study_id"),
            "Study identifier (use synthetic ID for demo)"
          )
        ),
        shiny::h3(class = "sub-head", "Study documents"),
        shiny::p(
          class = "field-help",
          "Register protocol, SAP, ADaM specifications, Define-XML/metadata, SDTM specifications, and other references. This prototype does not extract document contents; human-entered citations are required later."
        ),
        shiny::fileInput(
          ns("documents"),
          "Documents (.pdf, .docx, .xlsx, .xml, .csv, .txt)",
          multiple = TRUE,
          accept = c(".pdf", ".docx", ".xlsx", ".xml", ".csv", ".txt")
        ),
        shiny::uiOutput(ns("document_list")),
        shiny::h3(class = "sub-head", "Source SDTM datasets"),
        shiny::fileInput(
          ns("sdtm_files"),
          "Source datasets (.xpt, .sas7bdat, .csv)",
          multiple = TRUE,
          accept = paste0(".", ALLOWED_DATA_EXT)
        ),
        shiny::uiOutput(ns("sdtm_list")),
        advance("Register inputs & continue")
      ),
      requirements = panel(
        "Build and approve the requirements baseline",
        "Manually record traceable rules from approved study documents. Conflicts and assumptions require human resolution.",
        shiny::p(
          class = "field-help",
          "One requirement per line: ID | target dataset.variable | source document/version/page or section | derivation / acceptance rule."
        ),
        shiny::textAreaInput(
          ns("requirement_text"),
          "Study requirements",
          rows = 8,
          placeholder = "REQ-001 | ADSL.SAFFL | ADaM spec v1, §3.1 | Set Y for subjects in the safety population"
        ),
        shiny::textAreaInput(
          ns("conflicts"),
          "Document conflicts or ambiguities (enter 'None' if none)",
          rows = 2
        ),
        shiny::textAreaInput(
          ns("resolution"),
          "Resolution / rationale by requirements reviewer",
          rows = 2
        ),
        shiny::checkboxInput(
          ns("req_attest"),
          "I verified the cited source documents and approve this baseline version.",
          FALSE
        ),
        shiny::actionButton(
          ns("approve_requirements"),
          "Approve & freeze baseline",
          class = "btn-secondary-wide"
        ),
        shiny::uiOutput(ns("requirements_view")),
        advance("Continue to production R", "Study inputs")
      ),
      production_r = panel(
        "Production R program and ADaM generation",
        "Review and authorize the exact production R program. The prototype does not execute uploaded programs; run approved code in an authorized environment and upload its output and run evidence.",
        shiny::fileInput(
          ns("r_program_file"),
          "Production R program (.R, .txt)",
          accept = c(".R", ".r", ".txt")
        ),
        shiny::uiOutput(ns("r_program_view")),
        shiny::textAreaInput(
          ns("r_review"),
          "Reviewer notes and run plan",
          rows = 2
        ),
        shiny::checkboxInput(
          ns("r_attest"),
          "I reviewed this exact R program version and authorize its external execution.",
          FALSE
        ),
        shiny::actionButton(
          ns("approve_r"),
          "Approve production R code",
          class = "btn-secondary-wide"
        ),
        shiny::fileInput(
          ns("r_output_file"),
          "R-produced ADaM output (.xpt, .sas7bdat, .csv)",
          accept = paste0(".", ALLOWED_DATA_EXT)
        ),
        shiny::textInput(
          ns("r_run_info"),
          "R version, execution log reference, and input manifest"
        ),
        shiny::checkboxInput(
          ns("r_output_attest"),
          "I confirm this output came from the approved R program checksum.",
          FALSE
        ),
        shiny::actionButton(
          ns("register_r_output"),
          "Register R ADaM output",
          class = "btn-secondary-wide"
        ),
        shiny::uiOutput(ns("r_output_view")),
        advance("Continue to R traceability", "Requirements")
      ),
      trace_r = pipeline_trace_phase_ui(ns, "R"),
      sas_review = panel(
        "Independent SAS QC program",
        "Use the approved requirements and shared study documents only. This prototype cannot generate SAS. Upload the independently authored draft for human review.",
        shiny::div(
          class = "warn-note",
          "The UI hides the production R source from this screen, but the prototype is one local Shiny process and does NOT technically enforce blinding. Do not treat this prototype as an independent/blinded QC environment."
        ),
        shiny::fileInput(
          ns("sas_program_file"),
          "Independent SAS QC draft (.sas, .txt)",
          accept = c(".sas", ".txt")
        ),
        shiny::uiOutput(ns("sas_program_view")),
        shiny::textAreaInput(
          ns("sas_review"),
          "Human review notes, assumptions, and requirement citations",
          rows = 3
        ),
        shiny::checkboxInput(
          ns("sas_attest"),
          "I reviewed this exact SAS code checksum against the approved requirements and approve it for execution in an authorized SAS environment.",
          FALSE
        ),
        shiny::actionButton(
          ns("approve_sas"),
          "Approve SAS QC code version",
          class = "btn-secondary-wide"
        ),
        advance("Continue to SAS QC output", "R traceability")
      ),
      sas_output = panel(
        "SAS QC output",
        "Run the approved SAS checksum in an authorized SAS environment. SAS execution is external to this prototype.",
        shiny::fileInput(
          ns("sas_output_file"),
          "SAS QC ADaM output (.xpt, .sas7bdat, .csv)",
          accept = paste0(".", ALLOWED_DATA_EXT)
        ),
        shiny::textInput(
          ns("sas_run_info"),
          "SAS version, log reference, and executed program checksum"
        ),
        shiny::checkboxInput(
          ns("sas_output_attest"),
          "I verified the executed checksum matches the human-approved SAS code.",
          FALSE
        ),
        shiny::actionButton(
          ns("register_sas_output"),
          "Register SAS QC output",
          class = "btn-secondary-wide"
        ),
        shiny::uiOutput(ns("sas_output_view")),
        advance("Continue to SAS traceability", "SAS code review")
      ),
      trace_sas = pipeline_trace_phase_ui(ns, "SAS"),
      compare = panel(
        "Deterministic R-versus-SAS comparison",
        "The comparator receives only the completed datasets and approved settings—not program source code.",
        shiny::uiOutput(ns("comparison_inputs")),
        shiny::selectizeInput(
          ns("keys"),
          "Key variables (required; no implicit row-order matching)",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::numericInput(
          ns("tolerance"),
          "Absolute numeric tolerance",
          value = 1e-8,
          min = 0
        ),
        shiny::checkboxInput(
          ns("compare_attest"),
          "I reviewed and approve these comparison settings.",
          FALSE
        ),
        shiny::actionButton(
          ns("run_compare"),
          "Run deterministic comparison",
          class = "btn-primary-wide"
        ),
        shiny::uiOutput(ns("comparison_view")),
        advance("Continue to difference review", "SAS traceability")
      ),
      triage = panel(
        "Review differences and proposed explanations",
        "Comparator results are deterministic. Pattern hints are rule-based, not AI. Explanations are hypotheses—not root-cause findings. Review and disposition every difference.",
        shiny::uiOutput(ns("findings_view")),
        advance("Continue to CDISC checks", "Comparison")
      ),
      cdisc = panel(
        "CDISC conformance review",
        "Run the configured validator outside this prototype. Register the reports for both outputs and record human review. Conformance is separate from specification traceability and output agreement.",
        shiny::textInput(
          ns("validator"),
          "Validator product/version, ruleset, standards and Define-XML versions"
        ),
        shiny::fileInput(
          ns("cdisc_files"),
          "External validator reports (.csv, .txt)",
          multiple = TRUE,
          accept = c(".csv", ".txt")
        ),
        shiny::uiOutput(ns("cdisc_view")),
        shiny::textAreaInput(
          ns("cdisc_notes"),
          "Review notes, dispositions, unresolved validator findings",
          rows = 3
        ),
        shiny::checkboxInput(
          ns("cdisc_attest"),
          "I reviewed the CDISC validation reports for both R and SAS outputs.",
          FALSE
        ),
        shiny::actionButton(
          ns("approve_cdisc"),
          "Record CDISC human review",
          class = "btn-secondary-wide"
        ),
        advance("Continue to report & sign-off", "Difference review")
      ),
      signoff = panel(
        "Consolidated review report and human sign-off",
        "The report keeps traceability, CDISC conformance, deterministic agreement, hypotheses, and human decisions separate. Approval does not confer regulatory validation status.",
        shiny::uiOutput(ns("report_summary")),
        shiny::textAreaInput(
          ns("final_statement"),
          "Reviewer conclusion and open items",
          rows = 3
        ),
        shiny::checkboxInput(
          ns("final_attest"),
          "I verified all conclusions and traceable evidence, reviewed all dispositions, and approve this draft QC package.",
          FALSE
        ),
        shiny::actionButton(
          ns("final_approve"),
          "Record human sign-off",
          class = "btn-primary-wide"
        ),
        shiny::uiOutput(ns("final_view")),
        shiny::downloadButton(
          ns("download_report"),
          "Download consolidated review report (CSV)",
          class = "btn-ghost"
        ),
        shiny::downloadButton(
          ns("download_audit"),
          "Download audit log (CSV)",
          class = "btn-ghost"
        ),
        advance("Sign-off complete", disabled = TRUE)
      )
    )
  })

  output$document_list <- shiny::renderUI({
    if (!nrow(s$docs)) {
      return(shiny::div(class = "empty-note", "No documents registered."))
    }
    shiny::tagList(lapply(seq_len(nrow(s$docs)), function(i) {
      shiny::div(
        class = "artifact-row",
        shiny::strong(s$docs$name[i]),
        shiny::selectInput(
          session$ns(paste0("doc_category_", i)),
          "Document type",
          choices = c(
            "Select type",
            "Protocol",
            "SAP",
            "ADaM specification",
            "Define-XML / metadata",
            "SDTM specification",
            "Controlled terminology",
            "Other"
          ),
          selected = if (s$docs$category[i] == "Unclassified") {
            "Select type"
          } else {
            s$docs$category[i]
          }
        ),
        shiny::textInput(
          session$ns(paste0("doc_version_", i)),
          "Version",
          value = s$docs$version[i]
        ),
        shiny::textInput(
          session$ns(paste0("doc_citation_", i)),
          "Effective date / document citation note",
          value = s$docs$citation_note[i]
        )
      )
    }))
  })
  shiny::observe({
    if (nrow(s$docs)) {
      for (i in seq_len(nrow(s$docs))) {
        s$docs$category[i] <- input[[paste0("doc_category_", i)]] %||%
          s$docs$category[i]
        s$docs$version[i] <- input[[paste0("doc_version_", i)]] %||%
          s$docs$version[i]
        s$docs$citation_note[i] <- input[[paste0("doc_citation_", i)]] %||%
          s$docs$citation_note[i]
      }
    }
  })
  output$sdtm_list <- shiny::renderUI({
    if (!length(s$sdtm_meta)) {
      return(shiny::div(class = "empty-note", "No SDTM datasets registered."))
    }
    rows <- do.call(
      rbind,
      lapply(names(s$sdtm_meta), function(n) {
        data.frame(
          domain_file = n,
          rows = s$sdtm_meta[[n]]$rows,
          columns = s$sdtm_meta[[n]]$cols,
          md5 = s$sdtm_meta[[n]]$md5,
          stringsAsFactors = FALSE
        )
      })
    )
    html_table(rows)
  })
  output$requirements_view <- shiny::renderUI({
    if (is.null(s$requirements)) {
      return(NULL)
    }
    html_table(s$requirements)
  })
  output$r_program_view <- shiny::renderUI({
    if (is.null(s$r_meta)) {
      return(shiny::div(class = "empty-note", "No R program uploaded."))
    }
    shiny::tagList(
      shiny::div(
        class = "loaded-card",
        shiny::strong(s$r_meta$name),
        paste("MD5", s$r_meta$md5),
        paste(s$r_meta$lines, "lines")
      ),
      shiny::tags$pre(class = "code-block", shiny::tags$code(s$r_program))
    )
  })
  output$r_output_view <- shiny::renderUI({
    if (is.null(s$r_output_meta)) {
      return(NULL)
    }
    shiny::div(
      class = "ok-note",
      paste(
        s$r_output_meta$name,
        s$r_output_meta$rows,
        "rows · MD5",
        s$r_output_meta$md5
      )
    )
  })
  output$sas_program_view <- shiny::renderUI({
    if (is.null(s$sas_meta)) {
      return(shiny::div(class = "empty-note", "No SAS draft uploaded."))
    }
    shiny::tagList(
      shiny::div(
        class = "loaded-card",
        shiny::strong(s$sas_meta$name),
        paste("MD5", s$sas_meta$md5)
      ),
      shiny::tags$pre(class = "code-block", shiny::tags$code(s$sas_program))
    )
  })
  output$sas_output_view <- shiny::renderUI({
    if (is.null(s$sas_output_meta)) {
      return(NULL)
    }
    shiny::div(
      class = "ok-note",
      paste(
        s$sas_output_meta$name,
        s$sas_output_meta$rows,
        "rows · MD5",
        s$sas_output_meta$md5
      )
    )
  })
  output$comparison_inputs <- shiny::renderUI({
    if (is.null(s$r_output_meta) || is.null(s$sas_output_meta)) {
      return(shiny::div(
        class = "warn-note",
        "Both R and SAS output datasets are required."
      ))
    }
    shiny::div(
      class = "loaded-grid",
      shiny::div(
        class = "loaded-card",
        shiny::strong("Production R output"),
        paste(
          s$r_output_meta$name,
          s$r_output_meta$rows,
          "rows",
          substr(s$r_output_meta$md5, 1, 12)
        )
      ),
      shiny::div(
        class = "loaded-card",
        shiny::strong("Independent SAS QC output"),
        paste(
          s$sas_output_meta$name,
          s$sas_output_meta$rows,
          "rows",
          substr(s$sas_output_meta$md5, 1, 12)
        )
      )
    )
  })
  output$comparison_view <- shiny::renderUI({
    if (is.null(s$comparison)) {
      return(NULL)
    }
    c <- s$comparison
    shiny::tagList(
      PILL_DETERMINISTIC(),
      shiny::div(
        class = if (c$summary$identical) "ok-note" else "warn-note",
        sprintf(
          "%d value differences across %d variables; unmatched rows: R %d, SAS %d",
          c$summary$total_diffs,
          c$summary$n_vars_diff,
          c$summary$n_only_ref,
          c$summary$n_only_qc
        )
      ),
      html_table(c$var_summary)
    )
  })
  output$findings_view <- shiny::renderUI({
    if (is.null(s$comparison)) {
      return(shiny::div(class = "empty-note", "Run comparison first."))
    }
    if (!length(s$findings)) {
      return(shiny::div(
        class = "ok-note",
        "No comparator differences. Human confirmation will be required at sign-off."
      ))
    }
    shiny::tagList(lapply(seq_along(s$findings), function(i) {
      f <- s$findings[[i]]
      d <- s$triage[[f$id]]
      hint <- f$hint %||% NA_character_
      ns <- session$ns
      shiny::div(
        class = "panel finding-panel",
        shiny::div(
          class = "section-head",
          shiny::h3(paste(f$id, f$title)),
          shiny::span(
            class = paste("severity", tolower(f$severity)),
            f$severity
          )
        ),
        PILL_DETERMINISTIC(),
        shiny::p(f$summary),
        html_table(f$evidence),
        if (!is.na(hint)) {
          shiny::div(class = "hint-card", PILL_HINT(), shiny::p(hint))
        },
        shiny::strong(
          "Application-proposed hypothesis · requires human verification"
        ),
        shiny::selectInput(
          ns(paste0("cause_", i)),
          "Possible cause category",
          choices = c(
            "Select..." = "",
            "Production R logic",
            "SAS QC logic",
            "SDTM source data",
            "Mapping / join / filter",
            "Specification ambiguity / conflict",
            "SAS/R behavior or type conversion",
            "Runtime / version / comparison settings",
            "Inconclusive"
          ),
          selected = d$category %||% ""
        ),
        shiny::textAreaInput(
          ns(paste0("hypothesis_", i)),
          "Hypothesis / suggested next investigation",
          rows = 2,
          value = d$hypothesis %||% hint %||% ""
        ),
        shiny::textInput(
          ns(paste0("source_", i)),
          "Trace references: document section, code line(s), data key/variable",
          value = d$source_ref %||% ""
        ),
        shiny::selectInput(
          ns(paste0("decision_", i)),
          "Human disposition",
          choices = c("Pending", "Verified", "Rejected", "Needs investigation"),
          selected = d$decision %||% "Pending"
        ),
        shiny::textAreaInput(
          ns(paste0("rationale_", i)),
          "Reviewer rationale",
          rows = 2,
          value = d$rationale %||% ""
        ),
        shiny::actionButton(
          ns(paste0("save_", i)),
          "Save review",
          class = "btn-secondary-wide"
        )
      )
    }))
  })
  output$cdisc_view <- shiny::renderUI({
    if (!nrow(s$cdisc_reports)) {
      return(shiny::div(
        class = "empty-note",
        "No external validator report registered."
      ))
    }
    html_table(s$cdisc_reports)
  })
  output$report_summary <- shiny::renderUI({
    checks <- data.frame(
      area = c(
        "Requirements baseline",
        "Production R output",
        "R traceability",
        "SAS code review",
        "SAS output",
        "SAS traceability",
        "Deterministic comparison",
        "Difference dispositions",
        "CDISC review"
      ),
      status = c(
        if (s$requirements_approved) "Human approved" else "Pending",
        if (!is.null(s$r_output)) "Registered" else "Pending",
        if (s$r_trace_approved) "Human approved" else "Pending",
        if (s$sas_approved) "Human approved" else "Pending",
        if (!is.null(s$sas_output)) "Registered" else "Pending",
        if (s$sas_trace_approved) "Human approved" else "Pending",
        if (!is.null(s$comparison)) {
          "Deterministic result recorded"
        } else {
          "Pending"
        },
        if (findings_reviewed(s)) "Human reviewed" else "Pending",
        if (s$cdisc_approved) "Human reviewed" else "Pending"
      ),
      stringsAsFactors = FALSE
    )
    html_table(checks)
  })
  output$final_view <- shiny::renderUI({
    if (is.null(s$final_approval)) {
      return(NULL)
    }
    shiny::div(
      class = "approved-card",
      PILL_HUMAN(),
      shiny::strong("Sign-off recorded — draft prototype package"),
      shiny::p(paste(s$final_approval$reviewer, s$final_approval$time)),
      shiny::p(s$final_approval$statement),
      shiny::p(
        class = "field-help",
        "Not a validated electronic signature, durable audit record, or claim of validated data."
      )
    )
  })

  for (branch in c("R", "SAS")) {
    local({
      b <- branch
      key <- if (b == "R") "r_trace" else "sas_trace"
      output[[paste0(tolower(b), "_trace_table")]] <- shiny::renderUI({
        trace <- s[[key]]
        if (is.null(trace) || !nrow(trace)) {
          return(shiny::div(
            class = "empty-note",
            "No traceability rows recorded."
          ))
        }
        html_table(trace)
      })
    })
  }

  shiny::observe({
    s$reviewer <- trimws(input$reviewer %||% s$reviewer)
    s$task <- trimws(input$task %||% s$task)
    s$study_id <- trimws(input$study_id %||% s$study_id)
  })

  shiny::observeEvent(
    input$documents,
    {
      f <- input$documents
      if (is.null(f)) {
        return()
      }
      if (s$requirements_approved || s$phase != "workspace") {
        reset_after("workspace", "Study document set changed")
      }
      s$docs <- data.frame(
        name = character(), category = character(), version = character(),
        citation_note = character(), md5 = character()
      )
      add <- do.call(
        rbind,
        lapply(seq_len(nrow(f)), function(i) {
          data.frame(
            name = basename(f$name[i]),
            category = "Unclassified",
            version = "Unspecified",
            citation_note = "",
            md5 = unname(tools::md5sum(f$datapath[i])),
            stringsAsFactors = FALSE
          )
        })
      )
      s$docs <- rbind(s$docs[!s$docs$name %in% add$name, , drop = FALSE], add)
      for (i in seq_len(nrow(f))) {
        s$artifacts[[paste0("document:", basename(f$name[i]))]] <- f$datapath[i]
      }
      ctx$log(
        "Study documents registered",
        paste(paste0(add$name, "; md5=", add$md5), collapse = " | ")
      )
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$sdtm_files,
    {
      f <- input$sdtm_files
      if (is.null(f)) {
        return()
      }
      if (s$requirements_approved || s$phase != "workspace") {
        reset_after("workspace", "Source SDTM set changed")
      }
      s$sdtm <- list()
      s$sdtm_meta <- list()
      for (i in seq_len(nrow(f))) {
        tryCatch(
          {
            d <- read_dataset(f$datapath[i], f$name[i])
            nm <- basename(f$name[i])
            s$sdtm[[nm]] <- d
            s$sdtm_meta[[nm]] <- list(
              rows = nrow(d),
              cols = ncol(d),
              md5 = unname(tools::md5sum(f$datapath[i]))
            )
            s$artifacts[[paste0("sdtm:", nm)]] <- f$datapath[i]
            ctx$log(
              "SDTM dataset registered",
              sprintf(
                "%s; rows=%d; cols=%d; md5=%s",
                nm,
                nrow(d),
                ncol(d),
                s$sdtm_meta[[nm]]$md5
              )
            )
          },
          error = function(e) {
            shiny::showNotification(
              paste(f$name[i], conditionMessage(e)),
              type = "error"
            )
          }
        )
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$approve_requirements, {
    text <- trimws(input$requirement_text %||% "")
    lines <- if (nzchar(text)) {
      strsplit(text, "\n", fixed = TRUE)[[1]]
    } else {
      character()
    }
    if (
      !length(lines) ||
        !nzchar(trimws(input$conflicts %||% "")) ||
        !nzchar(trimws(input$resolution %||% "")) ||
        !isTRUE(input$req_attest)
    ) {
      return(shiny::showNotification(
        "Add source-cited requirements, conflict status, resolution rationale, and human attestation.",
        type = "warning"
      ))
    }
    parts <- lapply(lines, function(x) {
      trimws(strsplit(x, "|", fixed = TRUE)[[1]])
    })
    if (
      any(lengths(parts) < 4L) ||
        any(vapply(parts, function(x) any(!nzchar(x[1:4])), logical(1)))
    ) {
      return(shiny::showNotification(
        "Every line needs ID, target, document/version/section citation, and rule.",
        type = "error"
      ))
    }
    ids <- vapply(parts, `[[`, character(1), 1L)
    if (anyDuplicated(ids)) {
      return(shiny::showNotification(
        "Requirement IDs must be unique.",
        type = "error"
      ))
    }
    if (s$requirements_approved) {
      reset_after("requirements", "Approved requirements baseline replaced")
    }
    s$requirements <- do.call(
      rbind,
      lapply(parts, function(p) {
        data.frame(
          requirement_id = p[1],
          target = p[2],
          source_ref = p[3],
          rule = paste(p[-(1:3)], collapse = " | "),
          stringsAsFactors = FALSE
        )
      })
    )
    s$requirements_approved <- TRUE
    ctx$log(
      "Requirements baseline approved",
      sprintf(
        "count=%d; conflicts=%s; resolution=%s",
        nrow(s$requirements),
        trimws(input$conflicts),
        trimws(input$resolution)
      )
    )
  })

  shiny::observeEvent(
    input$r_program_file,
    {
      f <- input$r_program_file
      if (is.null(f)) {
        return()
      }
      tryCatch(
        {
          if (!is.null(s$r_meta)) {
            reset_after("production_r", "Production R program version changed")
          }
          s$r_program <- read_code(f$datapath, f$name)
          s$r_meta <- list(
            name = basename(f$name),
            md5 = unname(tools::md5sum(f$datapath)),
            lines = length(strsplit(s$r_program, "\n")[[1]])
          )
          s$r_approved <- FALSE
          s$r_output <- NULL
          s$r_output_meta <- NULL
          s$r_trace <- NULL
          s$r_trace_approved <- FALSE
          s$sas_approved <- FALSE
          s$sas_output <- NULL
          s$sas_output_meta <- NULL
          s$sas_trace <- NULL
          s$sas_trace_approved <- FALSE
          s$cdisc_approved <- FALSE
          s$cdisc_reports <- data.frame(name = character(), md5 = character())
          clear_comparison()
          s$artifacts[["production_r_program"]] <- f$datapath
          ctx$log(
            "Production R program registered",
            sprintf("%s; md5=%s", s$r_meta$name, s$r_meta$md5)
          )
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        }
      )
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(input$approve_r, {
    if (
      is.null(s$r_meta) ||
        !nzchar(trimws(input$r_review %||% "")) ||
        !isTRUE(input$r_attest)
    ) {
      return(shiny::showNotification(
        "Upload the R program, enter reviewer notes, and approve the exact code version.",
        type = "warning"
      ))
    }
    s$r_approved <- TRUE
    ctx$log(
      "Production R code approved for external run",
      sprintf(
        "program_md5=%s; reviewer_notes=%s",
        s$r_meta$md5,
        trimws(input$r_review)
      )
    )
  })
  shiny::observeEvent(input$register_r_output, {
    f <- input$r_output_file
    if (
      is.null(f) ||
        !isTRUE(s$r_approved) ||
        !isTRUE(input$r_output_attest) ||
        !nzchar(trimws(input$r_run_info %||% ""))
    ) {
      return(shiny::showNotification(
        "Approve R code first; then upload output, run manifest, and confirm provenance.",
        type = "warning"
      ))
    }
    tryCatch(
      {
        d <- read_dataset(f$datapath, f$name)
        reset_after("trace_r", "Production R output registered or replaced")
        s$r_output <- d
        s$r_output_meta <- list(
          name = basename(f$name),
          rows = nrow(d),
          cols = ncol(d),
          md5 = unname(tools::md5sum(f$datapath)),
          program_md5 = s$r_meta$md5,
          run_info = trimws(input$r_run_info)
        )
        s$artifacts[["r_output"]] <- f$datapath
        ctx$log(
          "R ADaM output registered",
          sprintf(
            "%s; rows=%d; md5=%s; program_md5=%s; %s",
            s$r_output_meta$name,
            nrow(d),
            s$r_output_meta$md5,
            s$r_meta$md5,
            s$r_output_meta$run_info
          )
        )
      },
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      }
    )
  })

  shiny::observeEvent(
    input$sas_program_file,
    {
      f <- input$sas_program_file
      if (is.null(f)) {
        return()
      }
      tryCatch(
        {
          if (!is.null(s$sas_meta)) {
            reset_after(
              "sas_review",
              "Independent SAS QC program version changed"
            )
          }
          s$sas_program <- read_code(f$datapath, f$name)
          s$sas_meta <- list(
            name = basename(f$name),
            md5 = unname(tools::md5sum(f$datapath)),
            lines = length(strsplit(s$sas_program, "\n")[[1]])
          )
          s$sas_approved <- FALSE
          s$sas_output <- NULL
          s$sas_output_meta <- NULL
          s$sas_trace <- NULL
          s$sas_trace_approved <- FALSE
          s$cdisc_approved <- FALSE
          s$cdisc_reports <- data.frame(name = character(), md5 = character())
          clear_comparison()
          s$artifacts[["independent_sas_program"]] <- f$datapath
          ctx$log(
            "Independent SAS QC draft registered",
            sprintf("%s; md5=%s", s$sas_meta$name, s$sas_meta$md5)
          )
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        }
      )
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(input$approve_sas, {
    if (
      is.null(s$sas_meta) ||
        !nzchar(trimws(input$sas_review %||% "")) ||
        !isTRUE(input$sas_attest)
    ) {
      return(shiny::showNotification(
        "Upload SAS code, record review notes, and attest approval.",
        type = "warning"
      ))
    }
    s$sas_review <- trimws(input$sas_review)
    s$sas_approved <- TRUE
    ctx$log(
      "SAS QC program approved for external execution",
      sprintf("program_md5=%s; reviewer_notes=%s", s$sas_meta$md5, s$sas_review)
    )
  })
  shiny::observeEvent(input$register_sas_output, {
    f <- input$sas_output_file
    if (
      is.null(f) ||
        !isTRUE(s$sas_approved) ||
        !isTRUE(input$sas_output_attest) ||
        !nzchar(trimws(input$sas_run_info %||% ""))
    ) {
      return(shiny::showNotification(
        "Approve SAS code first; upload output, SAS version/log reference, and executed checksum.",
        type = "warning"
      ))
    }
    if (!grepl(s$sas_meta$md5, input$sas_run_info, fixed = TRUE)) {
      return(shiny::showNotification(
        "Executed program checksum does not match the reviewed SAS code checksum.",
        type = "error"
      ))
    }
    tryCatch(
      {
        d <- read_dataset(f$datapath, f$name)
        reset_after("trace_sas", "SAS QC output registered or replaced")
        s$sas_output <- d
        s$sas_output_meta <- list(
          name = basename(f$name),
          rows = nrow(d),
          cols = ncol(d),
          md5 = unname(tools::md5sum(f$datapath)),
          program_md5 = s$sas_meta$md5,
          run_info = trimws(input$sas_run_info)
        )
        s$artifacts[["sas_output"]] <- f$datapath
        ctx$log(
          "SAS QC output registered",
          sprintf(
            "%s; rows=%d; md5=%s; program_md5=%s; %s",
            s$sas_output_meta$name,
            nrow(d),
            s$sas_output_meta$md5,
            s$sas_meta$md5,
            s$sas_output_meta$run_info
          )
        )
      },
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      }
    )
  })

  for (branch in c("R", "SAS")) {
    local({
      b <- branch
      fkey <- if (b == "R") "r_trace" else "sas_trace"
      akey <- if (b == "R") "r_trace_approved" else "sas_trace_approved"
      output_name <- if (b == "R") "r_output" else "sas_output"
      program_meta <- if (b == "R") "r_meta" else "sas_meta"
      shiny::observeEvent(input[[paste0(tolower(b), "_trace_add")]], {
        reqid <- trimws(input[[paste0(tolower(b), "_req")]] %||% "")
        if (!nzchar(reqid)) {
          return()
        }
        if (!reqid %in% s$requirements$requirement_id) {
          return(shiny::showNotification(
            "Requirement ID is not in the approved baseline.",
            type = "error"
          ))
        }
        row <- data.frame(
          requirement_id = reqid,
          source_ref = trimws(input[[paste0(tolower(b), "_source")]] %||% ""),
          code_ref = trimws(input[[paste0(tolower(b), "_code")]] %||% ""),
          status = input[[paste0(tolower(b), "_status")]] %||% "Unverified",
          evidence = trimws(input[[paste0(tolower(b), "_evidence")]] %||% ""),
          stringsAsFactors = FALSE
        )
        old <- s[[fkey]]
        if (is.null(old)) {
          old <- data.frame(
            requirement_id = character(),
            source_ref = character(),
            code_ref = character(),
            status = character(),
            evidence = character()
          )
        }
        old <- old[old$requirement_id != reqid, , drop = FALSE]
        s[[fkey]] <- rbind(old, row)
        s[[akey]] <- FALSE
        ctx$log(
          paste(b, "traceability row added"),
          paste(reqid, row$status, row$code_ref, sep = "; ")
        )
      })
      shiny::observeEvent(input[[paste0(tolower(b), "_trace_approve")]], {
        x <- s[[fkey]]
        missing <- setdiff(s$requirements$requirement_id, x$requirement_id)
        if (length(missing)) {
          return(shiny::showNotification(
            paste("Missing requirements:", paste(missing, collapse = ", ")),
            type = "warning"
          ))
        }
        if (
          any(
            !nzchar(x$source_ref) | !nzchar(x$code_ref) | !nzchar(x$evidence)
          ) ||
            any(
              !x$status %in%
                c(
                  "Covered",
                  "Partial",
                  "Not covered",
                  "Not applicable",
                  "Unverified"
                )
            )
        ) {
          return(shiny::showNotification(
            "Every trace row requires source, code, coverage, and evidence.",
            type = "warning"
          ))
        }
        if (
          !isTRUE(input[[paste0(tolower(b), "_trace_attest")]]) ||
            is.null(s[[output_name]]) ||
            is.null(s[[program_meta]])
        ) {
          return(shiny::showNotification(
            "Output, program, and human review attestation are required.",
            type = "warning"
          ))
        }
        s[[akey]] <- TRUE
        ctx$log(
          paste(b, "traceability approved"),
          sprintf("requirements=%d", nrow(x))
        )
      })
    })
  }

  output$comparison_view <- shiny::renderUI({
    if (is.null(s$comparison)) {
      return(NULL)
    }
    c <- s$comparison
    shiny::tagList(
      PILL_DETERMINISTIC(),
      shiny::div(
        class = if (c$summary$identical) "ok-note" else "warn-note",
        sprintf(
          "%d differences across %d variables; unmatched R rows %d; unmatched SAS rows %d",
          c$summary$total_diffs,
          c$summary$n_vars_diff,
          c$summary$n_only_ref,
          c$summary$n_only_qc
        )
      ),
      html_table(c$var_summary)
    )
  })
  shiny::observe({
    if (!is.null(s$r_output) && !is.null(s$sas_output)) {
      cols <- intersect(names(s$r_output), names(s$sas_output))
      shiny::updateSelectizeInput(
        session,
        "keys",
        choices = cols,
        selected = intersect(
          c("USUBJID", "STUDYID", "PARAMCD", "AVISITN", "ASEQ"),
          cols
        ),
        server = TRUE
      )
    }
  })

  shiny::observeEvent(
    input$cdisc_files,
    {
      f <- input$cdisc_files
      if (is.null(f)) {
        return()
      }
      s$cdisc_approved <- FALSE
      s$final_approval <- NULL
      s$cdisc_reports <- do.call(
        rbind,
        lapply(seq_len(nrow(f)), function(i) {
          data.frame(
            name = basename(f$name[i]),
            md5 = unname(tools::md5sum(f$datapath[i])),
            stringsAsFactors = FALSE
          )
        })
      )
      for (i in seq_len(nrow(f))) {
        s$artifacts[[paste0("cdisc:", basename(f$name[i]))]] <- f$datapath[i]
      }
      ctx$log(
        "CDISC reports registered",
        paste(s$cdisc_reports$name, collapse = ", ")
      )
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(input$approve_cdisc, {
    if (
      !nrow(s$cdisc_reports) ||
        !nzchar(trimws(input$validator %||% "")) ||
        !nzchar(trimws(input$cdisc_notes %||% "")) ||
        !isTRUE(input$cdisc_attest)
    ) {
      return(shiny::showNotification(
        "Register external reports, validator/ruleset versions, reviewer notes, and attestation.",
        type = "warning"
      ))
    }
    s$cdisc_notes <- trimws(input$cdisc_notes)
    s$cdisc_approved <- TRUE
    ctx$log(
      "CDISC conformance review approved",
      paste(
        input$validator,
        s$cdisc_notes,
        paste(s$cdisc_reports$name, collapse = ","),
        sep = "; "
      )
    )
  })

  shiny::observeEvent(input$run_compare, {
    if (
      is.null(s$r_output) ||
        is.null(s$sas_output) ||
        !isTRUE(s$r_trace_approved) ||
        !isTRUE(s$sas_trace_approved)
    ) {
      return(shiny::showNotification(
        "Both outputs and human-approved traceability reviews are required.",
        type = "warning"
      ))
    }
    if (!isTRUE(input$compare_attest)) {
      return(shiny::showNotification(
        "Approve the comparison profile first.",
        type = "warning"
      ))
    }
    if (
      !length(input$keys) ||
        is.null(input$tolerance) ||
        is.na(input$tolerance) ||
        input$tolerance < 0
    ) {
      return(shiny::showNotification(
        "Choose key variables and a non-negative tolerance.",
        type = "warning"
      ))
    }
    tryCatch(
      {
        c <- compare_datasets(
          s$r_output,
          s$sas_output,
          input$keys,
          tolerance = input$tolerance
        )
        s$comparison <- c
        s$comparison_profile <- list(
          engine = "workbench deterministic comparator",
          version = "prototype",
          keys = input$keys,
          tolerance = input$tolerance
        )
        s$findings <- build_findings(c)
        s$triage <- setNames(
          vector("list", length(s$findings)),
          vapply(s$findings, `[[`, character(1), "id")
        )
        s$cdisc_approved <- FALSE
        s$final_approval <- NULL
        ctx$log(
          "Deterministic comparison completed",
          sprintf(
            "engine=%s; keys=%s; tolerance=%s; findings=%d",
            s$comparison_profile$engine,
            paste(input$keys, collapse = ","),
            input$tolerance,
            length(s$findings)
          )
        )
      },
      error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error",
          duration = 10
        )
      }
    )
  })

  for (i in seq_len(500L)) {
    local({
      idx <- i
      shiny::observeEvent(
        input[[paste0("save_", idx)]],
        {
          if (idx > length(s$findings)) {
            return()
          }
          f <- s$findings[[idx]]
          cat <- input[[paste0("cause_", idx)]] %||% ""
          hyp <- trimws(input[[paste0("hypothesis_", idx)]] %||% "")
          src <- trimws(input[[paste0("source_", idx)]] %||% "")
          dec <- input[[paste0("decision_", idx)]] %||% "Pending"
          rat <- trimws(input[[paste0("rationale_", idx)]] %||% "")
          if (
            !nzchar(cat) ||
              !nzchar(hyp) ||
              !nzchar(src) ||
              dec == "Pending" ||
              !nzchar(rat)
          ) {
            return(shiny::showNotification(
              "Choose a category, enter evidence links and rationale, and record a human disposition.",
              type = "warning"
            ))
          }
          s$triage[[f$id]] <- list(
            category = cat,
            hypothesis = hyp,
            source_ref = src,
            decision = dec,
            rationale = rat,
            reviewer = s$reviewer,
            time = now_stamp()
          )
          ctx$log(
            "Finding human-reviewed",
            sprintf(
              "%s; category=%s; disposition=%s; refs=%s",
              f$id,
              cat,
              dec,
              src
            )
          )
        },
        ignoreInit = TRUE
      )
    })
  }

  shiny::observeEvent(input$final_approve, {
    if (!pipeline_ready_for_signoff(s)) {
      return(shiny::showNotification(
        "Complete all approvals, evidence links, external validator review, and finding dispositions first.",
        type = "warning",
        duration = 10
      ))
    }
    if (
      !nzchar(trimws(input$final_statement %||% "")) ||
        !isTRUE(input$final_attest)
    ) {
      return(shiny::showNotification(
        "Reviewer conclusion and sign-off attestation are required.",
        type = "warning"
      ))
    }
    s$final_approval <- list(
      reviewer = s$reviewer,
      time = now_stamp(),
      statement = trimws(input$final_statement)
    )
    ctx$log("Human sign-off recorded", s$final_approval$statement)
  })

  shiny::observeEvent(input$advance, {
    phase <- s$phase
    if (
      phase == "workspace" &&
        any(s$docs$category == "Unclassified" | !nzchar(s$docs$version))
    ) {
      return(shiny::showNotification(
        "Classify every registered document and record its version before continuing.",
        type = "warning"
      ))
    }
    can_advance <- switch(
      phase,
      workspace = nzchar(s$reviewer) &&
        nzchar(s$task) &&
        nrow(s$docs) > 0 &&
        length(s$sdtm) > 0 &&
        !any(s$docs$category %in% c("Unclassified", "Select type")) &&
        !any(!nzchar(s$docs$version)),
      requirements = isTRUE(s$requirements_approved),
      production_r = isTRUE(s$r_approved) && !is.null(s$r_output),
      trace_r = isTRUE(s$r_trace_approved),
      sas_review = isTRUE(s$sas_approved),
      sas_output = !is.null(s$sas_output),
      trace_sas = isTRUE(s$sas_trace_approved),
      compare = !is.null(s$comparison),
      triage = findings_reviewed(s),
      cdisc = isTRUE(s$cdisc_approved),
      signoff = FALSE,
      FALSE
    )
    if (!can_advance) {
      return(shiny::showNotification(
        "This phase's required inputs and human approvals are not complete.",
        type = "warning",
        duration = 8
      ))
    }
    nxt <- unname(pipeline_next[[phase]])
    pipeline_transition(s, nxt, ctx)
  })
  shiny::observeEvent(input$back, {
    idx <- match(s$phase, PIPELINE_STEPS)
    if (idx > 1) s$phase <- PIPELINE_STEPS[idx - 1L]
  })
  output$download_report <- shiny::downloadHandler(
    filename = function() "adam_qc_review_report.csv",
    content = function(file) {
      utils::write.csv(pipeline_report(s), file, row.names = FALSE)
    }
  )
  output$download_audit <- shiny::downloadHandler(
    filename = function() "adam_qc_audit_log.csv",
    content = function(file) utils::write.csv(s$audit, file, row.names = FALSE)
  )

  ctx$log(
    "Session started",
    "Inputs and results are transient; no uploaded source files are modified."
  )
}

pipeline_trace_phase_ui <- function(ns, branch) {
  prefix <- tolower(branch)
  shiny::tagList(
    shiny::div(
      class = "phase-heading",
      shiny::div(class = "eyebrow", "WORKFLOW PHASE"),
      shiny::h2(paste(branch, "program-to-requirement traceability")),
      shiny::p(
        "Link every approved requirement to source document, source data, program lines, and output evidence. Human review is required."
      )
    ),
    shiny::div(
      class = "panel",
      shiny::uiOutput(ns(paste0(prefix, "_trace_table"))),
      shiny::textInput(ns(paste0(prefix, "_req")), "Requirement ID"),
      shiny::textInput(
        ns(paste0(prefix, "_source")),
        "Document section and SDTM source domain/variable/key"
      ),
      shiny::textInput(
        ns(paste0(prefix, "_code")),
        paste(branch, "program file/version and line range")
      ),
      shiny::selectInput(
        ns(paste0(prefix, "_status")),
        "Coverage",
        choices = c(
          "Covered",
          "Partial",
          "Not covered",
          "Not applicable",
          "Unverified"
        )
      ),
      shiny::textAreaInput(
        ns(paste0(prefix, "_evidence")),
        "Output evidence and reviewer note",
        rows = 2
      ),
      shiny::actionButton(
        ns(paste0(prefix, "_trace_add")),
        "Add / update requirement trace",
        class = "btn-ghost"
      ),
      shiny::checkboxInput(
        ns(paste0(prefix, "_trace_attest")),
        "I reviewed every approved requirement and its source/code/output evidence.",
        FALSE
      ),
      shiny::actionButton(
        ns(paste0(prefix, "_trace_approve")),
        paste("Approve", branch, "traceability review"),
        class = "btn-secondary-wide"
      ),
      shiny::div(
        class = "phase-actions",
        shiny::actionButton(ns("back"), "← Back", class = "btn-ghost"),
        shiny::actionButton(
          ns("advance"),
          if (branch == "R") {
            "Continue to independent SAS QC"
          } else {
            "Continue to comparison"
          },
          class = "btn-primary-wide"
        )
      )
    )
  )
}

findings_reviewed <- function(s) {
  if (!length(s$findings)) {
    return(TRUE)
  }
  all(vapply(
    s$findings,
    function(f) {
      d <- s$triage[[f$id]]
      !is.null(d) &&
        d$decision %in% c("Verified", "Rejected") &&
        nzchar(d$source_ref) &&
        nzchar(d$rationale)
    },
    logical(1)
  ))
}

pipeline_ready_for_signoff <- function(s) {
  isTRUE(s$requirements_approved) &&
    isTRUE(s$r_approved) &&
    !is.null(s$r_output) &&
    isTRUE(s$r_trace_approved) &&
    isTRUE(s$sas_approved) &&
    !is.null(s$sas_output) &&
    isTRUE(s$sas_trace_approved) &&
    !is.null(s$comparison) &&
    findings_reviewed(s) &&
    isTRUE(s$cdisc_approved)
}

pipeline_report <- function(s) {
  rows <- list()
  add <- function(section, item, status, evidence = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      section = section,
      item = item,
      status = status,
      evidence = evidence,
      stringsAsFactors = FALSE
    )
  }
  add(
    "Task",
    "Task / study",
    paste(s$task, s$study_id, sep = " / "),
    paste("Reviewer:", s$reviewer)
  )
  for (i in seq_len(nrow(s$docs))) {
    add(
      "Study document",
      s$docs$name[i],
      s$docs$category[i],
      paste(
        "version",
        s$docs$version[i],
        "citation",
        s$docs$citation_note[i],
        "MD5",
        s$docs$md5[i]
      )
    )
  }
  if (!is.null(s$requirements)) {
    for (i in seq_len(nrow(s$requirements))) {
      add(
        "Requirement",
        s$requirements$requirement_id[i],
        s$requirements$target[i],
        paste(s$requirements$source_ref[i], s$requirements$rule[i])
      )
    }
  }
  if (!is.null(s$r_output_meta)) {
    add(
      "Production R output",
      s$r_output_meta$name,
      "Registered",
      paste(
        "rows",
        s$r_output_meta$rows,
        "md5",
        s$r_output_meta$md5,
        "program md5",
        s$r_output_meta$program_md5,
        s$r_output_meta$run_info
      )
    )
  }
  if (!is.null(s$r_trace)) {
    for (i in seq_len(nrow(s$r_trace))) {
      add(
        "R traceability",
        s$r_trace$requirement_id[i],
        s$r_trace$status[i],
        paste(
          s$r_trace$source_ref[i],
          s$r_trace$code_ref[i],
          s$r_trace$evidence[i]
        )
      )
    }
  }
  if (!is.null(s$sas_meta)) {
    add(
      "Independent SAS QC program",
      s$sas_meta$name,
      if (s$sas_approved) "Human approved" else "Pending",
      paste("MD5", s$sas_meta$md5, s$sas_review)
    )
  }
  if (!is.null(s$sas_output_meta)) {
    add(
      "SAS QC output",
      s$sas_output_meta$name,
      "Registered",
      paste(
        "rows",
        s$sas_output_meta$rows,
        "md5",
        s$sas_output_meta$md5,
        "program md5",
        s$sas_output_meta$program_md5,
        s$sas_output_meta$run_info
      )
    )
  }
  if (!is.null(s$sas_trace)) {
    for (i in seq_len(nrow(s$sas_trace))) {
      add(
        "SAS traceability",
        s$sas_trace$requirement_id[i],
        s$sas_trace$status[i],
        paste(
          s$sas_trace$source_ref[i],
          s$sas_trace$code_ref[i],
          s$sas_trace$evidence[i]
        )
      )
    }
  }
  if (!is.null(s$comparison)) {
    add(
      "Deterministic comparator",
      s$comparison_profile$engine,
      "Result",
      sprintf(
        "keys=%s; tolerance=%g; differences=%d",
        paste(s$comparison_profile$keys, collapse = ","),
        s$comparison_profile$tolerance,
        s$comparison$summary$total_diffs
      )
    )
  }
  for (f in s$findings) {
    d <- s$triage[[f$id]]
    add(
      "Comparator finding",
      paste(f$id, f$title),
      if (is.null(d)) "Pending" else d$decision,
      paste(
        f$summary,
        d$category %||% "",
        d$hypothesis %||% "",
        d$source_ref %||% "",
        d$rationale %||% "",
        sep = " | "
      )
    )
    if (nrow(f$evidence)) {
      for (j in seq_len(nrow(f$evidence))) {
        add(
          "Comparator evidence",
          paste(f$id, "row", j),
          "Deterministic",
          paste(
            names(f$evidence),
            unlist(f$evidence[j, , drop = FALSE], use.names = FALSE),
            sep = "=",
            collapse = " | "
          )
        )
      }
    }
  }
  for (i in seq_len(nrow(s$cdisc_reports))) {
    add(
      "CDISC validator report",
      s$cdisc_reports$name[i],
      s$cdisc_engine,
      paste("MD5", s$cdisc_reports$md5[i], s$cdisc_notes)
    )
  }
  if (!is.null(s$final_approval)) {
    add(
      "Human sign-off",
      s$final_approval$reviewer,
      s$final_approval$time,
      s$final_approval$statement
    )
  }
  do.call(rbind, rows)
}
