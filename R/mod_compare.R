DEFAULT_KEY_CANDIDATES <- c("STUDYID", "USUBJID", "PARAMCD", "AVISITN", "VISITNUM", "ASEQ", "AESEQ")

mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "panel",
      shiny::div(class = "section-head", shiny::h2("Comparison settings")),
      shiny::uiOutput(ns("settings"))
    ),
    shiny::uiOutput(ns("results"))
  )
}

mod_compare_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store

    common_cols <- shiny::reactive({
      if (is.null(store$ref) || is.null(store$qc)) character() else intersect(names(store$ref), names(store$qc))
    })

    output$settings <- shiny::renderUI({
      ns <- session$ns
      if (store$state == "intake") {
        return(shiny::div(class = "empty-note", "Load datasets and confirm intake first."))
      }
      cols <- common_cols()
      guess <- intersect(DEFAULT_KEY_CANDIDATES, cols)
      if (!is.null(store$demo_keys)) guess <- intersect(store$demo_keys, cols)
      locked <- store$state != "configured"
      shiny::tagList(
        shiny::div(
          class = "form-row",
          shiny::selectizeInput(ns("keys"), "Key variables (empty = compare by row order)", choices = cols,
                                selected = shiny::isolate(input$keys) %||% guess, multiple = TRUE),
          shiny::numericInput(ns("tol"), "Numeric tolerance (absolute)", value = shiny::isolate(input$tol) %||% 1e-8, min = 0)
        ),
        shiny::p(class = "field-help", "Character values are trimmed and blank equals missing. Column names are upper-cased. Duplicate keys are reported and only the first record per key is compared."),
        if (locked) {
          shiny::actionButton(ns("reopen"), "Edit settings and re-run", class = "btn-ghost")
        } else {
          shiny::actionButton(ns("run"), "Run comparison", class = "btn-primary-wide")
        }
      )
    })

    shiny::observeEvent(input$run, {
      shiny::req(store$state == "configured", !is.null(store$ref), !is.null(store$qc))
      tol <- input$tol
      if (is.null(tol) || is.na(tol) || tol < 0) {
        return(shiny::showNotification("Tolerance must be a non-negative number.", type = "error"))
      }
      keys <- input$keys %||% character()
      shiny::withProgress(message = "Comparing datasets", value = 0.5, {
        tryCatch({
          cmp <- compare_datasets(store$ref, store$qc, keys, tolerance = tol)
          store$comparison <- cmp
          store$findings <- build_findings(cmp)
          store$dispositions <- list()
          ctx$transition("compared")
          s <- cmp$summary
          ctx$log("Comparison run", sprintf("mode=%s; keys=%s; tolerance=%s; matched=%d; findings=%d; ref_md5=%s; qc_md5=%s",
                                            s$key_mode, paste(keys, collapse = ","), format(tol), s$n_matched, length(store$findings),
                                            store$ref_meta$md5, store$qc_meta$md5))
          ctx$goto("findings")
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
          ctx$log("Comparison failed", conditionMessage(e))
        })
      })
    })

    shiny::observeEvent(input$reopen, {
      ctx$invalidate("Settings reopened for re-run", to = "configured")
    })

    metric <- function(label, value, kind = "") {
      shiny::div(class = paste("metric", kind), shiny::div(class = "metric-value", value), shiny::div(class = "metric-label", label))
    }

    output$results <- shiny::renderUI({
      cmp <- store$comparison
      if (is.null(cmp)) return(NULL)
      s <- cmp$summary
      st <- cmp$structure
      shiny::div(
        class = "panel",
        shiny::div(class = "section-head", shiny::h2("Comparison results"), PILL_DETERMINISTIC()),
        if (s$identical) {
          shiny::div(class = "ok-note", "No differences found: structure, records, and values match within tolerance.")
        } else {
          shiny::div(class = "warn-note", sprintf("%d difference(s) across %d variable(s). See the Findings tab for evidence and disposition.", s$total_diffs, s$n_vars_diff))
        },
        shiny::div(
          class = "metric-grid",
          metric("Reference rows", format(s$n_ref, big.mark = ",")),
          metric("QC rows", format(s$n_qc, big.mark = ",")),
          metric("Matched records", format(s$n_matched, big.mark = ",")),
          metric("Only in reference", s$n_only_ref, if (s$n_only_ref) "bad"),
          metric("Only in QC", s$n_only_qc, if (s$n_only_qc) "bad"),
          metric("Variables differing", sprintf("%d / %d", s$n_vars_diff, s$n_vars_compared), if (s$n_vars_diff) "bad")
        ),
        shiny::p(class = "field-help", sprintf("Matched by %s%s; tolerance %s.", s$key_mode, if (length(s$keys)) paste0(" (", paste(s$keys, collapse = ", "), ")") else "", format(s$tolerance))),
        if (!is.null(cmp$var_summary)) {
          shiny::tagList(
            shiny::h3(class = "sub-head", "Per-variable summary"),
            html_table(cmp$var_summary[, c("variable", "n_compared", "n_diff", "n_missing_in_qc", "n_missing_in_ref", "n_value_mismatch", "max_abs_diff")])
          )
        },
        if (s$truncated) shiny::p(class = "field-help", "Stored differences were capped at 50,000 rows; counts above remain exact.")
      )
    })
  })
}
