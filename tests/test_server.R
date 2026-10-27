for (f in c("utils", "workflow", "audit", "io", "compare", "findings", "exec", "data",
            "mod_intake", "mod_code", "mod_compare", "mod_findings", "mod_approval", "mod_audit", "ui", "server")) {
  source(file.path("R", paste0(f, ".R")))
}

# Headless walk through the full workflow using the synthetic demo; no browser or app launch.
shiny::testServer(app_server, {
  stopifnot(store$state == "intake")

  session$setInputs(`intake-reviewer` = "Test Reviewer", `intake-demo` = 1)
  stopifnot(!is.null(store$ref), !is.null(store$qc))

  session$setInputs(`intake-confirm` = 1)
  stopifnot(store$state == "configured")

  session$setInputs(`code-items` = "Missing-value handling reviewed", `code-note` = "", `code-record` = 1)
  stopifnot(!is.null(store$code_review))

  session$setInputs(`compare-keys` = c("USUBJID", "PARAMCD", "AVISITN"), `compare-tol` = 1e-8, `compare-run` = 1)
  stopifnot(store$state == "compared", length(store$findings) == 1L)
  stopifnot(store$comparison$diffs$ref_value == "66", store$comparison$diffs$qc_value == "55")

  # Approval is rejected before the finding has a human disposition.
  session$setInputs(`approval-statement` = "x", `approval-attest` = TRUE, `approval-approve` = 1)
  stopifnot(store$state == "compared")

  session$setInputs(`findings-sel` = "F-001", `findings-decision` = DISPOSITION_CHOICES[1],
                    `findings-rationale` = "Synthetic missing-ADT baseline mismatch.", `findings-record` = 1)
  stopifnot(store$state == "dispositioned", length(store$dispositions) == 1L)

  session$setInputs(`approval-statement` = "QC outcome approved.", `approval-attest` = TRUE, `approval-approve` = 2)
  stopifnot(store$state == "approved", !is.null(store$approval))

  # Changing an input after approval discards results and returns to intake.
  session$setInputs(`intake-demo` = 2)
  stopifnot(store$state == "intake", is.null(store$approval), length(store$dispositions) == 0L)

  events <- store$audit$event
  stopifnot(all(c("Comparison run", "Human disposition recorded", "Human approval recorded", "Results invalidated") %in% events))
  stopifnot(identical(store$audit$seq, seq_len(nrow(store$audit))))
})
