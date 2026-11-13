source("app.R")

shiny::testServer(pipeline_server, {
  stopifnot(s$phase == "workspace")

  session$setInputs(advance = 1)
  stopifnot(s$phase == "workspace")

  s$reviewer <- "Test reviewer"
  s$task <- "Synthetic ADVS QC"
  s$study_id <- "SYN-001"
  s$docs <- data.frame(
    name = "synthetic-adam-spec.pdf",
    category = "ADaM specification",
    version = "1.0",
    citation_note = "Section 1",
    md5 = "synthetic",
    stringsAsFactors = FALSE
  )
  s$sdtm <- list("vs.xpt" = data.frame(USUBJID = "SYN-001", VSORRES = "70"))
  session$setInputs(advance = 2)
  stopifnot(s$phase == "requirements")

  session$setInputs(
    requirement_text = "REQ-001 | ADVS.BASE | ADaM spec v1, section 1 | Derive baseline from source VS",
    conflicts = "None",
    resolution = "Reviewed against synthetic specification.",
    req_attest = TRUE,
    approve_requirements = 1
  )
  stopifnot(s$requirements_approved, s$requirements$requirement_id == "REQ-001")
  session$setInputs(advance = 3)
  stopifnot(s$phase == "production_r")

  demo <- demo_datasets()
  s$r_meta <- list(name = "production.R", md5 = "r-code-hash", lines = 10L)
  s$r_approved <- TRUE
  s$r_output <- demo$ref
  s$r_output_meta <- list(
    name = "advs_r.xpt",
    rows = nrow(demo$ref),
    cols = ncol(demo$ref),
    md5 = "r-out",
    program_md5 = "r-code-hash",
    run_info = "synthetic test"
  )
  session$setInputs(advance = 4)
  stopifnot(s$phase == "trace_r")

  s$r_trace <- data.frame(
    requirement_id = "REQ-001",
    source_ref = "spec section 1; VS.VSORRES",
    code_ref = "production.R lines 4-8",
    status = "Covered",
    evidence = "synthetic baseline rows",
    stringsAsFactors = FALSE
  )
  s$r_trace_approved <- TRUE
  session$setInputs(advance = 5)
  stopifnot(s$phase == "sas_review")

  s$sas_meta <- list(name = "qc.sas", md5 = "sas-code-hash", lines = 20L)
  s$sas_program <- "/* synthetic independent QC program */"
  s$sas_approved <- TRUE
  session$setInputs(advance = 6)
  stopifnot(s$phase == "sas_output")

  s$sas_output <- demo$qc
  s$sas_output_meta <- list(
    name = "advs_sas.xpt",
    rows = nrow(demo$qc),
    cols = ncol(demo$qc),
    md5 = "sas-out",
    program_md5 = "sas-code-hash",
    run_info = "SAS version; synthetic test"
  )
  session$setInputs(advance = 7)
  stopifnot(s$phase == "trace_sas")

  s$sas_trace <- data.frame(
    requirement_id = "REQ-001",
    source_ref = "spec section 1; VS.VSORRES",
    code_ref = "qc.sas lines 5-12",
    status = "Covered",
    evidence = "synthetic baseline rows",
    stringsAsFactors = FALSE
  )
  s$sas_trace_approved <- TRUE
  session$setInputs(advance = 8)
  stopifnot(s$phase == "compare")

  session$setInputs(
    keys = c("USUBJID", "PARAMCD", "AVISITN"),
    tolerance = 1e-8,
    compare_attest = TRUE,
    run_compare = 1
  )
  stopifnot(!is.null(s$comparison), s$comparison$summary$total_diffs == 1L)
  stopifnot(
    s$comparison$diffs$ref_value == "66",
    s$comparison$diffs$qc_value == "55"
  )
  session$setInputs(advance = 9)
  stopifnot(s$phase == "triage")

  finding <- s$findings[[1]]
  s$triage[[finding$id]] <- list(
    category = "SDTM source data",
    hypothesis = "Synthetic ADT missingness fixture",
    source_ref = "REQ-001; key SYN-003/WT/0; ADT",
    decision = "Verified",
    rationale = "Reviewed the synthetic fixture and comparator evidence.",
    reviewer = s$reviewer,
    time = now_stamp()
  )
  session$setInputs(advance = 10)
  stopifnot(s$phase == "cdisc")

  s$cdisc_reports <- data.frame(
    name = "synthetic-validator.csv",
    md5 = "report-hash"
  )
  s$cdisc_engine <- "Synthetic external validator fixture"
  s$cdisc_approved <- TRUE
  session$setInputs(advance = 11)
  stopifnot(s$phase == "signoff")

  session$setInputs(
    final_statement = "Synthetic workflow reviewed.",
    final_attest = TRUE,
    final_approve = 1
  )
  stopifnot(!is.null(s$final_approval))
  stopifnot(all(
    c(
      "Requirements baseline approved",
      "Workflow phase advanced",
      "Deterministic comparison completed",
      "Human sign-off recorded"
    ) %in%
      s$audit$event
  ))
})
