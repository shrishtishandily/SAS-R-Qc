source("app.R")

empty_documents <- data.frame(
  name = character(),
  category = character(),
  version = character(),
  citation_note = character(),
  md5 = character()
)
first_batch <- data.frame(
  name = c("protocol.pdf", "sap.pdf"),
  category = c("Protocol", "SAP"),
  version = c("1", "1"),
  citation_note = c("", ""),
  md5 = c("protocol-v1", "sap-v1")
)
second_batch <- data.frame(
  name = "define.xml",
  category = "Define-XML / metadata",
  version = "1",
  citation_note = "",
  md5 = "define-v1"
)
combined <- merge_document_registry(first_batch, second_batch)
stopifnot(
  nrow(combined) == 3L,
  setequal(combined$name, c("protocol.pdf", "sap.pdf", "define.xml"))
)
replacement <- transform(first_batch[1, , drop = FALSE], md5 = "protocol-v2")
combined <- merge_document_registry(combined, replacement)
stopifnot(
  nrow(combined) == 3L,
  combined$md5[combined$name == "protocol.pdf"] == "protocol-v2"
)
stopifnot(nrow(merge_document_registry(empty_documents, first_batch)) == 2L)

first_sdtm_batch <- list("dm.xpt" = data.frame(USUBJID = "SYN-001"))
second_sdtm_batch <- list("ae.xpt" = data.frame(USUBJID = "SYN-001"))
all_sdtm <- merge_dataset_registry(first_sdtm_batch, second_sdtm_batch)
stopifnot(setequal(names(all_sdtm), c("dm.xpt", "ae.xpt")))
replaced_sdtm <- merge_dataset_registry(
  all_sdtm,
  list("dm.xpt" = data.frame(USUBJID = "SYN-002"))
)
stopifnot(
  length(replaced_sdtm) == 2L,
  replaced_sdtm[["dm.xpt"]]$USUBJID == "SYN-002"
)
supporting_adam <- merge_dataset_registry(
  list("adsl.xpt" = data.frame(USUBJID = "SYN-001")),
  list("advs.xpt" = data.frame(USUBJID = "SYN-001"))
)
stopifnot(setequal(names(supporting_adam), c("adsl.xpt", "advs.xpt")))

shiny::testServer(pipeline_server, {
  stopifnot(s$phase == "workspace")

  dm_path <- tempfile(fileext = ".csv")
  ae_path <- tempfile(fileext = ".csv")
  adsl_path <- tempfile(fileext = ".csv")
  adlb_path <- tempfile(fileext = ".csv")
  warmup_sdtm_path <- tempfile(fileext = ".csv")
  warmup_adam_path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(USUBJID = "SYN-001"), dm_path, row.names = FALSE)
  utils::write.csv(
    data.frame(USUBJID = "SYN-001", AETERM = "Example"),
    ae_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(USUBJID = "SYN-001", SAFFL = "Y"),
    adsl_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(USUBJID = "SYN-001", AVAL = 12),
    adlb_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(USUBJID = "WARMUP"),
    warmup_sdtm_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(USUBJID = "WARMUP"),
    warmup_adam_path,
    row.names = FALSE
  )

  upload_record <- function(path) {
    data.frame(
      name = basename(path),
      size = file.info(path)[["size"]],
      type = "text/csv",
      datapath = path,
      stringsAsFactors = FALSE
    )
  }
  session$setInputs(sdtm_files = upload_record(warmup_sdtm_path))
  session$setInputs(sdtm_files = upload_record(dm_path))
  session$setInputs(sdtm_files = upload_record(ae_path))
  stopifnot(setequal(names(s$sdtm), c(basename(dm_path), basename(ae_path))))

  session$setInputs(supporting_adam_files = upload_record(warmup_adam_path))
  session$setInputs(supporting_adam_files = upload_record(adsl_path))
  session$setInputs(supporting_adam_files = upload_record(adlb_path))
  stopifnot(setequal(
    names(s$supporting_adam),
    c(basename(warmup_adam_path), basename(adsl_path), basename(adlb_path))
  ))

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
  s$requirement_candidates <- data.frame(
    requirement_id = "REQ-001",
    target = "ADVS.BASE",
    source_ref = "synthetic-adam-spec.pdf v1.0; section 1",
    rule = "Extracted synthetic baseline requirement",
    source_file = "synthetic-adam-spec.pdf",
    evidence = "Synthetic baseline requirement excerpt",
    extraction_type = "Synthetic test candidate",
    stringsAsFactors = FALSE
  )
  session$setInputs(advance = 2)
  stopifnot(s$phase == "requirements")

  s$ai_requirement_suggestions <- transform(
    s$requirement_candidates,
    rule = "Local model proposal requiring reviewer disposition"
  )
  session$setInputs(
    requirement_text = "REQ-001 | ADVS.BASE | ADaM spec v1, section 1 | Derive baseline from source VS",
    conflicts = "None",
    resolution = "Reviewed against synthetic specification.",
    req_attest = TRUE,
    approve_requirements = 1
  )
  stopifnot(!s$requirements_approved)
  session$setInputs(save_ai_requirement_review = 1)
  stopifnot(!s$ai_requirement_suggestions_reviewed)
  session$setInputs(
    ai_requirement_review_note = "Accepted after checking the synthetic spec.",
    ai_requirement_review_decision = "accept",
    ai_requirement_review_attest = TRUE,
    save_ai_requirement_review = 2
  )
  stopifnot(s$ai_requirement_suggestions_reviewed)
  session$setInputs(approve_requirements = 2)
  stopifnot(!s$requirements_approved)
  session$setInputs(append_ai_requirements = 1)
  stopifnot(nrow(s$requirement_candidates) == 2L)
  session$setInputs(approve_requirements = 3)
  stopifnot(s$requirements_approved, s$requirements$requirement_id == "REQ-001")
  stopifnot(
    s$requirements$source_file == "synthetic-adam-spec.pdf",
    s$requirements$evidence == "Synthetic baseline requirement excerpt",
    s$requirements$extraction_type == "Synthetic test candidate"
  )
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

  unlink(c(
    dm_path,
    ae_path,
    adsl_path,
    adlb_path,
    warmup_sdtm_path,
    warmup_adam_path
  ))
})
