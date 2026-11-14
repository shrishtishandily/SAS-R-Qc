source("app.R")

old_url <- Sys.getenv("ADAM_QC_LOCAL_LLM_URL", unset = NA_character_)
on.exit(
  if (is.na(old_url)) {
    Sys.unsetenv("ADAM_QC_LOCAL_LLM_URL")
  } else {
    Sys.setenv(ADAM_QC_LOCAL_LLM_URL = old_url)
  },
  add = TRUE
)
Sys.setenv(ADAM_QC_LOCAL_LLM_URL = "http://example.com:11434")
stopifnot(inherits(try(local_llm_endpoint(), silent = TRUE), "try-error"))
Sys.setenv(ADAM_QC_LOCAL_LLM_URL = "http://127.0.0.1:11434")
stopifnot(identical(local_llm_endpoint(), "http://127.0.0.1:11434"))

old_chat <- local_llm_chat_json
mock_payload <- NULL
mock_system <- NULL
mock_mode <- NULL
local_llm_chat_json <- function(
  model,
  system_prompt,
  user_payload,
  timeout = 180
) {
  mock_payload <<- user_payload
  mock_system <<- system_prompt
  if (grepl("requirement extraction", system_prompt, fixed = TRUE)) {
    list(
      requirements = list(
        list(
          target = "ADAE.ASTDT",
          proposed_rule = "Use the documented date rule.",
          evidence_ids = c("SRC-1", "NOT-SUPPLIED"),
          confidence = "low",
          open_question = "Confirm partial dates."
        ),
        list(
          target = "ADAE.AESEQ",
          proposed_rule = "Invented rule.",
          evidence_ids = "NOT-SUPPLIED",
          confidence = "high",
          open_question = ""
        )
      )
    )
  } else if (
    grepl("Draft an independent SAS QC", system_prompt, fixed = TRUE)
  ) {
    list(
      sas_code = "data qc;\nset sdtm;\nrun;",
      assumptions = list("Confirm source domain mapping."),
      requirement_map = list(list(
        requirement_id = "REQ-001",
        code_lines = c(2, 999)
      ))
    )
  } else if (
    grepl("Review the supplied program", system_prompt, fixed = TRUE)
  ) {
    list(
      reviews = list(
        list(
          requirement_id = "REQ-001",
          status = "partial",
          code_lines = c(2, 999),
          evidence_quote = "derive()",
          explanation = "Check rule.",
          uncertainty = "Sample only.",
          follow_up = "Review source."
        ),
        list(
          requirement_id = "INVALID",
          status = "covered",
          code_lines = 1,
          evidence_quote = "",
          explanation = "",
          uncertainty = "",
          follow_up = ""
        )
      )
    )
  } else if (grepl("Review one ADaM output", system_prompt, fixed = TRUE)) {
    list(
      reviews = list(list(
        requirement_id = "REQ-001",
        status = "possible_issue",
        variables = c("ASTDT", "NOT_A_VARIABLE"),
        sample_row_numbers = c(2, 99),
        observed_evidence = "Sample date exists.",
        explanation = "Check date derivation.",
        uncertainty = "Only first five rows.",
        follow_up = "Inspect all records."
      ))
    )
  } else if (grepl("Analyze only", system_prompt, fixed = TRUE)) {
    list(
      analyses = list(
        list(
          finding_id = "F-001",
          possible_cause = "R logic",
          hypothesis = "Possible date handling issue.",
          proposed_remediation = "Review the partial-date derivation; do not edit code yet.",
          requirement_ids = c("REQ-001", "BAD-REQ"),
          confidence = "low",
          next_check = "Inspect date conversion."
        ),
        list(
          finding_id = "FAKE",
          possible_cause = "invented",
          hypothesis = "bad",
          requirement_ids = "REQ-001"
        )
      )
    )
  } else if (grepl("Summarize this QC report", system_prompt, fixed = TRUE)) {
    list(
      summary = "Draft summary.",
      cited_refs = c("REPORT-0001", "REPORT-9999"),
      open_questions = list("Resolve issue."),
      limitations = list("Not full validation.")
    )
  } else {
    stop("Unexpected mocked LLM task.")
  }
}

req <- data.frame(
  requirement_id = "REQ-001",
  target = "ADAE.ASTDT",
  rule = "Derive event start date",
  source_ref = "SAP v1; page 2",
  evidence = "Use AESTDTC",
  source_file = "SAP.pdf",
  extraction_type = "PDF text passage",
  stringsAsFactors = FALSE
)
candidates <- data.frame(
  requirement_id = "SRC-1",
  target = "ADAE.ASTDT",
  source_ref = "SAP.pdf v1; PDF page 2",
  rule = "Draft source excerpt",
  source_file = "SAP.pdf",
  evidence = "Use AESTDTC",
  extraction_type = "PDF text passage",
  stringsAsFactors = FALSE
)
proposals <- local_llm_propose_requirements(candidates, "mock-local")
stopifnot(nrow(proposals) == 1L)
stopifnot(proposals$source_ref == candidates$source_ref)
stopifnot(grepl("SRC-1", proposals$extraction_type, fixed = TRUE))

review <- local_llm_review_program(
  "line one\nderive()",
  req,
  "mock-local",
  "SAS"
)
stopifnot(nrow(review) == 1L, review$code_ref == "SAS lines 2")

sas <- local_llm_build_sas(
  req,
  list("ae.xpt" = data.frame(USUBJID = "S1", AETERM = "Example")),
  "mock-local"
)
stopifnot(grepl("data qc", sas$code, fixed = TRUE))
stopifnot(identical(unlist(sas$requirement_map[[1]]$code_lines), 2L))
stopifnot(!grepl("production_secret", mock_payload, fixed = TRUE))
stopifnot(grepl("ae.xpt", mock_payload, fixed = TRUE))

output_data <- data.frame(
  USUBJID = paste0("S", 1:8),
  ASTDT = as.Date("2024-01-01") + 1:8
)
output_review <- local_llm_review_dataset(
  output_data,
  "adae.xpt",
  req,
  "mock-local",
  "R ADaM"
)
stopifnot(nrow(output_review) == 1L, output_review$sample_rows == "2")
stopifnot(output_review$variables == "ASTDT")
stopifnot(grepl("sample-based", output_review$provenance, fixed = TRUE))

finding <- list(
  id = "F-001",
  title = "ASTDT mismatch",
  summary = "One value differs.",
  evidence = data.frame(
    key = "S1",
    variable = "ASTDT",
    ref_value = "1",
    qc_value = "2"
  ),
  hint = "Offset"
)
triage <- local_llm_triage(list(finding), req, "mock-local")
stopifnot(
  nrow(triage) == 1L,
  triage$finding_id == "F-001",
  triage$requirement_refs == "REQ-001",
  grepl("do not edit code yet", triage$proposed_remediation, fixed = TRUE)
)

report <- data.frame(
  section = "Finding",
  item = "F-001",
  status = "Pending",
  evidence = "S1 ASTDT differs"
)
summary <- local_llm_summarize_report(report, "mock-local")
stopifnot(summary$cited_refs == "REPORT-0001")
stopifnot(grepl("human verification", summary$provenance, fixed = TRUE))

local_llm_chat_json <- old_chat

shiny::testServer(pipeline_server, {
  stopifnot(!s$llm_enabled)
  s$llm_available <- FALSE
  session$setInputs(enable_local_llm = 1, local_llm_ack = TRUE)
  stopifnot(!s$llm_enabled)
  session$setInputs(ai_requirements = 1, ai_review_r = 1, generate_sas_llm = 1)
  stopifnot(
    !s$llm_enabled,
    !nrow(s$ai_requirement_suggestions),
    is.null(s$sas_program)
  )

  s$requirements_approved <- TRUE
  s$requirements <- data.frame(
    requirement_id = "REQ-001",
    target = "ADAE.ASTDT",
    rule = "Derive date",
    source_ref = "SAP p2",
    evidence = "AESTDTC"
  )
  s$r_meta <- list(name = "production.R", md5 = "r-md5")
  s$r_ai_review <- data.frame(requirement_id = "REQ-001")
  session$setInputs(r_review = "Reviewed", r_attest = TRUE, approve_r = 1)
  stopifnot(!s$r_approved)
  session$setInputs(
    r_ai_review_note = "Reviewed the proposed finding.",
    r_ai_review_attest = TRUE,
    save_r_ai_review = 1
  )
  stopifnot(s$r_ai_review_reviewed)
  session$setInputs(approve_r = 2)
  stopifnot(s$r_approved)

  s$phase <- "production_r"
  s$r_output <- data.frame(USUBJID = "S1", ASTDT = as.Date("2024-01-02"))
  s$r_output_meta <- list(name = "ADAE.xpt")
  s$r_output_ai_review <- data.frame(requirement_id = "REQ-001")
  s$r_output_ai_reviewed <- FALSE
  session$setInputs(advance = 1)
  stopifnot(s$phase == "production_r")
  session$setInputs(
    r_output_ai_review_note = "Reviewed sample-based output suggestion.",
    r_output_ai_review_attest = TRUE,
    save_r_output_ai_review = 1
  )
  stopifnot(s$r_output_ai_reviewed)
  session$setInputs(advance = 2)
  stopifnot(s$phase == "trace_r")
})
