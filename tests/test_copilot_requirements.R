source("app.R")

calls <- list()
copilot_bridge <- function(request) {
  calls[[length(calls) + 1L]] <<- request
  list(
    text = jsonlite::toJSON(
      list(
        requirements = list(
          list(
            target = "ADAE.ASTDT",
            proposed_rule = "Use AESTDTC",
            evidence_ids = list("REQ-001", "INVALID"),
            confidence = "low",
            open_question = "Check partial dates"
          ),
          list(
            target = "ADAE.AESEQ",
            proposed_rule = "Invented",
            evidence_ids = list("INVALID"),
            confidence = "high",
            open_question = ""
          )
        )
      ),
      auto_unbox = TRUE
    )
  )
}

shiny::testServer(pipeline_server, {
  s$requirement_candidates <- data.frame(
    requirement_id = "REQ-001",
    target = "ADAE.ASTDT",
    source_ref = "Synthetic SAP v1; page 2",
    rule = "Draft",
    source_file = "synthetic.txt",
    evidence = "Use AESTDTC",
    extraction_type = "Text passage",
    stringsAsFactors = FALSE
  )
  session$setInputs(copilot_model = "test-model", ai_requirements = 1)
  stopifnot(length(calls) == 0L)
  session$setInputs(copilot_requirements_confirm = TRUE, ai_requirements = 2)
  stopifnot(length(calls) == 0L)
  copilot_state$models <- data.frame(id = "test-model", name = "Test model")
  session$setInputs(ai_requirements = 3)
  stopifnot(length(calls) == 1L, calls[[1]]$synthetic_confirmed)
  stopifnot(
    !is.null(calls[[1]]$schema),
    nrow(s$ai_requirement_suggestions) == 1L
  )
  stopifnot(
    s$ai_requirement_suggestions$source_ref == "Synthetic SAP v1; page 2"
  )
  stopifnot(grepl(
    "GitHub Copilot test-model",
    s$ai_requirement_suggestions$extraction_type,
    fixed = TRUE
  ))
  stopifnot(!s$ai_requirement_suggestions_reviewed)
  s$requirements_approved <- TRUE
  session$setInputs(ai_requirements = 4)
  stopifnot(length(calls) == 1L)
})
