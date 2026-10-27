new_finding <- function(type, title, severity, summary, evidence, n, hint = NA_character_, variable = NA_character_) {
  list(
    id = NA_character_, type = type, title = title, severity = severity, summary = summary,
    evidence = evidence, n = n, hint = hint, variable = variable
  )
}

build_findings <- function(cmp, max_evidence = 100L) {
  out <- list()
  add <- function(f) out[[length(out) + 1L]] <<- f

  s <- cmp$structure
  if (length(s$only_ref)) {
    add(new_finding("structure", "Variables only in reference dataset", "High",
      sprintf("%d variable(s) exist in the reference but not in the QC dataset.", length(s$only_ref)),
      data.frame(variable = s$only_ref), length(s$only_ref)))
  }
  if (length(s$only_qc)) {
    add(new_finding("structure", "Variables only in QC dataset", "High",
      sprintf("%d variable(s) exist in the QC dataset but not in the reference.", length(s$only_qc)),
      data.frame(variable = s$only_qc), length(s$only_qc)))
  }
  if (nrow(s$type_mismatch)) {
    add(new_finding("structure", "Variable type mismatch", "Medium",
      sprintf("%d common variable(s) differ in type (numeric / character / date).", nrow(s$type_mismatch)),
      s$type_mismatch, nrow(s$type_mismatch)))
  }

  d <- cmp$duplicates
  if (length(d$ref) || length(d$qc)) {
    ev <- rbind(
      if (length(d$ref)) data.frame(dataset = "reference", key = d$ref),
      if (length(d$qc)) data.frame(dataset = "qc", key = d$qc)
    )
    add(new_finding("keys", "Duplicate key values", "High",
      "Key values are not unique. Only the first record per key was compared.", ev, nrow(ev)))
  }

  r <- cmp$rows
  if (length(r$only_ref)) {
    add(new_finding("rows", "Records only in reference dataset", "High",
      sprintf("%d key value(s) are present in the reference but not in the QC dataset.", length(r$only_ref)),
      data.frame(key = r$only_ref), length(r$only_ref)))
  }
  if (length(r$only_qc)) {
    add(new_finding("rows", "Records only in QC dataset", "High",
      sprintf("%d key value(s) are present in the QC dataset but not in the reference.", length(r$only_qc)),
      data.frame(key = r$only_qc), length(r$only_qc)))
  }

  vs <- cmp$var_summary
  if (!is.null(vs)) {
    for (i in which(vs$n_diff > 0)) {
      v <- vs$variable[i]
      ev <- cmp$diffs[cmp$diffs$variable == v, c("key", "ref_value", "qc_value", "category", "abs_diff"), drop = FALSE]
      add(new_finding("values", sprintf("Value differences in %s", v),
        if (vs$all_case_only[i]) "Low" else "High",
        sprintf("%d of %d matched records differ (missing in QC: %d, missing in reference: %d, value mismatch: %d).",
                vs$n_diff[i], vs$n_compared[i], vs$n_missing_in_qc[i], vs$n_missing_in_ref[i], vs$n_value_mismatch[i]),
        ev, vs$n_diff[i], hint = cmp$hints[[v]] %||% NA_character_, variable = v))
    }
  }

  for (i in seq_along(out)) out[[i]]$id <- sprintf("F-%03d", i)
  out
}

DISPOSITION_CHOICES <- c(
  "Confirmed discrepancy: correction required",
  "Explained: acceptable difference",
  "Reference (SAS) issue",
  "Needs further investigation"
)

findings_export <- function(findings, dispositions) {
  if (!length(findings)) {
    return(data.frame(finding_id = character(), type = character(), variable = character(), severity = character(),
                      n = integer(), decision = character(), rationale = character(), reviewer = character(), time = character()))
  }
  do.call(rbind, lapply(findings, function(f) {
    d <- dispositions[[f$id]]
    data.frame(
      finding_id = f$id, type = f$type, variable = f$variable %||% NA_character_, severity = f$severity, n = f$n,
      decision = d$decision %||% "Pending", rationale = d$rationale %||% "",
      reviewer = d$reviewer %||% "", time = d$time %||% "", stringsAsFactors = FALSE
    )
  }))
}
