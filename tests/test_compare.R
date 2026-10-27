for (f in c("utils", "workflow", "audit", "io", "compare", "findings", "exec", "data")) {
  source(file.path("R", paste0(f, ".R")))
}

demo <- demo_datasets()
ref <- normalize_dataset(demo$ref)
qc <- normalize_dataset(demo$qc)

# Seeded case: exactly one BASE mismatch, reference 66 vs QC 55, where ADT is missing.
cmp <- compare_datasets(ref, qc, demo$keys)
stopifnot(!cmp$summary$identical, cmp$summary$total_diffs == 1L, cmp$summary$n_matched == 12L)
stopifnot(identical(cmp$diffs$variable, "BASE"), cmp$diffs$ref_value == "66", cmp$diffs$qc_value == "55")
stopifnot(grepl("ADT is missing", cmp$hints$BASE, fixed = TRUE))

findings <- build_findings(cmp)
stopifnot(length(findings) == 1L, findings[[1]]$id == "F-001", findings[[1]]$severity == "High")

# Identical inputs produce no findings.
same <- compare_datasets(ref, ref, demo$keys)
stopifnot(same$summary$identical, length(build_findings(same)) == 0L)

# Structure, row, duplicate, tolerance, and case handling.
a <- data.frame(ID = c("1", "2", "3"), X = c(1, 2, 3), Y = c("a", "B", "c"), Z = 1:3)
b <- data.frame(ID = c("1", "2", "4", "4"), X = c(1, 2.0000000001, 3, 3), Y = c("A", "B ", "c", "c"), W = 1:4)
res <- compare_datasets(normalize_dataset(a), normalize_dataset(b), "ID")
stopifnot(res$structure$only_ref == "Z", res$structure$only_qc == "W")
stopifnot(identical(res$rows$only_ref, "3"), identical(res$rows$only_qc, "4"), identical(res$duplicates$qc, "4"))
stopifnot(res$var_summary$n_diff[res$var_summary$variable == "X"] == 0L)
stopifnot(res$var_summary$all_case_only[res$var_summary$variable == "Y"])

# Unknown key variable is rejected.
stopifnot(inherits(try(compare_datasets(ref, qc, "NOPE"), silent = TRUE), "try-error"))

# Dataset round trip through CSV keeps the comparison deterministic.
tmp <- tempfile(fileext = ".csv")
utils::write.csv(demo$qc, tmp, row.names = FALSE)
qc_csv <- read_dataset(tmp, "qc.csv")
stopifnot(compare_datasets(ref, qc_csv, demo$keys)$summary$total_diffs == 1L)
unlink(tmp)

# Unsupported extensions are rejected.
stopifnot(inherits(try(read_dataset(tmp, "evil.exe"), silent = TRUE), "try-error"))

# XPT round trip (haven) keeps dates and numbers comparable.
tmp_xpt <- tempfile(fileext = ".xpt")
haven::write_xpt(demo$ref, tmp_xpt)
ref_xpt <- read_dataset(tmp_xpt, "ref.xpt")
stopifnot(compare_datasets(ref_xpt, ref, demo$keys)$summary$identical)
unlink(tmp_xpt)
