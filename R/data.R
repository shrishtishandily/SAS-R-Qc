DEMO_SAS_CODE <- paste(
  "/* Synthetic demo program (display only; never executed) */",
  "data work.advs_ref;",
  "  set work.advs_src;",
  "  by usubjid paramcd;",
  "  retain base;",
  "  if avisitn = 0 then base = aval;",
  "run;",
  sep = "\n"
)

DEMO_R_CODE <- paste(
  "# Synthetic demo program (display only unless local R execution is enabled)",
  "src <- read.csv('advs_src.csv')",
  "src$BASE <- ifelse(is.na(src$ADT), 55, src$AVAL[src$AVISITN == 0])",
  "write.csv(src, 'qc_output.csv', row.names = FALSE)",
  sep = "\n"
)

# Fully synthetic records with one seeded mismatch: BASE where ADT is missing.
demo_datasets <- function() {
  ref <- data.frame(
    USUBJID = rep(sprintf("SYN-%03d", 1:6), each = 2),
    PARAMCD = "WT",
    AVISITN = rep(c(0, 1), 6),
    ADT = as.Date("2024-01-08") + rep(c(0, 7), 6),
    AVAL = c(72, 71, 68, 67, 66, 65, 75, 74, 70, 69, 64, 63),
    BASE = rep(c(72, 68, 66, 75, 70, 64), each = 2),
    stringsAsFactors = FALSE
  )
  ref$ADT[5] <- as.Date(NA)
  ref$BASE[5] <- 66
  qc <- ref
  qc$BASE[5] <- 55
  list(ref = ref, qc = qc, keys = c("USUBJID", "PARAMCD", "AVISITN"), sas_code = DEMO_SAS_CODE, r_code = DEMO_R_CODE)
}
