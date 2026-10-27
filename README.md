# ADaM QC Workbench

A standard Shiny prototype for bounded, human-in-the-loop independent QC of ADaM datasets during SAS-to-R migration. You upload a SAS reference dataset and an R QC dataset (plus the programs, for review), run a deterministic comparison, disposition each finding, and approve the outcome with a session audit log.

> **Prototype limitations:** not a validated system and not for regulatory submission. No AI/LLM, SAS runner, external service, database, or credential is used. Identity is a self-declared name, the audit log lives only in the session, and approvals are not electronic signatures. Upload only data you are authorized to handle on the machine running the app.

## Workflow

`intake → configured → compared → dispositioned → approved`

The state machine in `R/workflow.R` is enforced by `transition_workflow()`. Any change to an input (new upload, re-run settings) discards results, dispositions, and approval and returns the workflow to an earlier state; the invalidation is recorded in the audit log.

| Tab | What happens |
|---|---|
| 1 Intake | Enter reviewer name; upload reference and QC datasets (`.xpt`, `.sas7bdat`, `.csv`) and optional SAS / R programs; confirm intake. **Load synthetic demo** loads a small generated example. |
| 2 Code review | Programs shown read-only; record a checklist-based review. |
| 3 Run & compare | Choose key variables and numeric tolerance; run the comparator. |
| 4 Findings | Each finding shows evidence, an optional rule-based hint, and a human disposition form. |
| 5 Approval | Readiness checks, approval statement, attestation; download findings and audit CSVs. |
| 6 Audit | Append-only session event log. |

## Evidence labels

- **Deterministic comparator result:** structure, record, and value differences computed by `R/compare.R`.
- **Rule-based hint (not AI):** fixed pattern checks over differing records (for example, "ADT is missing in the reference for 100% of differing records"). Hints describe patterns; they do not establish root cause.
- **Human disposition:** decisions and rationale entered by the reviewer.

## Comparison rules

- Records are matched on the selected key variables (or by row order if none). Duplicate keys are reported; only the first record per key is compared.
- Column names are upper-cased. Character values are trimmed, and blank equals missing. Numeric values compare within an absolute tolerance (default `1e-8`). Dates compare as numbers; numeric-vs-character columns are compared numerically when every value converts.
- Findings cover variables on one side only, type mismatches, duplicate keys, records on one side only, and per-variable value differences.

## Requirements

- R 4.1 or later
- Packages `shiny` and `haven`

```r
install.packages(c("shiny", "haven"))
```

## Launch

Start the app from the project directory (the `www/` styles are served only when the app directory is used):

```r
shiny::runApp("path/to/adam-qc-workbench")
```

In Positron or RStudio you can also open `app.R` and use **Run App**. Uploads up to 200 MB are accepted.

## Optional local R program execution

Disabled by default: uploaded programs are displayed only, and SAS programs are never executed. To let the app run an uploaded **R** program, set the environment variable before launching:

```r
Sys.setenv(ADAM_QC_ALLOW_R_EXEC = "true")
shiny::runApp("path/to/adam-qc-workbench")
```

The program runs in a separate local R process (120 s limit) with your own permissions. **This is not a sandbox**; run only programs you trust. It must write `qc_output.csv` or `qc_output.xpt` in its working directory; optional input files are copied there. The output becomes the QC dataset.

## Least-privilege design notes

- Upload types and sizes are restricted; `.rds` and other serialized R objects are not accepted.
- Uploaded values are rendered as escaped text only, never as HTML.
- No network calls, persistence, credentials, or external services. Files stay in temporary session storage.
- The audit log records actions, file names, row counts, and checksums, not data values (dispositions and rationales are recorded as entered).
- A production deployment would need authenticated identity and role-based authorization, isolated execution, governed storage, tamper-evident audit retention, and validation.

## Project structure

- `app.R`: entry point
- `R/workflow.R`: state machine
- `R/compare.R`, `R/findings.R`: deterministic comparator and finding builder
- `R/io.R`: validated dataset and program readers
- `R/exec.R`: optional local R execution
- `R/audit.R`, `R/utils.R`, `R/data.R` (synthetic demo)
- `R/mod_*.R`: Shiny modules for each tab; `R/ui.R`, `R/server.R`
- `www/styles.css`: styling
- `tests/`: `test_workflow.R`, `test_compare.R`, `test_server.R`

## Tests

From the project directory:

```r
for (f in list.files("tests", full.names = TRUE)) source(f)
```

`test_server.R` walks the full workflow headlessly with `shiny::testServer()`; no app is launched.
