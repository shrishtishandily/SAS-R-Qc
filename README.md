# ADaM QC Workbench

A human-gated Shiny prototype for a sequential ADaM workflow: register study documents and source SDTM, approve a requirements baseline, register production R code and its externally generated output, trace the R output, review an independent SAS QC draft, register its externally generated output, trace the SAS result, deterministically compare both datasets, review differences, record external CDISC validation review, and sign off a draft report.

See [ARCHITECTURE_BLUEPRINT.md](ARCHITECTURE_BLUEPRINT.md) for the target architecture, separation-of-duties model, requirements traceability, approval gates, comparison policy, reporting, and validation roadmap.

> **Prototype limitations:** This is not a validated system and not for clinical, regulatory, or production use. It does not execute uploaded R or SAS programs, launch SAS, or connect to Pinnacle 21 / CDISC CORE. Program execution and standards validation happen outside the prototype. Its UI hides R source during SAS review but cannot enforce technical blinding because this is one local Shiny process. Reviewer names are unauthenticated, approvals are not electronic signatures, uploaded files and audit records are session-local, and the audit log is not durable or tamper-evident. Use synthetic data only in this prototype.

## Workflow

The enforced forward sequence is:

`Study inputs → Requirements baseline → Production R → R traceability → Independent SAS QC review → SAS QC output → SAS traceability → Deterministic comparison → Difference review → CDISC review → Report and sign-off`

Each next step is gated on required human approvals. The SAS output screen is reachable only after a reviewer approves the exact SAS source checksum. Registering output requires a confirmation that its executed-code checksum matches. The final draft sign-off is blocked until each prior gate and each comparison finding has a human disposition. Changes to program uploads reset their approvals and dependent output.

| Phase | Prototype behavior |
|---|---|
| Study inputs | Register protocol, SAP, ADaM specs, Define-XML/metadata, other documents, source SDTM domains, and supporting ADSL/other ADaM datasets; enter document type/version and task/reviewer. Dataset/document pickers accept multiple files and later upload batches accumulate; same-name files replace the prior entry. The generated target ADaM output is uploaded separately in the Production R phase. |
| Requirements baseline | Extract draft candidates from registered Define-XML, PDFs, DOCX, text, CSV, and Excel files; review exact source citations/evidence, edit the candidate rules, resolve conflicts, and obtain human approval. Extraction does not determine correctness or approve a baseline. |
| Production R | Register and review R source/checksum; approve the exact code; run it externally; register the output and run-manifest reference. |
| R traceability | Manually link each requirement to document/source, R program line, output evidence, and coverage; reviewer approves the trace. |
| Independent SAS QC | Upload an independent draft; review code and approve its checksum. This single Shiny process does not technically enforce blinding. |
| SAS QC output | Run the approved SAS program externally and register output, SAS version/log reference, and executed checksum. |
| SAS traceability | Manually link requirements to SAS lines and output evidence; reviewer approves. |
| Deterministic comparison | Choose key variables and numeric tolerance; approve the comparison profile; compare output datasets using the prototype comparator. Source programs are not passed to the comparator. |
| Difference review | Inspect deterministic evidence and rule-based hints; record human review/disposition for every finding. Findings without disposition block sign-off. |
| CDISC review | Upload external validation report(s), identify validator/ruleset/standards versions, and record human review. Validation itself is not run by the prototype. |
| Report/sign-off | Review the consolidated CSV and audit CSV, enter a conclusion, and attest. Sign-off is session-local and not a validated signature. |

## Provenance labels and interpretation

- **Deterministic comparator result:** calculated from the two registered output datasets by `R/compare.R`.
- **Rule-based hint (not AI):** a fixed pattern check, not a model conclusion or root cause.
- **Human disposition / approval:** the reviewer’s recorded decision, rationale, and prototype attestation.
- **CDISC validation:** results from the uploaded external validator report; independent of code-to-spec traceability and output comparison.

A matching result does not prove that either implementation meets study intent. A CDISC validator result does not prove program correctness. Any unresolved, uncited, or unverified requirement must remain visible.

## Comparator behavior

The prototype uses its own deterministic R implementation rather than `diffdf` or SAS `PROC COMPARE`. It compares common variables after matching records on selected keys, checks one-sided variables/records, duplicate keys, type differences, missingness, and value differences. Key variables are required in the staged workflow; row-order matching is disabled there. Numeric comparison uses an absolute tolerance (default `1e-8`). Character trimming, date conversion, and type-conversion behavior are implemented in `R/compare.R`. Review and validate these semantics for the intended standards before relying on them; this prototype comparator is not formally validated.

## Copilot Synthetic Test

The optional Copilot panel sends manually entered text to GitHub Copilot's hosted service. Use entirely synthetic text only; never paste study documents, real records, source code, logs, or credentials. The checkbox is an operator attestation, not a data-loss-prevention control. Nothing is sent on startup. A connection check requires the checkbox; generation additionally requires a model returned by that check. Responses are drafts and do not alter QC state or approvals.

The existing Ollama implementation is retained for compatibility but its activation and connection controls are disabled. The existing workflow AI buttons are not connected to Copilot; requirements extraction, code review, SAS generation, and discrepancy investigation through Copilot are future phases. No fallback provider is configured. The SDK runs in `empty` mode with an explicit empty tool list, without access to uploaded app artifacts. Its runtime may still keep session state according to its own behavior; consult GitHub's data handling policies. This is not an isolation boundary suitable for real study data.

GitHub documents the Copilot SDK for embedding in standalone applications with a Copilot subscription and authenticated CLI credentials. The Python SDK requires Python 3.11+ and a compatible runtime; model availability depends on the signed-in account. SDK usage consumes Copilot allowance and may incur extra charges depending on plan and budget. Check your [billing and budget](https://docs.github.com/en/copilot/concepts/billing-and-usage/individuals/billing), including whether your account uses legacy request-based billing. The model list does not guarantee access or available credits. A successful account login in Positron does not necessarily mean the separate runtime is authenticated.

Install Python 3.11+ and, from the project directory, run in PowerShell:

```powershell
python -m venv .venv
.\.venv\Scripts\python.exe -m pip install -r backend/requirements.txt
.\.venv\Scripts\python.exe -m copilot download-runtime
```

Sign in using the official GitHub Copilot CLI (`copilot login`) if not already authenticated. Credentials stay in the CLI's credential storage, not app source or the browser. Review the [CLI authentication documentation](https://docs.github.com/en/copilot/reference/copilot-cli-reference/cli-command-reference) and [SDK authentication documentation](https://github.com/github/copilot-sdk/blob/main/docs/auth/authenticate.md). Set the interpreter in the R session before launching the app:

```r
Sys.setenv(ADAM_QC_PYTHON = normalizePath(".venv/Scripts/python.exe"))
shiny::runApp()
```

The Python gateway is started on demand as a local child process; no separate backend server or open listening port is needed. The SDK runtime download is an explicit setup step. If custom Copilot provider configuration is detected, the gateway fails closed instead of silently switching providers. This phase has mocked tests, not a verified live model response.

## Requirements and launch

- R 4.1 or later
- `shiny`, `haven`, `xml2`, `curl`, and `jsonlite`; `pdftools` and `readxl` enable PDF and Excel extraction

```r
install.packages(c("shiny", "haven", "xml2", "curl", "jsonlite", "pdftools", "readxl"))
shiny::runApp("path/to/adam-qc-workbench")
```

Launch from the project directory so `www/` styles resolve. The app is not automatically launched during setup.

## Data handling and audit limits

The requirements phase extracts draft candidates from Define-XML dataset/variable metadata and relevant text passages from PDF, DOCX, TXT, CSV, and Excel files. Candidates include document/version and a Define-XML object, PDF page, DOCX paragraph, or spreadsheet-sheet citation where available. Define-XML is scoped to the ADaM dataset named in the task when it can be identified. PDF extraction is text-based and may miss scanned pages or table structure; no OCR or semantic interpretation is performed. Extraction does not establish correctness or resolve conflicts. Reviewers must inspect the evidence, edit the candidate rules, address conflicts, and attest before the baseline can be approved. The app accepts supported tabular formats as restricted by `R/io.R`. It records file names/checksums and comparison evidence; it does not persist uploads or task state across sessions. The event log is only a session log. A production deployment requires authenticated roles, technical branch isolation, controlled execution, governed durable storage, integrity-protected audit records, validated signing, security review, formal system validation, and applicable SOPs.

## Tests

Run from the project directory:

```r
source("tests/test_workflow.R")
source("tests/test_compare.R")
source("tests/test_server.R")
source("tests/test_pipeline.R")
source("tests/test_extraction.R")
source("tests/test_local_llm.R")
source("tests/test_copilot_panel.R")
```

Run the Python gateway tests with `.\.venv\Scripts\python.exe -m unittest discover -s tests -p test_copilot_gateway.py`. The tests use synthetic fixtures and mocked model calls; they do not launch the app, contact Copilot, or run uploaded programs.
