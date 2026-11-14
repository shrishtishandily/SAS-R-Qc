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
| Production R | Register and review R source/checksum; optionally request a local-LLM requirement review; human-review AI findings; approve the exact code; run it externally; register the output and run-manifest reference. |
| R traceability | Manually link each requirement to document/source, R program line, output evidence, and coverage; reviewer approves the trace. |
| Independent SAS QC | Optionally draft SAS from only approved requirements and SDTM schema with local AI, or upload an independent draft; review code, citations, AI review findings, and assumptions; approve its checksum. The SAS prompt does not include production R source. This single Shiny process does not technically enforce blinding. |
| SAS QC output | Run the approved SAS program externally and register output, SAS version/log reference, and executed checksum; optionally request a local-LLM sample review of schema and first five rows against requirements. |
| SAS traceability | Manually link requirements to SAS lines and output evidence; reviewer approves. |
| Deterministic comparison | Choose key variables and numeric tolerance; approve the comparison profile; compare output datasets using the prototype comparator. Source programs are not passed to the comparator. |
| Difference review | Inspect deterministic evidence and rule-based hints; optionally ask local AI for evidence-linked hypotheses; record human review/disposition for every finding and AI hypothesis. Findings without disposition block sign-off. |
| CDISC review | Upload external validation report(s), identify validator/ruleset/standards versions, and record human review. Validation itself is not run by the prototype. |
| Report/sign-off | Optionally draft a local-LLM summary citing report row IDs; verify/reject the summary, then review the consolidated CSV and audit CSV, enter a conclusion, and attest. Sign-off is session-local and not a validated signature. |

## Provenance labels and interpretation

- **Deterministic comparator result:** calculated from the two registered output datasets by `R/compare.R`.
- **Rule-based hint (not AI):** a fixed pattern check, not a model conclusion or root cause.
- **Human disposition / approval:** the reviewer’s recorded decision, rationale, and prototype attestation.
- **CDISC validation:** results from the uploaded external validator report; independent of code-to-spec traceability and output comparison.

A matching result does not prove that either implementation meets study intent. A CDISC validator result does not prove program correctness. Any unresolved, uncited, or unverified requirement must remain visible.

## Comparator behavior

The prototype uses its own deterministic R implementation rather than `diffdf` or SAS `PROC COMPARE`. It compares common variables after matching records on selected keys, checks one-sided variables/records, duplicate keys, type differences, missingness, and value differences. Key variables are required in the staged workflow; row-order matching is disabled there. Numeric comparison uses an absolute tolerance (default `1e-8`). Character trimming, date conversion, and type-conversion behavior are implemented in `R/compare.R`. Review and validate these semantics for the intended standards before relying on them; this prototype comparator is not formally validated.

## Optional Local AI

No hosted model is used. AI actions are disabled until the reviewer explicitly checks a local Ollama service, selects an installed model, and attests that the model and selected content are approved for the session. The adapter only accepts a loopback URL (`localhost`, `127.0.0.1`, or `::1`) and does not follow redirects. If Ollama is absent/unavailable, all deterministic extraction and QC workflow functions remain usable; AI actions report that the local service is unavailable.

To enable local AI, install and start Ollama separately, download a model appropriate for the computer, then restart this app and use **Check local Ollama models** → select the model → attest → **Enable local AI for this session**. No Ollama installation, model download, or external request is performed by this project. The endpoint can be configured with `ADAM_QC_LOCAL_LLM_URL`, but it is rejected unless it resolves textually to loopback. `ADAM_QC_LOCAL_LLM_MODEL` can select an installed model as the initial choice.

Local AI tasks are limited to:

- Organizing extracted document/Define-XML candidates into proposed rules, citing only supplied extraction IDs and preserving the source passages.
- Reviewing uploaded production R code against the approved baseline; the application proposes findings with bounded line references.
- Drafting an independent SAS QC program from approved requirements and SDTM column/type schemas. The SAS-generation request does not include production R source or R output. The result is a draft and must be reviewed before external SAS execution.
- Reviewing the SAS draft against the baseline, and reviewing the R/SAS output schema plus only the first five data rows against the baseline.
- Suggesting possible causes for deterministic differences and drafting an evidence-linked report summary.

All model content is untrusted. Source document/program instructions embedded in text are treated as evidence, not followed as instructions. Responses must cite supplied requirement/finding IDs; code line and sample-row references are range checked. Invalid IDs are dropped. Prompts have explicit payload caps for code, requirements, and report rows. Every output is labeled as a local-model proposal; any generated SAS code, sample-based dataset finding, discrepancy hypothesis, or report summary requires a reviewer rationale/attestation before workflow sign-off. AI never changes a source document/program, executes code, performs the deterministic comparison, or approves a result.

This integration has not been run against an installed model on this machine: Ollama was not found and its local endpoint was unavailable during implementation. Adapter behavior is covered with mocked responses; install/configure Ollama locally before evaluating real model behavior. Keep the prototype on synthetic data. A local endpoint is not automatically secure or validated, and this Shiny process does not technically enforce SAS/R blinding.

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
```

The tests use synthetic fixtures and Shiny's headless `testServer()`; they do not launch the app or run uploaded programs.
