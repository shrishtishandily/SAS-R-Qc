# ADaM QC Workbench

A human-gated Shiny prototype for a sequential ADaM workflow: register study documents and source SDTM, approve a requirements baseline, register production R code and its externally generated output, trace the R output, review an independent SAS QC draft, register its externally generated output, trace the SAS result, deterministically compare both datasets, review differences, record external CDISC validation review, and sign off a draft report.

See [ARCHITECTURE_BLUEPRINT.md](ARCHITECTURE_BLUEPRINT.md) for the target architecture, separation-of-duties model, requirements traceability, approval gates, comparison policy, reporting, and validation roadmap.

> **Prototype limitations:** This is not a validated system and not for clinical, regulatory, or production use. It does not interpret document contents, generate R or SAS code, execute uploaded R or SAS programs, launch SAS, or connect to Pinnacle 21 / CDISC CORE. Program execution and standards validation must happen outside the prototype. Its UI hides R source during SAS review but cannot enforce technical blinding because this is one local Shiny process. Reviewer names are unauthenticated, approvals are not electronic signatures, uploaded files and audit records are session-local, and the audit log is not durable or tamper-evident. Use synthetic data only in this prototype.

## Workflow

The enforced forward sequence is:

`Study inputs → Requirements baseline → Production R → R traceability → Independent SAS QC review → SAS QC output → SAS traceability → Deterministic comparison → Difference review → CDISC review → Report and sign-off`

Each next step is gated on required human approvals. The SAS output screen is reachable only after a reviewer approves the exact SAS source checksum. Registering output requires a confirmation that its executed-code checksum matches. The final draft sign-off is blocked until each prior gate and each comparison finding has a human disposition. Changes to program uploads reset their approvals and dependent output.

| Phase | Prototype behavior |
|---|---|
| Study inputs | Register protocol, SAP, ADaM specs, Define-XML/metadata, other documents, and SDTM datasets; enter document type/version and task/reviewer. Document contents are not extracted. |
| Requirements baseline | Manually enter source-cited requirements, conflict status, resolution, and human approval. |
| Production R | Register and review R source/checksum; approve it; run it externally; register the output and run-manifest reference. |
| R traceability | Manually link each requirement to document/source, R program line, output evidence, and coverage; reviewer approves the trace. |
| Independent SAS QC | Upload independently authored SAS draft; review code, citations, and assumptions; approve its checksum. SAS generation is not implemented. |
| SAS QC output | Run the approved SAS program externally and register output, SAS version/log reference, and executed checksum. |
| SAS traceability | Manually link requirements to SAS lines and output evidence; reviewer approves. |
| Deterministic comparison | Choose key variables and numeric tolerance; approve the comparison profile; compare output datasets using the prototype comparator. Source programs are not passed to the comparator. |
| Difference review | Inspect exact finding evidence and rule-based hints; record possible cause, source references, rationale, and human verified/rejected/needs-investigation disposition. Findings without a final disposition block sign-off. |
| CDISC review | Upload external validation report(s), identify validator/ruleset/standards versions, and record human review. Validation itself is not run by the prototype. |
| Report/sign-off | Review a consolidated CSV report and audit CSV; enter a conclusion and attest. Sign-off is session-local and not a validated signature. |

## Provenance labels and interpretation

- **Deterministic comparator result:** calculated from the two registered output datasets by `R/compare.R`.
- **Rule-based hint (not AI):** a fixed pattern check, not a model conclusion or root cause.
- **Human disposition / approval:** the reviewer’s recorded decision, rationale, and prototype attestation.
- **CDISC validation:** results from the uploaded external validator report; independent of code-to-spec traceability and output comparison.

A matching result does not prove that either implementation meets study intent. A CDISC validator result does not prove program correctness. Any unresolved, uncited, or unverified requirement must remain visible.

## Comparator behavior

The prototype uses its own deterministic R implementation rather than `diffdf` or SAS `PROC COMPARE`. It compares common variables after matching records on selected keys, checks one-sided variables/records, duplicate keys, type differences, missingness, and value differences. Key variables are required in the staged workflow; row-order matching is disabled there. Numeric comparison uses an absolute tolerance (default `1e-8`). Character trimming, date conversion, and type-conversion behavior are implemented in `R/compare.R`. Review and validate these semantics for the intended standards before relying on them; this prototype comparator is not formally validated.

## Requirements and launch

- R 4.1 or later
- `shiny` and `haven`

```r
install.packages(c("shiny", "haven"))
shiny::runApp("path/to/adam-qc-workbench")
```

Launch from the project directory so `www/` styles resolve. The app is not automatically launched during setup.

## Data handling and audit limits

The app accepts supported tabular formats as restricted by `R/io.R`. It records file names/checksums and comparison evidence; it does not persist uploads or task state across sessions. The event log is only a session log. A production deployment requires authenticated roles, technical branch isolation, controlled execution, governed durable storage, integrity-protected audit records, validated signing, security review, formal system validation, and applicable SOPs.

## Tests

Run from the project directory:

```r
source("tests/test_workflow.R")
source("tests/test_compare.R")
source("tests/test_server.R")
source("tests/test_pipeline.R")
```

The tests use synthetic fixtures and Shiny's headless `testServer()`; they do not launch the app or run uploaded programs.
