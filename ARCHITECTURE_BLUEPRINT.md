# ADaM QC Workbench

## Architecture Blueprint for Review

**Status: Draft for review — architecture proposal, not a validated-system specification.** This blueprint consolidates the agreed product concept and recommendations. It does not claim that generated programs, datasets, or reports are correct or validated. Real study data, production execution, SAS execution, AI services, and regulatory use require separate authorization, security, validation, and operational design.

## 1. Purpose and design goals

The ADaM QC Workbench is intended to support an auditable, human-controlled workflow that produces a production ADaM dataset in R, independently produces a QC ADaM dataset in SAS from approved study requirements, evaluates each program against the same controlled requirements baseline, and deterministically compares the outputs.

The central design goals are:

- **Independent implementations:** the SAS QC generation and execution path must not use the production R source code or its derivation logic.
- **Shared approved target:** both implementations are evaluated against the same versioned, human-approved study requirements.
- **Evidence before conclusions:** each result and hypothesis links to underlying data, code, document passages, validation rules, and run metadata where available.
- **Human control:** no generated code executes and no source document or program changes without explicit human review and approval.
- **Reproducibility:** record immutable input identities, versions, checksums, settings, tools, runs, approvals, and outputs.
- **Honest claims:** standards validation, specification traceability, output agreement, and system validation are separate outcomes.

## 2. Scope and boundaries

### In scope

- Intake of study documents, SDTM datasets, ADaM specifications and metadata, programs, and generated outputs.
- Versioning and approval of the requirements baseline.
- Production R execution and independent SAS QC authoring/review/execution as separately controlled workstreams.
- Source-to-ADaM and program-to-requirement traceability checks for both R and SAS.
- CDISC conformance validation of suitable datasets and metadata using configured external or local validators.
- Deterministic comparison of R- and SAS-derived ADaM datasets.
- Evidence-linked difference triage, human disposition, approval, report generation, and audit export.

### Not claimed by the application alone

- That static code analysis proves a derivation is correct.
- That equal R/SAS outputs prove conformance to study intent.
- That a validator pass proves source program correctness or overall data fitness.
- That an AI-generated explanation is a verified root cause.
- That a typed reviewer name, session log, or prototype approval is a validated electronic signature or compliant audit trail.
- That the prototype is validated software or suitable for regulated production use.

## 3. Actors and separation of duties

| Actor | Responsibilities |
|---|---|
| Study document owner | Supplies controlled protocol, SAP, ADaM specifications, metadata/Define-XML, controlled terminology, and version information. |
| Requirements reviewer | Resolves conflicts and ambiguities; approves the requirements baseline before code assessment. |
| Production R programmer | Supplies and runs the production R program with selected SDTM data in the R execution boundary. |
| Independent QC programmer/reviewer | Reviews independently generated SAS QC code; approves its exact version before SAS execution. Must not receive production R source or derivation logic. |
| QC reviewer/approver | Verifies findings, evidence, suggestions, dispositions, and final report; signs off under the organization’s controlled process. |
| Administrator/operations | Configures authorized runtimes, validators, access policies, storage, retention, and audit controls. |

A single person may hold multiple roles only where the organization’s SOPs permit it; the system should record role and action rather than assume separation.

## 4. Conceptual architecture

The workbench should be composed of isolated services or execution contexts, even if a prototype initially models them as separate modules:

1. **Study workspace and controlled document registry** — accepts immutable inputs; calculates checksums; captures name, version, effective date, and source. No source file is overwritten.
2. **Requirements baseline service** — extracts/records requirements and source citations; supports conflict resolution, reviewer comments, approval, and version freeze.
3. **Production R lane** — receives SDTM inputs, approved requirements, and production R code. It runs only in an authorized R execution environment and stores outputs/logs as a new run version.
4. **Independent SAS QC lane** — receives only approved study requirements, permitted source SDTM, metadata, and relevant approved references. It must not be able to access production R code, R logs containing implementation details, or intermediate R outputs while generating QC logic. Generated SAS remains a draft until reviewed and approved. Only approved code may be run in the controlled SAS environment.
5. **Traceability and conformance services** — check each R and SAS implementation/output against the baseline. Validators such as Pinnacle 21 or CDISC CORE may be evaluated for configured CDISC checks; record product, version, rule set, standards, and configuration.
6. **Deterministic comparator** — receives only the completed R ADaM and SAS QC ADaM outputs plus approved comparison settings. It cannot inspect either source program.
7. **Difference triage and remediation proposal** — uses finding evidence and linked requirements/source evidence to suggest possible categories or corrections. Suggestions are explicitly proposed, not conclusions or code changes.
8. **Human review and reporting** — records verification, disposition, rationale, approval/sign-off, and produces a versioned draft/final review package according to workflow status.
9. **Audit and artifact registry** — durable, access-controlled, integrity-protected records in a production implementation. The prototype’s in-memory audit is not sufficient for regulated use.

### Information-flow boundary

| Workstream | May access | Must not access |
|---|---|---|
| Production R | Production R source, SDTM, approved baseline, R runtime | QC SAS source while drafting; unrelated study workspaces |
| SAS QC generation | Approved requirements/documents, permitted SDTM, SAS templates/tooling | Production R source, R derivation logic, R run logs that reveal implementation |
| SAS QC execution | Human-approved SAS code version, permitted inputs, SAS runtime | Unapproved edited code; production R source |
| Comparator | R ADaM output, SAS QC ADaM output, approved comparison profile | R/SAS source programs and hidden implementation details |
| Post-comparison review | Comparator evidence, approved requirements, relevant input records/logs, validator reports | Authority to silently modify code or data |

The comparator may see both outputs only after both independent lanes finish. The independent SAS lane stays blinded during code generation and execution; do not assume that a UI tab or a Git branch alone enforces this boundary.

## 5. End-to-end workflow and human gates

1. **Create task and access authorization.** Assign task/study identifiers, roles, data-handling classification, retention policy, and permitted execution location.
2. **Register study references.** Upload protocol, SAP, ADaM specifications, Define-XML/metadata, SDTM specifications, controlled terminology, and other relevant references. Preserve originals and calculate checksums.
3. **Build requirements baseline.** Create requirement records with stable IDs, exact source document/version/page/section, dataset/variable, derivation, source domains/variables, joins, filters, population rules, keys, and acceptance criteria. Record ambiguity/conflicts; require named human resolution. Freeze an approved baseline version.
4. **Production R lane.** Upload the production R program and SDTM inputs. Inspect and approve exact code/input versions and execution settings. Execute in a controlled R environment. Capture logs, versions, checksums, output ADaM datasets, and run metadata.
5. **R traceability and CDISC checks.** Assess code/output evidence against requirements and execute the configured conformance validator on applicable datasets/metadata. Human reviewers verify the evidence and exceptions.
6. **Independent SAS QC lane.** Provide only the approved requirements baseline, permitted study documents, metadata, and SDTM inputs. Generate a draft SAS program independently. Trace each generated derivation to requirement IDs. Human reviewer inspects the exact code, assumptions, and diffs and approves that checksum before execution. Run in controlled SAS environment and capture logs, software version, inputs, checksums, and QC ADaM outputs.
7. **SAS traceability and CDISC checks.** Apply the corresponding baseline and conformance review to SAS code/output. Record unresolved items.
8. **Deterministic output comparison.** Human confirms comparison profile. Compare R and SAS ADaM outputs and store full evidence and settings.
9. **Difference review and suggestions.** Show deterministic findings first. The application may suggest hypotheses/remediation with source evidence and limitations. A human accepts, edits, rejects, or marks inconclusive; no code or document changes happen automatically.
10. **Controlled change cycle.** A verified suggestion becomes a change request/diff. Human approves the exact proposed change, authorized personnel apply it as a new version, and reviewers approve rerun. Rerun affected traceability, conformance, and comparison checks. Keep superseded artifacts and findings linked.
11. **Consolidated report and sign-off.** Reviewer verifies each conclusion and source link, resolves or explicitly accepts open items, then signs off through the organization’s validated identity/signature workflow. Generate a report and artifact package with status clearly labeled (draft, under review, approved under controlled process).

### Approval gates (must block progression)

- Requirements baseline unresolved or not approved → block program generation/final assessment.
- Production R program/run not approved → block production R execution.
- SAS QC program not reviewed and approved at its exact checksum → block SAS execution.
- Comparison settings not reviewed/approved → block final comparator status.
- Any finding lacks evidence/source links or human disposition → block final sign-off.
- Any input/program/setting changes after a run → invalidate dependent outputs, approvals, and results; require rerun and re-review.
- Suggestions accepted without an authorized change approval → never modify program/document or trigger execution.

## 6. Requirements and traceability model

Each requirement should have a stable identifier and at least these fields:

- Requirement ID, title, type, status, owner, and baseline version.
- Source citation: document identifier/version/checksum, page/section/table, quoted or structured excerpt, and extraction method.
- Scope: ADaM dataset, variables, population, records/keys, and applicable workflow branch.
- Rule: source domains/variables; joins and cardinality; filters; derivation; missingness; type/format; controlled terminology; constraints; and expected checks.
- Resolution: conflicts, assumptions, reviewer decision, rationale, timestamp, and approval reference.
- Evidence links: R code repository/version/file/line range and run evidence; SAS code version/file/line range and run evidence; output dataset/variable/key; validator rule/report; comparator finding.
- Coverage per branch: covered, partial, not covered, not applicable, or unverified, with reviewer status.

Automated document extraction may propose candidate requirements but must preserve exact citations and confidence/limitations. It cannot decide conflicts or create an approved baseline without human approval. For code traceability, line numbers should be tied to immutable code version/checksum because line numbers drift between edits.

## 7. Program and execution controls

### Production R

- Display code and a proposed execution plan; require human confirmation of code checksum, SDTM inputs, runtime/package versions, parameters, working directory, output location, and resource limits.
- Execute in an isolated least-privilege environment; no unrestricted filesystem/network access by default.
- Capture stdout/stderr, exit status, package lock/session info, environment, timestamps, input/output checksums, and output schema.

### SAS QC

- Generate only from approved references and requirements; record source citations for each generated block or derivation.
- Clearly label generated SAS as draft and show assumptions and unresolved requirements.
- Require independent human code review and checksum-bound approval before SAS execution.
- Run only in an approved SAS installation/environment with limited input/output access, controlled logs, time/resource limits, and captured SAS version/options.
- Do not provide production R implementation, logs, or outputs to the generation step. The output comparator gets both ADaM outputs only after run completion.

Static code inspection can find suspicious constructs but cannot prove all semantic behavior. Use targeted test cases, execution evidence, and reviewer verification; flag non-demonstrable requirements as unverified.

## 8. Deterministic comparison design

Choose one authoritative comparison engine and version it. Evaluate `diffdf` as the R-native primary comparator for a Shiny/R workflow; consider SAS `PROC COMPARE` as a separately configured corroborating option where a licensed SAS environment is available. Do not combine tool outputs without reconciling rule semantics.

A versioned comparison profile must explicitly define:

- Dataset identity and expected structure; variable name/type/label/order comparisons.
- Key variables, key normalization, duplicate-key policy, and row-order fallback (prefer no implicit row-order matching).
- Record-only-on-one-side and variable-only-on-one-side rules.
- Missing/blank/special missing conventions, character trimming/case rules, date/time conversion.
- Numeric absolute/relative tolerance, precision, and handling of NaN/infinity.
- Formats, labels, controlled terminology, sort order, and any allowed exceptions.
- Comparator engine/version, settings checksum, run timestamp, and output checksums.

Every finding should contain dataset, variable, key, reference value, QC value, classification, exact comparison rule, and link to source rows. Validate comparator semantics with fixtures for equal data, missingness, duplicate keys, type differences, floating-point tolerance, dates, character whitespace/case, and unmatched rows.

## 9. CDISC conformance and standards checks

Treat conformance as a separate review stream for each applicable output and metadata package. Evaluate Pinnacle 21 and/or CDISC CORE support for the project’s standards, Define-XML version, controlled terminology, and regulatory rule set. Record validator product/version, ruleset/version, configuration, input checksums, report, and reviewer disposition for every issue. Do not equate P21/CORE results with code-to-spec conformance, output agreement, or validated-system status.

## 10. Difference triage and controlled suggestions

For each comparator finding, the application may present candidate categories such as:

- Production R logic or SAS QC logic issue.
- Source SDTM issue or unexpected source records.
- Mapping, join key, cardinality, or filter issue.
- Requirements/specification gap, ambiguity, or conflict.
- SAS/R language behavior, type conversion, missing-value, date, format, or precision difference.
- Runtime/package/SAS version, parameter, or comparator profile issue.

A suggestion must include: classification as **application-proposed hypothesis**; confidence or evidence sufficiency; supporting and contradicting evidence; traceable document/code/data references; assumptions; possible impact; and a proposed next investigative action. Never claim root cause without human verification. Never edit programs/documents or rerun code from the suggestion UI. An accepted suggestion becomes a controlled change request with exact patch/diff, affected requirement IDs, approver, rationale, old/new checksums, and rerun plan.

## 11. Consolidated review report and artifact package

The report should include:

1. Task identity, status, reviewers/roles, dates, and limitations.
2. Approved requirement baseline and document register with versions/checksums.
3. R and SAS program/input/output manifests, run environments, logs, checksums, and approval gates.
4. Per-branch requirement traceability coverage and evidence; unresolved/ambiguous requirements.
5. CDISC validation reports and review dispositions.
6. Comparator engine/version/profile, summary counts, and full finding evidence.
7. Application-proposed hypotheses and remediation suggestions, separately labeled from deterministic results.
8. Human verification, dispositions, rationales, change requests, approvals, and signatures.
9. Open issues, exceptions, residual risk, reproducibility manifest, and final status.

The report must not label outputs “validated” merely because the workflow passed or a reviewer clicked approve. The organization’s validated process determines the permitted final status. A prototype’s typed name and in-memory log are not validated electronic signature or compliant audit trail.

## 12. Security, privacy, and validation prerequisites

Before real study use, define data classification, authorized users, identity/role control, encryption, safe temporary storage, retention/deletion, backup, network boundaries, least-privilege service accounts, malware/file validation, execution isolation, secrets handling, audit integrity, incident response, and disaster recovery. Uploaded documents and programs are untrusted inputs. Never evaluate uploaded code in the Shiny process.

Production readiness requires a formal intended-use assessment, risk analysis, requirements and test evidence, change control, independent verification, validated infrastructure, SOPs/training, operational monitoring, backup/restore testing, and appropriate electronic record/signature controls. This blueprint alone supplies none of these assurances.

## 13. Proposed implementation roadmap

| Phase | Deliverable | Acceptance evidence |
|---|---|---|
| 0. Review and scope | Approved blueprint, roles, intended use, threat model, requirements | Stakeholder decisions recorded; open questions assigned |
| 1. Workflow prototype | Multi-step UI, state machine, synthetic fixtures, no real execution | State transition and gate tests; clear prototype labels |
| 2. Intake and baseline | Secure document/dataset registration, checksums, traceability matrix, versioned baseline | Input validation tests; citation and approval trace demonstrable |
| 3. R lane | Isolated R execution, output registry, logs and provenance | Reproducible run tests; approval-before-run enforced |
| 4. SAS QC lane | Independent requirements-based draft generation, code review, controlled SAS execution | Blinding/access tests; checksum-bound code approval; SAS run manifest |
| 5. Traceability/conformance | Per-branch checks, P21/CORE integration evaluation, review workflow | Known fixtures and validator report provenance tests |
| 6. Comparator and triage | Versioned comparison profiles, evidence-rich findings, non-mutating suggestions | Comparator fixtures; no-auto-edit/run tests; finding traceability |
| 7. Report, sign-off, and control | Consolidated report, durable audit and identity/signature integration | Role/gate, audit integrity, reproducibility and approval tests |
| 8. Formal validation | Intended-use validation, SOPs, operational qualification as required | Approved validation package before any regulated use |

Do not skip the independence, review, and audit controls to accelerate later phases. For an initial demonstrator, use only synthetic data and mocked execution/conformance results.

## 14. Decisions requested from reviewers

Please confirm or revise the following before the next implementation blueprint is translated into a detailed Markdown specification:

1. Is this a local-only prototype first, or should the target architecture be a centrally hosted multi-user system?
2. Which document/version is authoritative when protocol, SAP, ADaM specs, metadata, or Define-XML conflict?
3. Which ADaM specifications/metadata formats will be supported initially (Excel, Word, Define-XML, other)?
4. Which R and SAS execution environments are available and approved? Is local execution intentionally excluded from the first prototype?
5. Which comparator should be authoritative initially: `diffdf`, another R comparator, or SAS `PROC COMPARE`? What key/tolerance conventions are required?
6. Which CDISC validator and rule set are available/licensed, and which SDTM/ADaM/Define-XML versions are in scope?
7. What identity, separation-of-duties, approval, and electronic-signature process is required?
8. What report format and artifact retention period are desired?
9. Is any AI service permitted for post-comparison hypotheses? If yes, what data boundary, approved model, and review controls apply? Otherwise, retain deterministic rule-based triage only.
10. What is the intended-use boundary and what must be completed before real study data may be used?

## Appendix A. Current prototype versus target

The current Shiny prototype is a local upload-and-compare demonstrator with an R-side deterministic comparator, program previews, synthetic demo, human disposition, and session-level audit. Its optional local R execution is explicitly disabled unless a user opts in and is not sandboxed; it should remain disabled for untrusted or real study code. It does not yet implement document ingestion/traceability, independent SAS generation or execution, CDISC validator integration, durable storage/audit, validated identity/signatures, or the consolidated controlled report described above.
