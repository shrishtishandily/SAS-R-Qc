# ADaM QC Workbench

A synthetic-data-only prototype of a bounded, human-in-the-loop ADaM independent QC workbench for SAS-to-R migration. It demonstrates workflow, evidence provenance, human disposition, and session audit history using standard Shiny.

> **Prototype limitations:** This app contains no clinical or real study data. Its comparison is a fixed synthetic fixture, not an execution of SAS or R QC code. The “AI-proposed explanation” is static illustrative text; no AI/LLM is connected. No external services, file uploads, credentials, database, persistence, or network integrations are included. Do not use for clinical, regulatory, or production decisions.

## Synthetic scenario

The example finding represents a missing-ADT baseline mismatch. The deterministic fixture shows **SAS BASE = 66** and **R BASE = 55** (difference +11). These are synthetic aggregate values included only to demonstrate the review workflow.

## Requirements

- R 4.1 or later
- The `shiny` package

Install the sole R dependency from an R console:

```r
install.packages("shiny")
```

The app uses base R for all other logic and has no JavaScript or external service dependencies. Fonts and styling use local system fallbacks; no network resources are fetched.

## Launch

From the project directory in an R console:

```r
shiny::runApp()
```

Or from its parent directory:

```r
shiny::runApp("adam-qc-workbench")
```

The app is not launched as part of project setup. Buttons advance only the local Shiny session through the documented synthetic workflow.

## Workflow state machine

Allowed transitions are defined in `R/workflow.R` and enforced by `transition_workflow()`:

`setup → review → ready_to_compare → compared → dispositioned → approved`

The comparison state reveals only the fixed fixture. A reviewer must select a disposition and provide a rationale before recording it; approval is a separate action. There are no skip or reverse transitions in the prototype.

## Evidence provenance labels

- **Deterministic comparator result:** fixed, synthetic SAS/R values and difference.
- **AI-proposed explanation:** static placeholder copy, not model-generated.
- **Human-approved disposition:** recorded only after explicit reviewer actions; session state is not a validated electronic signature.

The audit history prepends locally generated session events. It is not tamper-proof, durable, or a compliant audit trail, and is lost when the session ends.

## Least-privilege design notes

The prototype has no upload control, runtime execution of candidate code, SAS integration, credentials, network calls, or durable storage. It handles only transient synthetic state. A real implementation would need, at minimum, approved identity and authorization, execution sandboxing, protected and governed data handling, durable tamper-evident audit controls, review/approval controls, validation, monitoring, and qualified operational procedures. This prototype provides none of those safeguards.

## Project structure

- `app.R` — Shiny entry point
- `R/workflow.R` — explicit state machine and transition guard
- `R/data.R` — synthetic finding and initial session audit event
- `R/ui.R` — dashboard UI
- `R/server.R` — session state, safe simulated interactions, and audit events
- `www/styles.css` — responsive local styling
- `tests/test_workflow.R` — base-R state-transition checks

## Run the lightweight checks

From the project directory:

```r
source("tests/test_workflow.R")
```

The checks validate permitted and rejected state transitions without starting the app.
