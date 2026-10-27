WORKFLOW_STATES <- c("intake", "configured", "compared", "dispositioned", "approved")

WORKFLOW_LABELS <- c(
  intake = "Intake",
  configured = "Ready to compare",
  compared = "Compared",
  dispositioned = "Dispositioned",
  approved = "Approved"
)

# Forward path is linear; any state may return to intake when inputs change.
allowed_transitions <- list(
  intake = "configured",
  configured = c("compared", "intake"),
  compared = c("dispositioned", "configured", "intake"),
  dispositioned = c("approved", "configured", "intake"),
  approved = c("configured", "intake")
)

can_transition <- function(from, to) {
  from %in% names(allowed_transitions) && to %in% allowed_transitions[[from]]
}

transition_workflow <- function(state, to) {
  if (!can_transition(state, to)) {
    stop(sprintf("Invalid workflow transition: %s -> %s", state, to), call. = FALSE)
  }
  to
}
