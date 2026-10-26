WORKFLOW_STATES <- c(
  "setup", "review", "ready_to_compare", "compared", "dispositioned", "approved"
)

WORKFLOW_LABELS <- c(
  setup = "Task setup",
  review = "Code review",
  ready_to_compare = "Ready to compare",
  compared = "Comparison complete",
  dispositioned = "Disposition recorded",
  approved = "Human approved"
)

allowed_transitions <- list(
  setup = "review",
  review = "ready_to_compare",
  ready_to_compare = "compared",
  compared = "dispositioned",
  dispositioned = "approved",
  approved = character()
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
