source(file.path("R", "workflow.R"))

stopifnot(identical(WORKFLOW_STATES, c(
  "setup", "review", "ready_to_compare", "compared", "dispositioned", "approved"
)))
stopifnot(can_transition("setup", "review"))
stopifnot(can_transition("compared", "dispositioned"))
stopifnot(can_transition("dispositioned", "approved"))
stopifnot(!can_transition("setup", "compared"))
stopifnot(!can_transition("approved", "setup"))
stopifnot(identical(transition_workflow("review", "ready_to_compare"), "ready_to_compare"))

invalid_transition <- try(transition_workflow("setup", "approved"), silent = TRUE)
stopifnot(inherits(invalid_transition, "try-error"))
