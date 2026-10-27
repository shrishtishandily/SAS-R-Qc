new_audit_log <- function() {
  data.frame(
    seq = integer(), time = character(), actor = character(), state = character(),
    event = character(), detail = character(), stringsAsFactors = FALSE
  )
}

# Append-only by convention: rows are only ever added, never edited or removed.
append_audit <- function(log, actor, state, event, detail = "") {
  rbind(log, data.frame(
    seq = nrow(log) + 1L, time = now_stamp(), actor = actor, state = state,
    event = event, detail = detail, stringsAsFactors = FALSE
  ))
}
