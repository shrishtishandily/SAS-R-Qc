`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# Builds an escaped HTML table; uploaded values are never interpreted as HTML.
html_table <- function(df, max_rows = 200L, class = "data-table") {
  if (is.null(df) || nrow(df) == 0) {
    return(shiny::div(class = "empty-note", "No rows to display."))
  }
  shown <- utils::head(df, max_rows)
  head_row <- shiny::tags$tr(lapply(names(shown), function(n) shiny::tags$th(scope = "col", n)))
  body_rows <- lapply(seq_len(nrow(shown)), function(i) {
    shiny::tags$tr(lapply(shown[i, , drop = TRUE], function(v) {
      shiny::tags$td(if (is.na(v)) "NA" else as.character(v))
    }))
  })
  note <- if (nrow(df) > max_rows) {
    shiny::div(class = "table-note", sprintf("Showing first %d of %d rows.", max_rows, nrow(df)))
  }
  shiny::tagList(
    shiny::div(
      class = "table-scroll",
      shiny::tags$table(class = class, shiny::tags$thead(head_row), shiny::tags$tbody(body_rows))
    ),
    note
  )
}

pill <- function(text, kind) shiny::span(class = paste("pill", kind), text)

PILL_DETERMINISTIC <- function() pill("DETERMINISTIC COMPARATOR RESULT", "det")
PILL_HINT <- function() pill("RULE-BASED HINT · NOT AI · NOT A CONCLUSION", "hint")
PILL_HUMAN <- function() pill("HUMAN DISPOSITION", "human")
