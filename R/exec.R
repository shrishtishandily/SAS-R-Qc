r_exec_enabled <- function() identical(tolower(Sys.getenv("ADAM_QC_ALLOW_R_EXEC", "false")), "true")

# Runs an uploaded R program in a separate local process. This is NOT a sandbox:
# the program runs with the same permissions as the user who launched the app.
run_r_program <- function(program_text, input_paths = character(), timeout = 120) {
  work <- tempfile("qcrun_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  for (nm in names(input_paths)) file.copy(input_paths[[nm]], file.path(work, basename(nm)))
  writeLines(program_text, file.path(work, "program.R"), useBytes = TRUE)
  launcher <- file.path(work, "launcher.R")
  writeLines(c(sprintf("setwd(%s)", deparse(work)), "source('program.R', echo = FALSE)"), launcher)

  log_file <- file.path(work, "run.log")
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(launcher)),
    stdout = log_file, stderr = log_file, timeout = timeout
  ))
  log_lines <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
  log_text <- paste(utils::tail(log_lines, 40), collapse = "\n")

  if (identical(status, 124L)) stop(sprintf("Program exceeded the %d second time limit.", timeout), call. = FALSE)
  if (!identical(status, 0L)) stop("Program failed. Last output:\n", log_text, call. = FALSE)

  out <- list.files(work, pattern = "^qc_output\\.(csv|xpt)$", full.names = TRUE)
  if (!length(out)) stop("Program finished but did not write qc_output.csv or qc_output.xpt.", call. = FALSE)
  list(data = read_dataset(out[1], out[1]), path = out[1], log = log_text, md5 = unname(tools::md5sum(out[1])))
}
