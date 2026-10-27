ALLOWED_DATA_EXT <- c("xpt", "sas7bdat", "csv")
ALLOWED_CODE_EXT <- c("sas", "r", "txt")
MAX_DATA_BYTES <- 200 * 1024^2
MAX_CODE_BYTES <- 1024^2

file_ext_lower <- function(name) tolower(tools::file_ext(name))

normalize_dataset <- function(df) {
  df <- haven::zap_formats(haven::zap_label(haven::zap_labels(df)))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  names(df) <- toupper(names(df))
  if (anyDuplicated(names(df))) {
    stop("Dataset has column names that are duplicated after upper-casing.", call. = FALSE)
  }
  df
}

read_dataset <- function(path, name) {
  ext <- file_ext_lower(name)
  if (!ext %in% ALLOWED_DATA_EXT) {
    stop(sprintf("Unsupported dataset type '.%s'. Allowed: %s.", ext, paste0(".", ALLOWED_DATA_EXT, collapse = ", ")), call. = FALSE)
  }
  if (file.size(path) > MAX_DATA_BYTES) {
    stop(sprintf("File exceeds the %d MB limit.", MAX_DATA_BYTES %/% 1024^2), call. = FALSE)
  }
  df <- tryCatch(
    switch(ext,
      xpt = haven::read_xpt(path),
      sas7bdat = haven::read_sas(path),
      csv = utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
    ),
    error = function(e) stop("The file could not be read as a valid dataset.", call. = FALSE)
  )
  normalize_dataset(df)
}

dataset_meta <- function(df, path, name) {
  list(
    name = basename(name),
    rows = nrow(df),
    cols = ncol(df),
    md5 = unname(tools::md5sum(path)),
    loaded = now_stamp()
  )
}

read_code <- function(path, name) {
  ext <- file_ext_lower(name)
  if (!ext %in% ALLOWED_CODE_EXT) {
    stop(sprintf("Unsupported program type '.%s'. Allowed: %s.", ext, paste0(".", ALLOWED_CODE_EXT, collapse = ", ")), call. = FALSE)
  }
  if (file.size(path) > MAX_CODE_BYTES) stop("Program file exceeds the 1 MB limit.", call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!all(validUTF8(lines))) stop("Program file is not valid UTF-8 text.", call. = FALSE)
  paste(lines, collapse = "\n")
}
