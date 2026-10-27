KEY_SEP <- "\u001f"

display_key <- function(k) gsub(KEY_SEP, " | ", k, fixed = TRUE)

# Character values are trimmed and blank is treated as missing (SAS convention).
trim_blank <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

display_vector <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(format(x))
  if (is.numeric(x)) {
    out <- format(x, digits = 15, trim = TRUE, scientific = FALSE)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  as.character(x)
}

as_compare_vector <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt") || inherits(x, "difftime") || is.logical(x) || is.numeric(x)) {
    return(as.numeric(x))
  }
  trim_blank(x)
}

type_label <- function(x) {
  if (inherits(x, "Date")) "date"
  else if (inherits(x, "POSIXt")) "datetime"
  else if (is.numeric(x) || is.logical(x)) "numeric"
  else "character"
}

make_keys <- function(df, keys) {
  parts <- lapply(keys, function(k) {
    v <- trimws(display_vector(df[[k]]))
    v[is.na(v)] <- "<NA>"
    v
  })
  do.call(paste, c(parts, sep = KEY_SEP))
}

compare_pair <- function(a, b, tolerance) {
  na_v <- as_compare_vector(a)
  nb_v <- as_compare_vector(b)

  if (is.numeric(na_v) != is.numeric(nb_v)) {
    chr <- if (is.numeric(na_v)) nb_v else na_v
    conv <- suppressWarnings(as.numeric(chr))
    if (all(is.na(conv) == is.na(chr))) {
      if (is.numeric(na_v)) nb_v <- conv else na_v <- conv
    } else {
      na_v <- trim_blank(display_vector(a))
      nb_v <- trim_blank(display_vector(b))
    }
  }

  numeric_mode <- is.numeric(na_v)
  a_na <- is.na(na_v)
  b_na <- is.na(nb_v)
  both <- !a_na & !b_na
  differs <- if (numeric_mode) {
    a_na != b_na | (both & abs(na_v - nb_v) > tolerance)
  } else {
    a_na != b_na | (both & na_v != nb_v)
  }
  differs[is.na(differs)] <- FALSE
  idx <- which(differs)

  category <- rep("value_mismatch", length(idx))
  category[!a_na[idx] & b_na[idx]] <- "missing_in_qc"
  category[a_na[idx] & !b_na[idx]] <- "missing_in_ref"
  if (!numeric_mode) {
    both_idx <- !a_na[idx] & !b_na[idx]
    case_only <- both_idx & tolower(na_v[idx]) == tolower(nb_v[idx])
    category[case_only] <- "case_only"
  }

  list(
    idx = idx,
    category = category,
    numeric = numeric_mode,
    signed_diff = if (numeric_mode) (nb_v - na_v)[idx] else rep(NA_real_, length(idx)),
    ref_display = display_vector(a)[idx],
    qc_display = display_vector(b)[idx]
  )
}

# Deterministic pattern checks over the differing records; they describe, not conclude.
pattern_hints <- function(cmp, ref_m, idx, others) {
  hints <- character()
  inside <- seq_len(nrow(ref_m)) %in% idx

  for (o in others) {
    miss <- is.na(as_compare_vector(ref_m[[o]]))
    p_in <- mean(miss[inside])
    p_out <- if (any(!inside)) mean(miss[!inside]) else 0
    if (p_in >= 0.8 && p_out <= 0.2) {
      hints <- c(hints, sprintf(
        "%s is missing in the reference for %d%% of differing records vs %d%% of other records.",
        o, round(100 * p_in), round(100 * p_out)
      ))
    }
  }

  if (all(cmp$category == "missing_in_qc")) {
    hints <- c(hints, "QC value is missing wherever the reference has a value.")
  } else if (all(cmp$category == "missing_in_ref")) {
    hints <- c(hints, "Reference value is missing wherever QC has a value.")
  } else if (all(cmp$category == "case_only")) {
    hints <- c(hints, "Differences are letter-case only.")
  } else if (cmp$numeric) {
    d <- cmp$signed_diff[!is.na(cmp$signed_diff)]
    if (length(d) > 1 && length(unique(round(d, 10))) == 1) {
      hints <- c(hints, sprintf("All numeric differences share the same offset (QC - reference = %s).", format(d[1], digits = 10)))
    } else if (length(d) > 0 && max(abs(d)) < 0.01) {
      hints <- c(hints, "All numeric differences are below 0.01; check rounding or precision conventions.")
    }
  }
  if (length(hints)) paste(hints, collapse = " ") else NA_character_
}

compare_datasets <- function(ref, qc, keys = character(), tolerance = 1e-8, max_diff_rows = 50000L) {
  stopifnot(is.data.frame(ref), is.data.frame(qc))
  common <- intersect(names(ref), names(qc))
  only_ref <- setdiff(names(ref), names(qc))
  only_qc <- setdiff(names(qc), names(ref))

  bad_keys <- setdiff(keys, common)
  if (length(bad_keys)) {
    stop("Key variable(s) not present in both datasets: ", paste(bad_keys, collapse = ", "), call. = FALSE)
  }

  row_order <- length(keys) == 0
  ref_key <- if (row_order) as.character(seq_len(nrow(ref))) else make_keys(ref, keys)
  qc_key <- if (row_order) as.character(seq_len(nrow(qc))) else make_keys(qc, keys)

  dup_ref <- unique(ref_key[duplicated(ref_key)])
  dup_qc <- unique(qc_key[duplicated(qc_key)])
  keep_ref <- !duplicated(ref_key)
  keep_qc <- !duplicated(qc_key)
  ref1 <- ref[keep_ref, , drop = FALSE]
  qc1 <- qc[keep_qc, , drop = FALSE]
  ref_key1 <- ref_key[keep_ref]
  qc_key1 <- qc_key[keep_qc]

  matched <- intersect(ref_key1, qc_key1)
  ref_m <- ref1[match(matched, ref_key1), , drop = FALSE]
  qc_m <- qc1[match(matched, qc_key1), , drop = FALSE]

  type_mismatch <- do.call(rbind, lapply(common, function(v) {
    tr <- type_label(ref[[v]])
    tq <- type_label(qc[[v]])
    if (tr != tq) data.frame(variable = v, reference_type = tr, qc_type = tq, stringsAsFactors = FALSE)
  }))
  if (is.null(type_mismatch)) {
    type_mismatch <- data.frame(variable = character(), reference_type = character(), qc_type = character())
  }

  value_vars <- setdiff(common, keys)
  var_rows <- list()
  diff_rows <- list()
  hints <- list()
  for (v in value_vars) {
    res <- compare_pair(ref_m[[v]], qc_m[[v]], tolerance)
    n_diff <- length(res$idx)
    var_rows[[v]] <- data.frame(
      variable = v,
      n_compared = length(matched),
      n_diff = n_diff,
      n_missing_in_qc = sum(res$category == "missing_in_qc"),
      n_missing_in_ref = sum(res$category == "missing_in_ref"),
      n_value_mismatch = sum(res$category %in% c("value_mismatch", "case_only")),
      all_case_only = n_diff > 0 && all(res$category == "case_only"),
      max_abs_diff = if (res$numeric && n_diff > 0 && any(!is.na(res$signed_diff))) max(abs(res$signed_diff), na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (n_diff > 0) {
      diff_rows[[v]] <- data.frame(
        key = display_key(matched[res$idx]), variable = v,
        ref_value = res$ref_display, qc_value = res$qc_display,
        category = res$category, abs_diff = abs(res$signed_diff),
        stringsAsFactors = FALSE
      )
      hints[[v]] <- pattern_hints(res, ref_m, res$idx, setdiff(value_vars, v))
    }
  }

  var_summary <- if (length(var_rows)) do.call(rbind, var_rows) else NULL
  diffs <- if (length(diff_rows)) do.call(rbind, diff_rows) else NULL
  if (!is.null(var_summary)) rownames(var_summary) <- NULL
  if (!is.null(diffs)) rownames(diffs) <- NULL
  total_diffs <- if (is.null(diffs)) 0L else nrow(diffs)
  truncated <- total_diffs > max_diff_rows
  if (truncated) diffs <- diffs[seq_len(max_diff_rows), , drop = FALSE]

  rows_only_ref <- display_key(setdiff(ref_key1, qc_key1))
  rows_only_qc <- display_key(setdiff(qc_key1, ref_key1))
  n_vars_diff <- if (is.null(var_summary)) 0L else sum(var_summary$n_diff > 0)

  identical <- !length(only_ref) && !length(only_qc) && !nrow(type_mismatch) &&
    !length(dup_ref) && !length(dup_qc) && !length(rows_only_ref) && !length(rows_only_qc) &&
    total_diffs == 0

  list(
    summary = list(
      n_ref = nrow(ref), n_qc = nrow(qc), n_matched = length(matched),
      n_only_ref = length(rows_only_ref), n_only_qc = length(rows_only_qc),
      n_vars_compared = length(value_vars), n_vars_diff = n_vars_diff, total_diffs = total_diffs,
      keys = keys, key_mode = if (row_order) "row order" else "key variables",
      tolerance = tolerance, identical = identical, truncated = truncated
    ),
    structure = list(only_ref = only_ref, only_qc = only_qc, type_mismatch = type_mismatch),
    rows = list(only_ref = rows_only_ref, only_qc = rows_only_qc),
    duplicates = list(ref = display_key(dup_ref), qc = display_key(dup_qc)),
    var_summary = var_summary,
    diffs = diffs,
    hints = hints
  )
}
