empty_requirement_candidates <- function() {
  data.frame(
    requirement_id = character(),
    target = character(),
    source_ref = character(),
    rule = character(),
    source_file = character(),
    evidence = character(),
    extraction_type = character(),
    stringsAsFactors = FALSE
  )
}

candidate_row <- function(
  id,
  target,
  source_ref,
  rule,
  source_file,
  evidence,
  type
) {
  data.frame(
    requirement_id = id,
    target = target,
    source_ref = source_ref,
    rule = rule,
    source_file = source_file,
    evidence = evidence,
    extraction_type = type,
    stringsAsFactors = FALSE
  )
}
xml_attribute_text <- function(node) {
  attrs <- xml2::xml_attrs(node)
  if (!length(attrs)) {
    return("")
  }
  paste(paste(names(attrs), as.character(attrs), sep = "="), collapse = "; ")
}

extract_define_candidates <- function(
  path,
  filename,
  version,
  target_dataset = ""
) {
  doc <- xml2::read_xml(path)
  groups <- xml2::xml_find_all(doc, "//*[local-name()='ItemGroupDef']")
  group_names <- xml2::xml_attr(groups, "Name")
  target_dataset <- toupper(trimws(target_dataset %||% ""))
  if (nzchar(target_dataset) && !target_dataset %in% group_names) {
    task_tokens <- strsplit(gsub("[^A-Z0-9]+", " ", target_dataset), " +")[[1]]
    matches <- intersect(group_names, task_tokens)
    if (length(matches) == 1L) target_dataset <- matches
  }
  if (nzchar(target_dataset) && target_dataset %in% group_names) {
    groups <- groups[group_names == target_dataset]
  }
  item_defs <- xml2::xml_find_all(doc, "//*[local-name()='ItemDef']")
  item_oids <- xml2::xml_attr(item_defs, "OID")
  rows <- list()

  for (group in groups) {
    dataset <- xml2::xml_attr(group, "Name") %||% xml2::xml_attr(group, "OID")
    group_attrs <- xml_attribute_text(group)
    rows[[length(rows) + 1L]] <- candidate_row(
      sprintf("REQ-%03d", length(rows) + 1L),
      dataset,
      sprintf(
        "%s v%s; Define-XML ItemGroupDef %s",
        filename,
        version,
        xml2::xml_attr(group, "OID")
      ),
      paste("Verify dataset structure and keys from metadata:", group_attrs),
      filename,
      group_attrs,
      "Define-XML dataset metadata"
    )

    refs <- xml2::xml_find_all(group, "./*[local-name()='ItemRef']")
    for (ref in refs) {
      oid <- xml2::xml_attr(ref, "ItemOID")
      item <- item_defs[match(oid, item_oids)]
      if (!length(item) || is.na(oid)) {
        next
      }
      name <- xml2::xml_attr(item, "Name") %||% oid
      attrs <- xml_attribute_text(item)
      label <- xml2::xml_attr(item, "Label") %||% ""
      detail <- paste(c(sprintf("label=%s", label), attrs), collapse = "; ")
      rows[[length(rows) + 1L]] <- candidate_row(
        sprintf("REQ-%03d", length(rows) + 1L),
        paste0(dataset, ".", name),
        sprintf("%s v%s; Define-XML ItemDef %s", filename, version, oid),
        paste(
          "Verify variable metadata and derivation against the approved study documents:",
          detail
        ),
        filename,
        detail,
        "Define-XML variable metadata"
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else empty_requirement_candidates()
}

extract_pdf_candidates <- function(
  path,
  filename,
  category,
  version,
  max_pages = 500L
) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install the pdftools package to extract PDF text.", call. = FALSE)
  }
  pages <- pdftools::pdf_text(path)
  if (length(pages) > max_pages) {
    pages <- pages[seq_len(max_pages)]
  }
  rows <- list()
  relevant <- "(?i)protocol|statistical analysis plan|SAP|specification|derive|derivation|baseline|population|analysis flag|imput|missing|window|endpoint|visit|treatment|censor|source variable|parameter|ADaM|SDTM|filter|join|primary analysis|secondary analysis"

  for (page_index in seq_along(pages)) {
    text <- gsub("[\\r\\t]+", " ", pages[[page_index]])
    lines <- trimws(strsplit(text, "\\n")[[1]])
    lines <- lines[nzchar(lines)]
    evidence_lines <- lines[grepl(relevant, lines, perl = TRUE)]
    if (!length(evidence_lines)) {
      next
    }
    evidence <- substr(paste(evidence_lines, collapse = " "), 1L, 1800L)
    evidence <- gsub("\\|", "/", evidence)
    target_match <- regmatches(
      evidence,
      regexpr(
        "\\b[A-Z][A-Z0-9]{1,7}\\.[A-Z][A-Z0-9]{1,8}\\b",
        evidence,
        perl = TRUE
      )
    )
    target <- if (length(target_match) && nzchar(target_match)) {
      target_match
    } else {
      "Review required"
    }
    rows[[length(rows) + 1L]] <- candidate_row(
      sprintf("REQ-%03d", length(rows) + 1L),
      target,
      sprintf(
        "%s v%s; %s; PDF page %d",
        filename,
        version,
        category,
        page_index
      ),
      paste(
        "Draft extracted passage; reviewer must identify the applicable dataset/variable and verify the requirement:",
        evidence
      ),
      filename,
      evidence,
      "PDF text passage"
    )
  }
  if (length(rows)) do.call(rbind, rows) else empty_requirement_candidates()
}

extract_text_candidates <- function(
  path,
  filename,
  category,
  version,
  extension
) {
  records <- switch(
    extension,
    txt = {
      lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
      lapply(seq_along(lines), function(i) {
        list(location = sprintf("text line %d", i), evidence = lines[i])
      })
    },
    csv = {
      lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
      lapply(seq_along(lines), function(i) {
        list(location = sprintf("CSV row %d", i), evidence = lines[i])
      })
    },
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install readxl to extract Excel text.", call. = FALSE)
      }
      sheets <- readxl::excel_sheets(path)
      unlist(
        lapply(sheets, function(sheet) {
          data <- as.data.frame(readxl::read_excel(
            path,
            sheet = sheet,
            col_names = FALSE
          ))
          lapply(seq_len(nrow(data)), function(i) {
            list(
              location = sprintf("worksheet %s row %d", sheet, i),
              evidence = paste(
                unlist(data[i, , drop = FALSE], use.names = FALSE),
                collapse = " | "
              )
            )
          })
        }),
        recursive = FALSE
      )
    },
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install readxl to extract Excel text.", call. = FALSE)
      }
      sheets <- readxl::excel_sheets(path)
      unlist(
        lapply(sheets, function(sheet) {
          data <- as.data.frame(readxl::read_excel(
            path,
            sheet = sheet,
            col_names = FALSE
          ))
          lapply(seq_len(nrow(data)), function(i) {
            list(
              location = sprintf("worksheet %s row %d", sheet, i),
              evidence = paste(
                unlist(data[i, , drop = FALSE], use.names = FALSE),
                collapse = " | "
              )
            )
          })
        }),
        recursive = FALSE
      )
    },
    docx = {
      extracted <- tempfile("docx_extract_")
      dir.create(extracted)
      on.exit(unlink(extracted, recursive = TRUE), add = TRUE)
      members <- utils::unzip(path, list = TRUE)$Name
      if (!"word/document.xml" %in% members) {
        stop("DOCX package has no word/document.xml part.", call. = FALSE)
      }
      utils::unzip(path, files = "word/document.xml", exdir = extracted)
      document <- xml2::read_xml(file.path(extracted, "word", "document.xml"))
      paragraphs <- xml2::xml_find_all(
        document,
        "//*[local-name()='body']/*[local-name()='p']"
      )
      lapply(seq_along(paragraphs), function(i) {
        list(
          location = sprintf("DOCX paragraph %d", i),
          evidence = trimws(xml2::xml_text(paragraphs[[i]]))
        )
      })
    },
    stop("Unsupported document type for text extraction.", call. = FALSE)
  )
  relevant <- "(?i)protocol|statistical analysis plan|SAP|specification|derive|derivation|baseline|population|analysis flag|imput|missing|window|endpoint|visit|treatment|censor|source variable|parameter|ADaM|SDTM|filter|join|primary analysis|secondary analysis"
  keep <- Filter(
    function(record) {
      nzchar(trimws(record$evidence)) &&
        grepl(relevant, record$evidence, perl = TRUE)
    },
    records
  )
  if (!length(keep)) {
    return(empty_requirement_candidates())
  }
  keep <- head(keep, 500L)
  do.call(
    rbind,
    lapply(seq_along(keep), function(i) {
      evidence <- gsub("\\|", "/", substr(keep[[i]]$evidence, 1L, 1800L))
      target_match <- regmatches(
        evidence,
        regexpr(
          "\\b[A-Z][A-Z0-9]{1,7}\\.[A-Z][A-Z0-9]{1,8}\\b",
          evidence,
          perl = TRUE
        )
      )
      target <- if (length(target_match) && nzchar(target_match)) {
        target_match
      } else {
        "Review required"
      }
      candidate_row(
        sprintf("REQ-%03d", i),
        target,
        sprintf(
          "%s v%s; %s; %s",
          filename,
          version,
          category,
          keep[[i]]$location
        ),
        paste(
          "Draft extracted passage; reviewer must verify and structure the requirement:",
          evidence
        ),
        filename,
        evidence,
        "Text / spreadsheet passage"
      )
    })
  )
}

extract_document_candidates <- function(
  path,
  filename,
  category = "Other",
  version = "Unspecified",
  target_dataset = ""
) {
  extension <- tolower(tools::file_ext(filename))
  candidates <- switch(
    extension,
    xml = extract_define_candidates(
      path,
      basename(filename),
      version,
      target_dataset
    ),
    pdf = extract_pdf_candidates(path, basename(filename), category, version),
    txt = extract_text_candidates(
      path,
      basename(filename),
      category,
      version,
      extension
    ),
    csv = extract_text_candidates(
      path,
      basename(filename),
      category,
      version,
      extension
    ),
    xls = extract_text_candidates(
      path,
      basename(filename),
      category,
      version,
      extension
    ),
    xlsx = extract_text_candidates(
      path,
      basename(filename),
      category,
      version,
      extension
    ),
    docx = extract_text_candidates(
      path,
      basename(filename),
      category,
      version,
      extension
    ),
    stop(
      sprintf("No extractor is configured for .%s files.", extension),
      call. = FALSE
    )
  )
  if (nrow(candidates)) {
    candidates$requirement_id <- sprintf("REQ-%03d", seq_len(nrow(candidates)))
  }
  candidates
}

candidate_requirement_lines <- function(candidates) {
  if (is.null(candidates) || !nrow(candidates)) {
    return(character())
  }
  paste(
    candidates$requirement_id,
    candidates$target,
    candidates$source_ref,
    gsub("\\|", "/", candidates$rule),
    sep = " | "
  )
}
