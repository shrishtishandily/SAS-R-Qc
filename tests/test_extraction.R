source("app.R")

xml_path <- tempfile(fileext = ".xml")
writeLines(
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.2" xmlns:def="http://www.cdisc.org/ns/def/v1.0">',
    '  <Study OID="S"><MetaDataVersion OID="MDV" Name="Test">',
    '    <ItemGroupDef OID="ADAE" Name="ADAE" def:Structure="one row per event" def:DomainKeys="USUBJID, AESEQ">',
    '      <ItemRef ItemOID="ADAE.USUBJID" OrderNumber="1"/>',
    '      <ItemRef ItemOID="ADAE.AESEQ" OrderNumber="2"/>',
    '    </ItemGroupDef>',
    '    <ItemGroupDef OID="ADSL" Name="ADSL" def:Structure="one row per subject" def:DomainKeys="USUBJID">',
    '      <ItemRef ItemOID="ADSL.USUBJID" OrderNumber="1"/>',
    '    </ItemGroupDef>',
    '    <ItemDef OID="ADAE.USUBJID" Name="USUBJID" DataType="text" Label="Subject ID" Comment="DM.USUBJID"/>',
    '    <ItemDef OID="ADAE.AESEQ" Name="AESEQ" DataType="integer" Label="Sequence"/>',
    '    <ItemDef OID="ADSL.USUBJID" Name="USUBJID" DataType="text" Label="Subject ID"/>',
    '  </MetaDataVersion></Study>',
    '</ODM>'
  ),
  xml_path
)

adae <- extract_document_candidates(
  xml_path,
  "define.xml",
  "Define-XML / metadata",
  "1.0",
  "ADAE"
)
stopifnot(nrow(adae) == 3L)
stopifnot(all(grepl("ADAE", adae$target, fixed = TRUE)))
stopifnot(any(grepl("DomainKeys=USUBJID, AESEQ", adae$evidence, fixed = TRUE)))
stopifnot(any(grepl("ItemDef ADAE.AESEQ", adae$source_ref, fixed = TRUE)))

adsl <- extract_document_candidates(
  xml_path,
  "define.xml",
  "Define-XML / metadata",
  "1.0",
  "ADSL QC"
)
stopifnot(nrow(adsl) == 2L, all(grepl("ADSL", adsl$target, fixed = TRUE)))

text_path <- tempfile(fileext = ".txt")
writeLines(
  c(
    "Study document",
    "ADAE.ASTDT is derived from the adverse event start date according to the analysis specification.",
    "Unrelated administrative contact information."
  ),
  text_path
)
text_candidates <- extract_document_candidates(
  text_path,
  "spec.txt",
  "ADaM specification",
  "2.1"
)
stopifnot(nrow(text_candidates) == 1L)
stopifnot(text_candidates$target == "ADAE.ASTDT")
stopifnot(grepl(
  "specification; text line 2",
  text_candidates$source_ref,
  fixed = TRUE
))
stopifnot(grepl("Draft extracted passage", text_candidates$rule, fixed = TRUE))

lines <- candidate_requirement_lines(text_candidates)
stopifnot(
  length(lines) == 1L,
  grepl("ADAE.ASTDT", lines, fixed = TRUE),
  grepl("spec.txt", lines, fixed = TRUE)
)

unlink(c(xml_path, text_path))
