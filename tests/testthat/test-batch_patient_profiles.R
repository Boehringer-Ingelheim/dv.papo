# Subjects present in `batch_test_data()`
known <- c("01-701-1015", "01-701-1023")

batch_config <- list(
  subject_level_dataset_name = "adsl",
  subjid_var = "USUBJID",
  summary = list(vars = c("SITEID", "AGE", "SEX"), column_count = 3),
  listings = list(
    "Adverse Event" = list(dataset = "adae", default_vars = c("AEDECOD", "AESEV", "ASTDT")),
    "Concomitant Medication" = list(dataset = "cm")
  ),
  plots = list(
    timeline_info = c(
      trt_start_date = "TRTSDT", trt_end_date = "TRTEDT",
      icf_date = "RFICDT", part_end_date = "RFENDT"
    ),
    vline_vars = c("Informed Consent Day" = "RFICDT"),
    range_plots = list(
      "Adverse Events Plot" = list(
        dataset = "adae",
        vars = c(
          start_date = "ASTDT", end_date = "AENDT", decode = "AEDECOD",
          grading = "AESEV", serious_ae = "AESER"
        )
      )
    ),
    value_plots = list(
      "Lab plot" = list(
        dataset = "lb",
        vars = c(
          analysis_param = "PARAM", analysis_val = "AVAL",
          analysis_date = "ADT", analysis_indicator = "ANRIND"
        ),
        default_analysis_params = "Alkaline Phosphatase (U/L)"
      )
    )
  )
)

batch_test_data <- local({
  cached <- NULL
  function() {
    testthat::skip_if_not_installed("safetyData")
    testthat::skip_if_not_installed("dplyr")
    if (is.null(cached)) cached <<- prep_safety_data(5)
    return(cached)
  }
})

# Provenance is left out by default to keep the call off `digest` and `devtools`, and to keep renders short
call_batcher <- function(subjid_vals, output_dir, config = batch_config, include_provenance = FALSE, ...) {
  testthat::skip_if_not_installed("gt")
  testthat::skip_if_not_installed("cowplot")
  do.call(batch_patient_profiles, c(
    list(
      subjid_vals = subjid_vals, dataset_list = batch_test_data(), output_dir = output_dir,
      include_provenance = include_provenance
    ),
    config,
    list(...)
  ))
}

# Rendering needs LaTeX, reading the PDF back needs pdftotext
skip_if_cannot_render <- function(read_back = FALSE) {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("xelatex")), "no xelatex installation")
  skip_if_not(rmarkdown::pandoc_available(), "no pandoc installation")
  if (read_back) skip_if_not(nzchar(Sys.which("pdftotext")), "no pdftotext installation")
}

pdf_text <- function(path) paste(system2("pdftotext", c(shQuote(path), "-"), stdout = TRUE), collapse = "\n")

test_that(vdoc[["add_spec"]](
  "batch_patient_profiles() stops before writing anything on invalid input",
  c(specs$batch$input_checks)
), {
  bad_config <- batch_config
  bad_config[["summary"]][["vars"]] <- "NOT_A_COLUMN"
  output_dir <- tempfile()

  expect_error(call_batcher(c(profile = known[[1]]), output_dir, config = bad_config), "invalid configuration")
  expect_error(call_batcher(known, output_dir), "subjid_vals", fixed = TRUE)
  expect_error(call_batcher(c(a = known[[1]], a = known[[2]]), output_dir), "duplicated names")
  expect_error(call_batcher(c("a/b" = known[[1]], "a:b" = known[[2]]), output_dir), "same output file name")
  expect_false(dir.exists(output_dir))

  dir.create(output_dir)
  existing <- file.path(output_dir, "profile.pdf")
  writeLines("previous run", existing)
  expect_error(call_batcher(c(profile = known[[1]]), output_dir), "Refusing to overwrite")
  expect_identical(list.files(output_dir), "profile.pdf")
  expect_identical(readLines(existing), "previous run")
})

test_that(vdoc[["add_spec"]](
  "batch_patient_profiles() replaces a PDF with an unknown subject by an error report",
  c(specs$batch$failure_report)
), {
  output_dir <- tempfile()
  messages <- capture_messages(
    res <- call_batcher(list(group = c(known, "NOT-A-SUBJECT")), output_dir, verbosity = 0)
  )
  manifest <- res[["manifest"]]

  expect_match(messages, "group: FAILED", fixed = TRUE, all = FALSE) # reported even with verbosity 0
  expect_setequal(list.files(output_dir), c("group.txt", "manifest.csv"))
  expect_true(manifest[["error"]] %in% readLines(file.path(output_dir, "group.txt")))
  expect_equal(manifest[c("entry", "file", "ok")], data.frame(entry = "group", file = "group.txt", ok = FALSE))
  expect_equal(utils::read.csv(file.path(output_dir, "manifest.csv")), manifest)
})

test_that(vdoc[["add_spec"]](
  "batch_patient_profiles() renders one PDF per entry and carries on past a failed one",
  c(specs$batch$pdf_per_element, specs$batch$failure_report)
), {
  skip_if_cannot_render()

  output_dir <- tempfile()
  res <- suppressMessages(call_batcher(list(failed = "NOT-A-SUBJECT", both = known), output_dir))

  expect_identical(res[["manifest"]][["ok"]], c(FALSE, TRUE))
  expect_setequal(list.files(output_dir), c("failed.txt", "both.pdf", "manifest.csv"))
})

test_that(vdoc[["add_spec"]](
  "batch_patient_profiles() leaves out the summary, listings and plots that are not configured",
  c(specs$batch$optional_sections)
), {
  skip_if_cannot_render(read_back = TRUE)
  headings <- c(summary = "Patient Summary", listings = "Listings", plots = "Patient Plots")

  for (section in names(headings)) {
    config <- batch_config
    config[setdiff(names(headings), section)] <- list(NULL)
    output_dir <- tempfile()
    suppressMessages(call_batcher(c(profile = known[[1]]), output_dir, config = config))
    text <- pdf_text(file.path(output_dir, "profile.pdf"))

    expect_match(text, headings[[section]], fixed = TRUE, info = section)
    for (omitted in headings[names(headings) != section]) expect_no_match(text, omitted, fixed = TRUE, info = section)
    # The subject heading moves to the first section present, without an extra "Empty Participant" one
    expect_match(text, paste("Patient Profile", known[[1]]), fixed = TRUE, info = section)
    expect_no_match(text, "Empty Participant", fixed = TRUE, info = section)
  }
})

test_that(vdoc[["add_spec"]](
  "batch_patient_profiles() appends the data source, session info and warnings to each PDF",
  c(specs$batch$provenance)
), {
  skip_if_cannot_render(read_back = TRUE)
  skip_if_not_installed("digest")
  skip_if_not_installed("devtools")

  config <- batch_config
  config[c("listings", "plots")] <- list(NULL)
  output_dir <- tempfile()
  suppressMessages(call_batcher(c(profile = known[[1]]), output_dir, config = config, include_provenance = TRUE))
  text <- pdf_text(file.path(output_dir, "profile.pdf"))

  for (heading in c("Data source", "Session Info", "Warnings & Messages")) expect_match(text, heading, fixed = TRUE)
  expect_match(text, digest::digest(batch_test_data()[["adsl"]]), fixed = TRUE)
})

test_that(vdoc[["add_spec"]](
  "Long listing cells are wrapped, cut and linked to a note with the whole text",
  c(specs$batch$long_cells)
), {
  testthat::skip_if_not_installed("gt")
  line_chars <- utils_batcher$CELL_LINE_CHARS
  max_chars <- utils_batcher$MAX_CELL_CHARS
  long_word <- strrep("a", max_chars + 1)
  long_prose <- paste(rep("50% {x}", max_chars), collapse = " ")
  data <- data.frame(
    x = c(long_word, strrep("b", max_chars), NA, long_prose),
    y = factor(c(strrep("c", line_chars), "short", long_word, "short")),
    n = 1:4
  )

  txt <- as.character(utils_batcher$gt_latex_table(data, long_cell_id = "doc-1"))
  anchors <- regmatches(txt, gregexpr("\\\\longcellanchor\\{[^}]*\\}", txt))[[1]]
  marks <- regmatches(txt, gregexpr("\\\\longcellmark\\{[^}]*\\}", txt))[[1]]
  notes <- regmatches(txt, gregexpr("\\\\longcellnote\\{[^}]*\\}\\{[^}]*\\}", txt))[[1]]

  # Only cells over `line_chars` wrap, escaped and breakable between any two non-blank characters
  breakable <- function(x) paste(x, collapse = "\\penalty50\\relax{}")
  wrap <- sprintf("\\longcell{%d}{", line_chars)
  expect_length(gregexpr(wrap, txt, fixed = TRUE)[[1]], 4)
  expect_match(txt, paste0(wrap, breakable(rep("b", max_chars)), "}"), fixed = TRUE)
  expect_match(
    txt,
    paste0("\\longcellanchor{doc-1-cell-3}", breakable(c("5", "0", "\\%")), " ", breakable(c("\\{", "x", "\\}")), " 5"),
    fixed = TRUE
  )

  # Only cells over `max_chars` are cut, in reading order, with ids prefixed by `long_cell_id` linking to each other
  k <- 1:3
  expect_identical(anchors, sprintf("\\longcellanchor{doc-1-cell-%d}", k))
  expect_identical(marks, sprintf("\\longcellmark{doc-1-note-%d}", k))
  expect_identical(notes, sprintf("\\longcellnote{doc-1-note-%d}{doc-1-cell-%d}", k, k))
  # Row by row: note 3 is the one in row 4, not the one in column `y`
  expect_match(txt, "\\longcellnote{doc-1-note-3}{doc-1-cell-3}{[3]}{5", fixed = TRUE)
  # The cell keeps as many characters as fit in `max_chars` with "\u2026[1]" and the link back targets its start
  expect_match(
    txt,
    paste0(
      wrap, "\\longcellanchor{doc-1-cell-1}", breakable(rep("a", max_chars - 4)),
      "\u2026\\longcellmark{doc-1-note-1}{[1]}"
    ),
    fixed = TRUE
  )
  # The tooltip holds the whole text
  expect_match(txt, paste0("{<FEFF", strrep("0061", max_chars + 1), ">}"), fixed = TRUE)

  # Off without `long_cell_id`
  expect_no_match(as.character(utils_batcher$gt_latex_table(data)), "longcell", fixed = TRUE)
})
