utils_batcher <- local({
  latex_header_repeat <- function(x) {
    attrs <- attributes(x)
    txt <- as.character(x)
    stopifnot(length(txt) == 1) # only single latex output, avoid other gt objects
    if (!grepl("\\\\endhead", txt)) {
      # the midrule line has trailing spacing in some gt versions
      txt <- sub("(\\\\midrule[^\n]*\n)", "\\1\\\\endhead\n", txt)
    }
    # as.character() drops knit_meta (gt's latex_dependency list), so knitr
    # emits no \usepackage lines -- fails with "Environment longtable undefined"
    attributes(txt) <- attrs
    class(txt) <- "knit_asis"
    txt
  }

  # gt escapes any backslash in cols_label(), and LaTeX cells can't have real
  # line breaks anyway -- so headers (name over label) are built as nested
  # tabulars, marked with control chars here and expanded after as_latex() runs.
  HDR_OPEN <- "\x1e" # ASCII record separator: wraps header
  HDR_BRK <- "\x1d" # ASCII group separator: name[HDR_BRK](label)
  PEN_BRK <- HDR_BRK

  header_labels <- function(data, columns = names(data)) {
    labels <- dv.papo:::get_labels(data, columns)
    labels[is.na(labels)] <- "No Label"
    align <- ifelse(
      vapply(columns, function(cl) is.numeric(data[[cl]]), logical(1)),
      "r",
      "l"
    )
    stats::setNames(
      paste0(HDR_OPEN, align, columns, HDR_BRK, "(", labels, ")", HDR_OPEN),
      columns
    )
  }

  latex_break_headers <- function(x) {
    attrs <- attributes(x)
    txt <- gsub(
      paste0(HDR_OPEN, "(.)(.*?)", HDR_BRK, "(.*?)", HDR_OPEN),
      "\\\\begin{tabular}[b]{@{}\\1@{}} \\\\textbf{\\2} \\\\\\\\ \\3 \\\\end{tabular}",
      as.character(x),
      perl = TRUE
    )
    # Check those marker do not appear in the body
    stopifnot(!grepl(paste0("[", HDR_OPEN, HDR_BRK, "]"), txt))
    attributes(txt) <- attrs
    txt
  }

  
  USE_TABLE_ESTIMATION_WIDTH <- 0L

  # Listing text cells longer than CELL_LINE_CHARS wrap at approximately that many characters,
  # Latex rules dictate the breaking in words, lines, etc. So each line will have at most those characters but
  # usually less. See `\longcell` in header.tex.
  # Those longer than MAX_CELL_CHARS are also cut and linked to a note, below the table, that holds the whole text.
  # This limit is strict as it does not depend on latex rules, but is a still replacement in R.
  CELL_LINE_CHARS <- 50L
  MAX_CELL_CHARS <- 200L

  # PDF text string, so any text can go in a PDF dictionary untouched by LaTeX
  pdf_hex_string <- function(x) {
    vapply(
      x,
      function(s) {
        bytes <- iconv(enc2utf8(s), from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)[[1]]
        paste0("<FEFF", paste(as.character(bytes), collapse = ""), ">")
      },
      character(1),
      USE.NAMES = FALSE
    )
  }

  
  add_penalty_latex_text <- function(x) {
    # Places a placeholder between the characters of each word that is a candidate for breaking
    x <- gsub("(?<=\\S)(?=\\S)", PEN_BRK, x, perl = TRUE)
    x <- gt::escape_latex(x)
    x <- gsub(PEN_BRK, "\\penalty50\\relax{}", x, fixed = TRUE)
    x <- gsub("\n", "\\newline{}", x, fixed = TRUE)    
  }

  # Replaces the long text cells of `body` by their LaTeX, flagged in `is_latex`. The cut ones are numbered in reading
  # order and their `notes` follow the table. `id` must be unique in the document, it prefixes the link targets.
  latex_long_cells <- function(body, id) {
    is_text <- vapply(body, function(col) is.character(col) || is.factor(col), logical(1))
    body[is_text] <- lapply(body[is_text], as.character)

    # Long cells will be latex
    is_latex <- matrix(FALSE, nrow(body), ncol(body))
    for (col in which(is_text)) is_latex[, col] <- !is.na(body[[col]]) & nchar(body[[col]]) > CELL_LINE_CHARS    
    cells <- which(is_latex, arr.ind = TRUE)

    # To reading order
    cells <- cells[order(cells[, "row"], cells[, "col"]), , drop = FALSE]
    
    full_text <- character(0)
    for (i in seq_len(nrow(cells))) {
      row <- cells[[i, "row"]]
      col <- cells[[i, "col"]]
      text <- body[[col]][[row]]
      cell <- add_penalty_latex_text(text)
      # Is not the same > CELL_LINE_CHARS which define a long cell, than > MAX_CELL_CHARS which define a note
      if (nchar(text) > MAX_CELL_CHARS) {
        full_text <- c(full_text, text)
        k <- length(full_text)
        mark <- sprintf("[%d]", k)
        # The ellipsis and the mark count towards MAX_CELL_CHARS, else they spill onto a line of their own. No trailing
        # blank, it would let them break away from the text
        abridged_text <- trimws(substr(text, 1, MAX_CELL_CHARS - 1L - nchar(mark)), which = "right")
        # The note links back to the start of the cell
        cell <- sprintf(
          "\\longcellanchor{%s-cell-%d}%s\u2026\\longcellmark{%s-note-%d}{%s}{%s}",
          id,
          k,
          add_penalty_latex_text(abridged_text),
          id,
          k,
          mark,
          pdf_hex_string(text)
        )
      }
      body[[col]][[row]] <- sprintf("\\longcell{%d}{%s}", CELL_LINE_CHARS, cell)
    }

    notes <- character(0)
    # Tooltip notes depend on the visualizer, Adobe, Okular support them, usually browsers do not. Nothing we can do on
    # our side
    if (length(full_text) > 0) {
      k <- seq_along(full_text)
      # In an environment so internals do not affect other groups
      notes <- paste0(
        "\\begin{longcellnotes}\n",
        paste(
          sprintf("\\longcellnote{%s-note-%d}{%s-cell-%d}{[%d]}{%s}", id, k, id, k, k, add_penalty_latex_text(full_text)),
          collapse = "\n"
        ),
        "\n\\end{longcellnotes}\n"
      )
    }

    list(body = body, is_latex = is_latex, notes = notes)
  }

  # `long_cell_id`: when given, long text cells are wrapped, cut and linked to notes, see `latex_long_cells()`
  gt_latex_table <- function(data, columns = names(data), long_cell_id = NULL) {
    body <- as.data.frame(data)[, columns, drop = FALSE]
    # gt latex output prints dates and POSIX as numbers instead of formatting them
    # until fixed we do it by ourselves
    is_date <- vapply(body, function(col) inherits(col, c("Date", "POSIXt")), logical(1))
    body[is_date] <- lapply(body[is_date], format)
    is_latex <- matrix(FALSE, nrow(body), ncol(body))
    notes <- character(0)
    if (!is.null(long_cell_id)) {
      long <- latex_long_cells(body, long_cell_id)
      body <- long[["body"]]
      is_latex <- long[["is_latex"]]
      notes <- long[["notes"]]
    }
    tbl <- gt::gt(body)
    for (col in which(colSums(is_latex) > 0)) {
      tbl <- gt::fmt_passthrough(tbl, columns = col, rows = which(is_latex[, col]), escape = FALSE)
    }
    tbl <- gt::cols_label(tbl, .list = as.list(header_labels(data, columns)))
    tbl <- gt::tab_options(
      tbl,
      latex.use_longtable = TRUE,
      table.font.size = gt::px(9)
    )
    tbl <- latex_header_repeat(latex_break_headers(gt::as_latex(tbl)))
    if (length(notes) > 0) {
      attrs <- attributes(tbl)
      tbl <- paste0(tbl, "\n\n", notes)
      attributes(tbl) <- attrs
    }
    tbl
  }

  poc(
    gt_latex_table = gt_latex_table,
    USE_TABLE_ESTIMATION_WIDTH = USE_TABLE_ESTIMATION_WIDTH,
    CELL_LINE_CHARS = CELL_LINE_CHARS,
    MAX_CELL_CHARS = MAX_CELL_CHARS
  )
})
