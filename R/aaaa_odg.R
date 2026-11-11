# Aliases

sm_mr2 <- shinymeta::metaReactive2

# shinymeta::metaReactive
sm_mr <- local({
  f <- shinymeta::metaReactive
  fargs <- formals(f)
  fargs$localize <- TRUE
  formals(f) <- fargs
  f
})

# shinymeta::metaExpr
sm_me <- local({
  f <- shinymeta::metaExpr
  fargs <- formals(f)
  fargs$localize <- TRUE
  formals(f) <- fargs
  f
})

ODG <- poc(
  # dv.manager pastes the markup returned by a `function(header)` of this class verbatim into the document
  CHUNK_CLASS = "chunk"
)

# Writes `x` as inert Rmd markdown text: alphanumerics and `-` stay as they are, every other character becomes an HTML
# numeric entity, which pandoc resolves in both HTML and PDF output. Unlike backslash escapes, this leaves no literal
# backtick for knitr to run as inline code.
odg_escape_md <- function(x) {
  chars <- strsplit(enc2utf8(x), "")[[1]]
  keep <- chars %in% c(LETTERS, letters, 0:9, "-")
  chars[!keep] <- sprintf("&#%d;", vapply(chars[!keep], utf8ToInt, integer(1), USE.NAMES = FALSE))
  paste(chars, collapse = "")
}
