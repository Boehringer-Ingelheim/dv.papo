ODG <- poc(
  # dv.manager pastes the markup returned by a `function(header)` of this class verbatim into the document
  CHUNK_CLASS = "chunk"
)

odg_escape_md <- function(x) {
  chars <- strsplit(enc2utf8(x), "")[[1]]
  keep <- chars %in% c(LETTERS, letters, 0:9, "-")
  chars[!keep] <- sprintf("&#%d;", vapply(chars[!keep], utf8ToInt, integer(1), USE.NAMES = FALSE))
  paste(chars, collapse = "")
}
