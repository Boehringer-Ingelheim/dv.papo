#### PREFACE STARTS

# @order 1 First block read so it can group all the other docs required just because of ODG
#
#' Functions exported for the output document generation (ODG)
#'
#' Functions exported for the output document generation (ODG)
#'
#' Internal, do not use. This document groups all functions in dv.papo that must be exported because of ODG. They are
#'  not meant to be called by apps or modules, and they can change without notice.
#'
#' @name odg_exported_functions
#'
#' @keywords internal
#' @order 1
NULL

#### PREFACE ENDS

ODG <- poc(
  # dv.manager pastes the markup returned by a `function(header)` of this class verbatim into the document
  CHUNK_CLASS = "chunk"
)

