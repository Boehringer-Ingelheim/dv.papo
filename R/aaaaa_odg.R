
..activate_odg <- function() {
  if (requireNamespace("shinymeta", quietly = TRUE)) {
    log_warn("Output documentation generation is under development")

    # These set of functions is declared inside to avoid calling shinymeta out of a function.
    # Doing that would require some extra if statements that would worsen code readability.

    ODGA <- list()

    # shinymeta::metaReactive2
    ODGA[["sm_mr2"]] <- shinymeta::metaReactive2

    # shinymeta::metaReactive
    ODGA[["sm_mr"]] <- local({
      f <- shinymeta::metaReactive
      fargs <- formals(f)
      fargs$localize <- TRUE
      formals(f) <- fargs
      f
    })

    # shinymeta::metaExpr
    ODGA[["sm_me"]] <- local({
      f <- shinymeta::metaExpr
      fargs <- formals(f)
      fargs$localize <- TRUE
      formals(f) <- fargs
      f
    })

    ODGE[["A"]] <- ODGA
  } else {
    log_warn(
      "`shinymeta` package is required to activate output documentation generation"
    )
    ODGE[["A"]] <- NODGA
  }
  invisible(NULL)
}

..deactivate_odg <- function() {
  ODGE[["A"]] <- NODGA
}
