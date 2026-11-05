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
