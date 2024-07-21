pkg_name <- "dv.papo"

library(testthat)
library(pkg_name, character.only = TRUE)

test_check(pkg_name)
