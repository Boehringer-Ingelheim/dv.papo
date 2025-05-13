testthat::test_that("colour palette is completed if it misses colours for grading values", {
  # Let 'n' denote number of grading values
  n <- 26
  sample_data <- data.frame(GRADING = LETTERS[1:n])
  grading_col_pal <- structure(sample(colours(), n - 10), names = LETTERS[1:(n - 10)])
  grading_col_pal_filled <- fill_palette(sample_data[["GRADING"]], grading_col_pal)
  testthat::expect_length(grading_col_pal_filled, n + length(CONST$default_palette))
})

testthat::test_that("colour palette is filled even if pre-defined colors used up", {
  sample_data <- data.frame(GRADING = c(LETTERS[1:26], "AA", "BB", "CC"))
  grading_col_pal <- structure(
    c(
      "red", "orange", "yellow", "green", "cyan", "blue",
      "magenta", "purple", "black", "pink", "khaki",
      "turquoise", "navyblue", "violet", "yellowgreen", "skyblue",
      "indianred", "cornsilk", "chocolate", "darkgoldenrod",
      "coral", "dodgerblue", "firebrick", "forestgreen",
      "dimgrey", "gold"
    ),
    names = LETTERS[1:26]
  )
  grading_col_pal_filled <- fill_palette(sample_data[["GRADING"]], grading_col_pal)
  testthat::expect_length(grading_col_pal_filled, 29 + length(CONST$default_palette))
})
