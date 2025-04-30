test_that("pt_info_data_filter preserves data.frame labels", {

  df <- datasets::iris |>
    dplyr::group_by(Species) |>
    dplyr::slice(1) |>
    as.data.frame()

  for (i in seq_len(ncol(df))) {
    attr(df[[i]], "label") <- LETTERS[i]
  }

  expected_labels <- get_labels(df)

  output <- pt_info_data_filter(
    df, subjid_var = "Species", columns = names(df), selected_key = "setosa"
  )

  testthat::expect_identical(get_labels(output), expected_labels)

})
