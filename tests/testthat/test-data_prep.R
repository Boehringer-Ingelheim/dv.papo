test_that("pt_info_data_filter preserves data.frame labels", {

  df <- as.data.frame(safetyData::adam_adsl)
  mock_labels <- c(LETTERS, letters)

  for (i in seq_len(ncol(df))) {
    attr(df[[i]], "label") <- mock_labels[i]
  }

  expected_labels <- get_labels(df)

  output <- pt_info_data_filter(
    df, subjid_var = "USUBJID", columns = names(df), selected_key = "01-701-1015"
  )

  testthat::expect_identical(get_labels(output), expected_labels)

})
