test_that("pt_info_data_filter preserves data.frame labels", {
  df <- as.data.frame(safetyData::adam_adsl)
  expected_labels <- get_labels(df)
  output <- pt_info_data_filter(
    df, subjid_var = "USUBJID", columns = names(df), selected_key = "01-701-1015"
  )
  testthat::expect_identical(get_labels(output), expected_labels)
})

test_that("default vals are used for plots if any mandatory parameters are missing.", {

  plots <- dv.papo:::CONST$plot_default_vals
  plots$range_plots$`Adverse Events Plot`$dataset <- NULL
  plots$range_plots$`Adverse Events Plot`$vars <- plots$range_plots$`Adverse Events Plot`$vars[2:5] #omit start_date
  plots$value_plots$`Lab plot`$tooltip <- NULL

  plots_filled <- dv.papo:::get_missing_plot_params(plots)



})
