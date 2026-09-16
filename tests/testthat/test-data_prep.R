df <- data.frame(SUBJID = c("SUBJ1", "SUBJ2"), COL1 = c(1, 2), COL2 = c("a", "b"))
attr(df[["COL1"]], "label") <- "Label 1"

test_that("pt_get_summary_data preserves labels, filling in the column name when none is set", {
  output <- pt_get_summary_data(df, subjid_var = "SUBJID", columns = c("COL1", "COL2"), selected_subjid = "SUBJ1")
  testthat::expect_false(output[["error_list"]][["any"]]())
  testthat::expect_identical(get_labels(output[["result"]]), c("Label 1", "COL2"))
})

test_that("pt_get_summary_data returns the requested columns, row count and values", {
  output <- pt_get_summary_data(df, subjid_var = "SUBJID", columns = "COL1", selected_subjid = "SUBJ1")
  testthat::expect_false(output[["error_list"]][["any"]]())
  testthat::expect_identical(names(output[["result"]]), "COL1")
  testthat::expect_identical(nrow(output[["result"]]), 1L)

  actual_value <- output[["result"]][["COL1"]]
  attr(actual_value, "label") <- NULL
  testthat::expect_identical(actual_value, df[["COL1"]][df[["SUBJID"]] == "SUBJ1"])
})

test_that("pt_get_summary_data flags an error and returns NA when selected_subjid matches no rows", {
  output <- pt_get_summary_data(df, subjid_var = "SUBJID", columns = "COL1", selected_subjid = "not-a-subject")
  testthat::expect_true(output[["error_list"]][["any"]]())
  testthat::expect_match(output[["error_list"]][["get_messages"]](), "Found 0 rows", fixed = TRUE)
  testthat::expect_true(is.na(output[["result"]]))
})

test_that("pt_get_summary_data flags an error when selected_subjid matches more than one row", {
  dup_df <- rbind(df, df[1, ])
  output <- pt_get_summary_data(dup_df, subjid_var = "SUBJID", columns = "COL1", selected_subjid = "SUBJ1")
  testthat::expect_true(output[["error_list"]][["any"]]())
  testthat::expect_true(is.na(output[["result"]]))
})

test_that("pt_get_summary_data flags an error naming a column absent from df", {
  output <- pt_get_summary_data(df, subjid_var = "SUBJID", columns = c("COL1", "NOT_A_COLUMN"), selected_subjid = "SUBJ1")
  testthat::expect_true(output[["error_list"]][["any"]]())
  testthat::expect_identical(length(output[["error_list"]][["get_messages"]]()), 1L)
  testthat::expect_match(output[["error_list"]][["get_messages"]](), "NOT_A_COLUMN", fixed = TRUE)
  testthat::expect_true(is.na(output[["result"]]))
})

test_that("pt_get_summary_data accumulates one error per problem found", {
  output <- pt_get_summary_data(
    df, subjid_var = "SUBJID", columns = c("COL1", "NOT_A_COLUMN"), selected_subjid = "not-a-subject"
  )
  testthat::expect_identical(length(output[["error_list"]][["get_messages"]]()), 2L)
  testthat::expect_true(is.na(output[["result"]]))
})

test_that("pt_get_summary_data flags an error when subjid_var is not a column of df", {
  output <- pt_get_summary_data(df, subjid_var = "NOT_A_COLUMN", columns = "COL1", selected_subjid = "SUBJ1")
  testthat::expect_true(output[["error_list"]][["any"]]())
  testthat::expect_true(is.na(output[["result"]]))
})
