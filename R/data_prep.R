#' Gets subject-level summary data
#'
#' @inheritParams mod_patient_profile_server
#' @param df `[data.frame]` Subject-level dataset to filter. Must contain `subjid_var` and `columns`.
#' @param columns `[character(n)]` Names of the columns of `df` to keep in the result.
#' @param selected_subjid `[character(1)]` Value of `subjid_var` identifying the patient to keep. Must match
#'   exactly one row of `df[[subjid_var]]`.
#'
#' @keywords internal
#'
#' @return A list with:
#' \itemize{
#'   \item{`result`}: a one-row data frame restricted to `columns`, with labels preserved, or `NA` if any error
#'     was collected.
#'   \item{`error_list`}: an error list (see `new_error_list()`)
#' }

pt_get_summary_data <- function(df, subjid_var, columns, selected_subjid) {
  
  res <- list(
    result = NA,
    error_list = new_error_list()
  )

  local({
    check <- checkmate::check_data_frame(df, min.rows = 1)
    if(!isTRUE(check)) {
      res[["error_list"]][["push"]](check)
    }
  })

  local({
    check <- checkmate::check_subset(c(subjid_var, columns), names(df))
    if(!isTRUE(check)) {
      res[["error_list"]][["push"]](check)
    }
  })
  
  row_index <- which(df[[subjid_var]] == selected_subjid)

  if (length(row_index) != 1) {
    msg <- sprintf(
      "Found %d rows df[[%s]]==%s. Number of rows must be equal to 1.",
      length(row_index),
      deparse(subjid_var),
      deparse(selected_subjid)
    )
    res[["error_list"]][["push"]](msg)
  }

  if (!res[["error_list"]][["any"]]()) {        
    res[["result"]] <- set_lbls(
      df[row_index, columns, drop = FALSE],
      get_lbls_robust(df)[columns]
    )
  }
  
  return(res)
}

#' This function will return corresponding labels in a dataset.
#' @param dataset Dataset to get labels from.
#' @param col_names Vector of character names of the columns in the dataset.
#' @param keep_as_original Logical TRUE or FALSE: If TRUE, the corresponding column with
#' no label will return column name, else return NA.
#' If not assigned, it will return all labels of the the dataset. Otherwise,
#' only labels of  corresponding columns will be returned.
#'
#' @keywords internal
#'
#' @return Vector containing the labels of the dataset
get_labels <- function(dataset, col_names = NULL, keep_as_original = FALSE) {
  if (is.null(col_names)) {
    col_names <- names(dataset)
  }
  out <- sapply(col_names, function(name) {
    if (name %in% names(dataset)) {
      label <- attributes(dataset[[name]])$label
      if (is.null(label)) {
        ifelse(keep_as_original, name, NA)
      } else {
        label
      }
    } else {
      stop(paste0(
        "Column '", name, "' NOT in Dataset: '",
        deparse(substitute(dataset)), "'!"
      ))
    }
  })

  out <- unname(out)
  return(out)
}

flag_columns_shinyvalidate <- function(cols, pre = "", post = "") {
  shiny::validate(
    shiny::need(
      length(cols) == 0,
      message = paste(pre, paste0("`", cols, "`", collapse = ", "), post)
    )
  )
}

flag_columns_capture_error <- function(cols, pre = "", post = "") {
  res <- NULL
  if (length(cols)) {
    res <- paste(pre, paste0("`", cols, "`", collapse = ", "), post)
  }
  return(res)
}

ensure_columns_exist <- function(df, cols, flag_column_function = flag_columns_shinyvalidate) {
  checkmate::assert_character(cols)
  excess_columns <- setdiff(cols, names(df))

  return(flag_column_function(
    excess_columns,
    pre = paste(
      "dv.papo: Can't find user defined column(s) in the dataset.",
      "You've tried to set the following column(s):"
    ),
    post = "Have you spelled their names correctly?"
  ))
}

ensure_columns_are_dates_or_datetimes <- function(df, cols, flag_column_function = flag_columns_shinyvalidate) {
  checkmate::assert_character(cols, null.ok = TRUE)
  mask <- sapply(df[cols], function(x) inherits(x, c("Date", "POSIXt")))
  incorrect_cols <- cols[!mask]
  return(flag_column_function(
    incorrect_cols,
    pre = paste(
      "dv.papo: Column(s) holding dates must be of type `Date` or `POSIXt`.",
      "The following column(s) are not of those types:"
    ),
    post = r"----(Type "vignette("lubridate")" into your console to learn more about time types.)----"
  ))
}

ensure_columns_are_numeric <- function(df, cols, flag_column_function = flag_columns_shinyvalidate) {
  checkmate::assert_character(cols, null.ok = TRUE)
  date_mask <- sapply(df[cols], is.numeric)
  incorrect_cols <- cols[!date_mask]
  return(flag_column_function(
    incorrect_cols,
    pre = paste(
      "dv.papo: Column(s) holding days/values must be of type numeric",
      "The following column(s) are not of type numeric:"
    )
  ))
}
