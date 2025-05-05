#' Filter datasets for patient information section
#'
#' @inheritParams mod_patient_profile_server
#' @param df TODO
#' @param columns TODO
#' @param selected_key TODO
#'
#' @keywords internal
#'
#' @return A list containing datasets for patient information and plots
pt_info_data_filter <- function(df, subjid_var, columns, selected_key) {
  res <- NA
  res_logical_check <- FALSE
  row_index <- which(df[[subjid_var]] == selected_key)
  if (length(row_index) == 1) {
    res <- df[row_index, columns]
    res_logical_check <- TRUE
  }

  # re-apply data labels for data.frames
  if (res_logical_check && ! inherits(df, "tbl") && inherits(df, "data.frame")) {
    df_labels <- structure(get_labels(df), names = names(df))[columns] #extract and save labels
    # re-apply saved labels
    for (i in columns) {
      attr(res[[i]], "label") <- df_labels[[i]]
    }
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
