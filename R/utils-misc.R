drop_columns_by_name <- function(df, col_names) {
  df[col_names] <- list(NULL)
  return(df)
}

select_columns_by_name <- function(df, col_names) {
  return(df[, col_names, drop = FALSE])
}

filter_with_mask <- function(df, mask) {
  labels <- Map(function(col) attr(col, "label"), df)
  res <- df[mask, ]
  for (name in names(labels)) attr(res[[name]], "label") <- labels[[name]]
  return(res)
}

silence_warning <- function(expr, warning_message) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (w$message != warning_message) {
        NULL
      } else {
        invokeRestart("muffleWarning")
      }
    }
  )
}

merge_with_no_duplicate_cols <- function(a, b, by) merge(a, b[c(by, setdiff(names(b), names(a)))], by)

#' Convert possibly truncated character(n) 'yyyy-mm-dd' to Date(n)
#' performing optional round up using the level of precision present
#' in the input data
#'
#' @param data [character(n)] Vector of dates
#'
#' @keywords internal
#'
robust_ymd <- function(data, round_up = FALSE) {
  # NOTE(miguel): 
  # `dv.papo` used to rely on `lubridate` for date parsing.
  # We dropped that library because we didn't need any of its many advanced features.
  # Instead, we wrote this function to parse the only format we cared about ('yyyy-mm-dd'),
  # and to round end-dates up taking into account the unit information implicit to possibly
  # truncated ('yyyy', 'yyyy-mm') input strings.
  # Using `lubridate::ceiling_date` instead results in even more code on our part, since
  # that call assumes that all input dates have a homogeneous precision, which is not the case
  # for us.
  #
  # In any case, the point is moot because we now require users of `dv.papo` to provide Date
  # objects and this function is a (non-exported) helper to deal with character dates.

  label <- attr(data, "label")

  data <- substr(data, 1, 10)
  missing_day <- !is.na(data) & nchar(data) == 7
  missing_month_day <- !is.na(data) & nchar(data) == 4
  if (round_up) { # next month, next year
    year <- as.integer(substr(data[missing_day], 1, 4))
    month <- as.integer(substr(data[missing_day], 6, 7)) + 1
    carry <- month == 13
    year[carry] <- year[carry] + 1
    month[carry] <- 1
    data[missing_day] <- sprintf("%04d-%02d", year, month)

    year <- as.integer(substr(data[missing_month_day], 1, 4)) + 1
    data[missing_month_day] <- sprintf("%04d", year)
  }

  data[missing_month_day] <- paste0(data[missing_month_day], "-01-01")
  data[missing_day] <- paste0(data[missing_day], "-01")

  data <- as.Date(data)

  if (round_up) { # one day before next month, before next year
    data[missing_day | missing_month_day] <- data[missing_day | missing_month_day] - 1
  }

  attr(data, "label") <- label

  return(data)
}
