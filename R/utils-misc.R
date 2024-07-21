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

robust_min <- function(...) min(..., +Inf, na.rm = TRUE) # TODO: Remove if unused
robust_max <- function(...) max(..., -Inf, na.rm = TRUE) # TODO: Remove if unused

robust_ymd <- function(data, round_up = FALSE) {
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
