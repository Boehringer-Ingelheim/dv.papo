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

#' Get missing mandatory plot list items
#'
#' @param plots `[list(1+)]` named list containing plot configuration.
#' @return `[list(1+)]`
#'
get_missing_plot_params <- function(plots) {

  m <- CONST$plot_mandatory_params    # let 'm' denote mandatory params
  v <- CONST$plot_default_vals        # let 'v' denote default vals


  # 1. check timeline_info
  if (! rlang::has_name(plots, "timeline_info")) {
    plots[["timeline_info"]] <- v[["timeline_info"]]
  } else {
    for (item in m[["timeline_info"]]) {
      if (! rlang::has_name(plots[["timeline_info"]], item)) {
        plots[["timeline_info"]][[item]] <- v[["timeline_info"]][[item]]
      }
    }
  }

  # 2. check vline_vars
  if (! rlang::has_name(plots, "vline_vars")) {
    plots[["vline_vars"]] <- v[["vline_vars"]]
  }

  # 3. check plots

  plot_type_info <- list(
    range_plots = list(
      ae = list(regexp = "adverse events", v_plot_name = "Adverse Events Plot"),
      cm = list(regexp = "concomitant medication", v_plot_name = "Concomitant Medication Plot")
    ),
    value_plots = list(
      lab = list(regexp = "lab plot", dv_plot_name = "Lab plot"),
      vital = list(regexp = "vital sign", v_plot_name = "Vital Sign Plot")
    )
  )

  plot_vars <- list(range_plots = m[["range_plots"]][[1]],
                    value_plots = m[["value_plots"]][[1]])

  # x <- c("range_plots", "value_plots")
  # plot_type = x[2]
  # p = names(plot_type_info[[plot_type]])[1]

  for (plot_type in c("range_plots", "value_plots")) {
    if (! rlang::has_name(plots, plot_type)) {
      plots[[plot_type]] <- v[[plot_type]]
    } else {

      for (p in names(plot_type_info[[plot_type]])) {
        plot_details <- plot_type_info[[plot_type]][[p]]
        if (any(grepl(plot_details[["regexp"]], names(plots[[plot_type]]), ignore.case = TRUE))) {

          plot_name <- grep(plot_details[["regexp"]], names(plots[[plot_type]]), ignore.case = TRUE, value = TRUE)

          for (i in plot_name) {
            #i = plot_name
            print(i)
            for (j in names(plot_vars[[plot_type]])) {
              #j = "vars"
              print(j)
              if (j == "vars") {

              missing_vars <- setdiff(plot_vars[[plot_type]][[j]],
                                      plots[[plot_type]][[i]][[j]] |> names())

              if (length(missing_vars) > 0) {
                plots[[plot_type]][[i]][[j]] <- c(
                  v[[plot_type]][[i]][[j]][missing_vars],
                  plots[[plot_type]][[i]][[j]]
                )

              }
                # for (var_col in m[[plot_type]][[plot_details$v_plot_name]][[j]]) {
                #   print(var_col)
                #   if (! rlang::has_name(plots[[plot_type]][[i]][[j]], var_col)) {
                #     plots[[plot_type]][[i]][[j]][[var_col]] <-
                #       v[[plot_type]][[plot_details$v_plot_name]][[j]][[var_col]]
                #   }
                # }
              } else {
                if (! rlang::has_name(plots[[plot_type]][[i]], j)) {
                  plots[[plot_type]][[i]][[j]] <- v[[plot_type]][[plot_details$v_plot_name]][[j]]
                }
              }
            }
          }


        }

      }

    }
  }

  #   if (! rlang::has_name(plots, "range_plots")) {
  #   plots[["range_plots"]] <- v[["range_plots"]]
  # } else {
  #   range_plot_params <- m[["range_plots"]][[1]] # params are the same for AE, CM plots
  #
  #   # i. Adverse Event plot checks
  #   if (any(grepl("adverse event", names(plots[["range_plots"]]), ignore.case = TRUE))) {
  #     plot_name <- grep(
  #       "adverse event", names(plots[["range_plots"]]), ignore.case = TRUE, value = TRUE
  #     )
  #     for (i in plot_name) {
  #       for (j in names(range_plot_params)) {
  #         if (j %in% c("vars")) {
  #           for (var_col in m[["range_plots"]][["Adverse Events Plot"]][[j]]) {
  #             if (! rlang::has_name(plots[["range_plots"]][[i]][[j]], var_col)) {
  #               plots[["range_plots"]][[i]][[j]][[var_col]] <-
  #                 v[["range_plots"]][["Adverse Events Plot"]][[j]][[var_col]]
  #             }
  #           }
  #         } else {
  #           if (! rlang::has_name(plots[["range_plots"]][[i]], j)) {
  #             plots[["range_plots"]][[i]][[j]] <- v[["range_plots"]][["Adverse Events Plot"]][[j]]
  #           }
  #         }
  #       }
  #     }
  #   }
  #
  #   # ii. Concomitant Medication Plot checks
  #   if (any(grepl("concomitant medication", names(plots[["range_plots"]]), ignore.case = TRUE))) {
  #     plot_name <- grep(
  #       "concomitant medication", names(plots[["range_plots"]]), ignore.case = TRUE, value = TRUE
  #     )
  #     for (i in plot_name) {
  #       for (j in names(range_plot_params)) {
  #         if (j %in% c("vars")) {
  #           for (var_col in m[["range_plots"]][["Concomitant Medication Plot"]][[j]]) {
  #             if (! rlang::has_name(plots[["range_plots"]][[i]][[j]], var_col)) {
  #               plots[["range_plots"]][[i]][[j]][[var_col]] <-
  #                 v[["range_plots"]][["Concomitant Medication Plot"]][[j]][[var_col]]
  #             }
  #           }
  #         } else {
  #           if (! rlang::has_name(plots[["range_plots"]][[i]], j)) {
  #             plots[["range_plots"]][[i]][[j]] <- v[["range_plots"]][["Concomitant Medication Plot"]][[j]]
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  #

  #check value_plots
return(plots)

}
#' Apply default vals to plot list.
#'
#' @param plots `[list(1+)]` named list containing plot configuration.
#'
#' @return `[list(1+)]`
#' @keywords internal
apply_plot_default_vals <- function(plots) {

}
