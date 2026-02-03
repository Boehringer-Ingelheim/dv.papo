#' Function to create AE or CM plot
#'
#' @param data Data frame containing the data for the plot
#' @param limits Vector that contains the limits of the plot
#' @param palette Named vector that contains the colors that are used in the plot
#' @param plot_name Name of plot
#' @param annotate_x_axis Logical indicating whether to annotate the x-axis
#'
#' @keywords internal
#'
#' @return A ggplot2 object
create_ae_cm_plot <- function(data, x_limits, palette, sl_info, vline_vars, vline_day_numbers,
                              x_axis_unit, x_axis_breaks, ref_date, plot_name, annotate_x_axis) {

  # Set column for title banner
  data[["title_banner"]] <- plot_name

  # NOTE(miguel): The following song and dance courtesy of plotly::layout not supporting dates on axes

  half_day_offset <- 0.5 # Makes it possible to see events that start and end on the same day

  # column names that end with '_z' are days that represent ref_date as zero (unlike CDISC)
  data[["start_day_z"]] <- as.numeric(data[["start_date"]] - ref_date)
  data[["end_day_z"]] <- as.numeric(data[["end_date"]] - ref_date) + half_day_offset
  data[["arrow_left_z"]] <- as.numeric(data[["arrow_left"]] - ref_date)
  data[["arrow_right_z"]] <- as.numeric(data[["arrow_right"]] - ref_date)
  x_limits_z <- x_limits - ref_date

  grading_available <- "grading" %in% names(data)
  grading <- "<no grading>"
  if (grading_available) grading <- data[["grading"]]

  # fix for AE/CM y-axis getting squashed:
  blank_decode_indexes <- which(trimws(data[["decode"]]) == "")
  data[["decode"]][blank_decode_indexes] <- htmltools::HTML("<b><i>undefined</i></b>")

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["start_day_z"]], y = .data[["decode"]])
  )

  plot <- plot + ggplot2::theme_bw(
    base_family = "Liberation Sans",
    base_size = 9
  )

  # Define a plus/minus amount to calculate the ymin and ymax values of the rectangles around their center point
  y_offset <- 0.1

  plot <- plot + ggiraph::geom_rect_interactive(
    ggplot2::aes(
      xmin = .data[["start_day_z"]],
      xmax = .data[["end_day_z"]],
      ymin = as.numeric(as.factor(.data[["decode"]])) - y_offset,
      ymax = as.numeric(as.factor(.data[["decode"]])) + y_offset,
      fill = grading,
      tooltip = .data[["tooltip"]]
    )
  )

  if ("serious_ae" %in% names(data)) {
    sae_labels <- ifelse(data[["serious_ae"]], "SAE", "")
    t_diff <- as.numeric(x_limits[2] - x_limits[1])
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["start_day_z"]],
        y = .data[["decode"]],
        label = sae_labels
      ),
      colour = "red", nudge_y = 0.25, nudge_x = 0.01 * t_diff, size = 3
    )
  }

  # Events starting and ending on the same day have short bars so the following
  # code allows hover text at the beginning of the range through transparent points
  plot <- plot + ggiraph::geom_point_interactive(
    ggplot2::aes(
      x = .data[["start_day_z"]],
      y = .data[["decode"]],
      color = grading,
      tooltip = .data[["tooltip"]]
    ),
    size = 5, alpha = 0, show.legend = FALSE
  )

  plot <- plot + ggplot2::scale_colour_manual(name = "Legend", values = palette)
  plot <- plot + ggplot2::scale_fill_manual(name = "Legend", values = palette)

  arrow_vjust <- function() {
    if (utils::packageVersion("ggplot2") < "4.0.0") 0.38 else 0.25
  }

  if (any(!is.na(data[["arrow_left"]]))) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["arrow_left_z"]] - 0.5,
        y = .data[["decode"]],
        label = "\u2190",
        color = grading
      ),
      vjust = arrow_vjust(), # Manual nudge upwards to optical centre
      size = 10,
      show.legend = FALSE
    )
  }

  if (any(!is.na(data[["arrow_right"]]))) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["arrow_right_z"]] + 0.7,
        y = .data[["decode"]],
        label = "\u2192",
        color = grading
      ),
      vjust = arrow_vjust(), # Manual nudge upwards to optical centre
      size = 10,
      show.legend = FALSE
    )
  }

  plot <- plot + ggplot2::facet_wrap(
    ggplot2::vars(.data[["title_banner"]]),
    ncol = 1,
    scales = "free_y"
  )

  plot <- plot + ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 7),
    strip.text = ggplot2::element_text(size = 10, hjust = 0) # banner text size
  )

  if (!annotate_x_axis) {
    plot <- plot + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.length.x = ggplot2::unit(0, "pt")
    )
  }

  as_CDISC_days <- function(days) days + (days >= 0)

  if (x_axis_unit == CONST$PLOT_X_AXIS_UNITS$DAYS) {
    if (length(x_axis_breaks) == 1) {
      breaks <- base::pretty(x_limits_z, n = x_axis_breaks)
    } else {
      breaks <- x_axis_breaks
    }

    lbl_fn <- function(days) {
      dates <- ref_date + days
      days <- as_CDISC_days(days)
      sprintf("%s\nDay %s", dates, days)
    }
  } else if (x_axis_unit == CONST$PLOT_X_AXIS_UNITS$WEEKS) {
    if (length(x_axis_breaks) == 1) {
      # Calculates the breaks in weeks and move them back to days, round in case we incurr in a numerical error
      breaks <- round(base::pretty(x_limits_z / 7, n = x_axis_breaks) * 7)
      # Only return values within limits otherwise the appear in the labels as NA
      breaks <- breaks[breaks >= x_limits_z[[1]] & breaks <= x_limits_z[[2]]]
    } else {
      breaks <- x_axis_breaks * 7
    }

    lbl_fn <- function(days) {
      dates <- ref_date + days
      days_z <- days
      days <- as_CDISC_days(days)
      labels <- vector(mode = "character", length = length(days))

      for (idx in seq_along(days)) {
        day <- days[[idx]]
        day_z <- days_z[[idx]]
        date <- dates[[idx]]
        if (day_z == 0) {
          labels[[idx]] <- sprintf("%s\nDay %s", date, day)
        } else {
          labels[[idx]] <- sprintf("%s\nWeek %s", date, day_z / 7)
        }
      }
      return(labels)
    }
  } else {
    stop("Unknown x_axis_unit")
  }

  plot <- plot + ggplot2::scale_x_continuous(
    labels = lbl_fn,
    breaks = breaks,
    limits = as.numeric(x_limits_z),
    expand = c(0, 0)
  )

  plot <- create_vlines(plot, sl_info, vline_vars, vline_day_numbers)

  return(plot)
}

#' Function to create lab or vital sign plot
#'
#' @param data Data frame containing the data for the plot
#' @param lb_selected_params Vector containing the values of the selected parameters
#' @param day Name of the variable that contains the analyze day
#' @param val Name of the variable that contains analyze value
#' @param low_limit Name of the variable that contains the values of the low limit of the normal range
#' @param high_limit Name of the variable that contains the values of the high limit of the normal range
#' @param param_var Name of the variable that contains the analysis parameter values
#' @param param_val Name of the analysis parameter
#' @param analysis_indicator Name of the variable that contains the values analysis indicator
#' @param summary_stats Name of the variable that contains the values of the summary statistic
#' @param limits Vector that contains the limits of the plot
#' @param plot_name Name of plot
#' @param annotate_x_axis Logical indicating whether to annotate the x-axis
#'
#' @keywords internal
#'
#' @return A ggplot2 object
create_lb_vs_plot <- function(data, date, val, low_limit, high_limit, param_var, param_val, summary_stats, x_limits,
                              x_axis_unit, x_axis_breaks, palette, sl_info, vline_vars, vline_day_numbers, ref_date,
                              plot_name, annotate_x_axis) {

  # NOTE(miguel): The following song and dance courtesy of plotly::layout not supporting dates on axes
  # column names that end with '_z' are days that represent ref_date as zero (unlike CDISC)
  data[["date_z"]] <- as.numeric(as.Date(data[[date]]) - ref_date)
  x_limits_z <- x_limits - ref_date

  # In order to create a unified legend for analysis indicator values, all possible values must appear in the data
  if ("analysis_indicator" %in% names(data)) {

    all_categories <- levels(data[["analysis_indicator"]])

    # Create a fill-in data frame with one row for each category
    ghost_data <- data.frame(date_z = NA_real_,
                             val = NA_real_,
                             analysis_indicator = all_categories,
                             stringsAsFactors = TRUE)

    ghost_data[[param_var]] <- param_val

    # Append to main plot data
    data <- dplyr::bind_rows(data, ghost_data)
  } else {
    all_categories <- NULL
  }

  # Set column for title banner
  data[["title_banner"]] <- paste0(plot_name, ": ", param_val)

  # initiate plot object
  plot <- ggplot2::ggplot(data,
                          ggplot2::aes(x = .data[["date_z"]],
                                       y = .data[[val]],
                                       group = 1)) +
    ggplot2::theme_bw(base_family = "Liberation Sans",
                      base_size = 9)

  # reference range
  if (!is.null(low_limit) && !is.null(high_limit)) {

    ref_range_df <- data.frame(
      xmin = x_limits_z[[1]],
      xmax = x_limits_z[[2]],
      # TODO: Assert that low and high limits are the same (maybe even outside this function?)
      low_limit = sort(data[[low_limit]], na.last = TRUE)[1],
      high_limit = sort(data[[high_limit]], decreasing = TRUE, na.last = TRUE)[1]
    )

    plot <- plot +
      ggplot2::geom_rect(
        data = ref_range_df,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = low_limit,
          ymax = high_limit,
          fill = "REFERENCE RANGE"
        ),
        alpha = 0.3,
        inherit.aes = FALSE
      )
  }

  plot <- create_vlines(plot, sl_info, vline_vars, vline_day_numbers)

  # line or summary line
  if (is.null(summary_stats)) {
    plot <- plot + ggplot2::geom_line(
      ggplot2::aes(linetype = .data[[param_var]]),
      linewidth = 0.1,
      show.legend = FALSE
    )
  } else {
    summary_stats_label <- attr(data[[summary_stats]], "label")
    plot <- plot + ggplot2::geom_line(
      ggplot2::aes(
        x = .data[["date_z"]],
        y = .data[[summary_stats]],
        linetype = summary_stats_label
      ),
      linewidth = 0.1,
      show.legend = FALSE
    )
  }

  # dots (plain or analysis indicator)
  if ("analysis_indicator" %in% names(data)) {
    # NOTE: if original order is desired, app creator should provide sorted analysis_indicator factor levels
    plot <- plot + ggiraph::geom_point_interactive(
      ggplot2::aes(color = .data[["analysis_indicator"]],
                   tooltip = .data[["tooltip"]]),
      size = 1
    )
  } else {
    plot <- plot + ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = .data[["tooltip"]]),
      color = "black",
      size = 1
    )
  }

  # get facet plots and set formats
  plot <- plot + ggplot2::facet_wrap(
    ggplot2::vars(.data[["title_banner"]]),
    ncol = 1,
    scales = "free_y"
  )

  plot <- plot + ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 7),
    strip.text = ggplot2::element_text(size = 10, hjust = 0) # banner text size
  )

  if (!annotate_x_axis) {
    plot <- plot + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.length.x = ggplot2::unit(0, "pt")
    )
  }

  as_CDISC_days <- function(days) days + (days >= 0)

  if (x_axis_unit == CONST$PLOT_X_AXIS_UNITS$DAYS) {
    if (length(x_axis_breaks) == 1) {
      breaks <- base::pretty(x_limits_z, n = x_axis_breaks)
    } else {
      breaks <- x_axis_breaks
    }

    lbl_fn <- function(days) {
      dates <- ref_date + days
      days <- as_CDISC_days(days)
      sprintf("%s\nDay %s", dates, days)
    }
  } else if (x_axis_unit == CONST$PLOT_X_AXIS_UNITS$WEEKS) {
    if (length(x_axis_breaks) == 1) {
      # Calculates the breaks in weeks and move them back to days, round in case we incur in a numerical error
      breaks <- round(base::pretty(x_limits_z / 7, n = x_axis_breaks) * 7)
      # Only return values within limits otherwise the appear in the labels as NA
      breaks <- breaks[breaks >= x_limits_z[[1]] & breaks <= x_limits_z[[2]]]
    } else {
      breaks <- x_axis_breaks * 7
    }

    lbl_fn <- function(days) {
      dates <- ref_date + days
      days_z <- days
      days <- as_CDISC_days(days)
      labels <- vector(mode = "character", length = length(days))

      for (idx in seq_along(days)) {
        day <- days[[idx]]
        day_z <- days_z[[idx]]
        date <- dates[[idx]]
        if (day_z == 0) {
          labels[[idx]] <- sprintf("%s\nDay %s", date, day)
        } else {
          labels[[idx]] <- sprintf("%s\nWeek %s", date, day_z / 7)
        }
      }
      return(labels)
    }
  } else {
    stop("Unknown x_axis_unit")
  }

  plot <- plot + ggplot2::scale_x_continuous(
    labels = lbl_fn,
    breaks = breaks,
    limits = as.numeric(x_limits_z),
    expand = c(0, 0)
  )

  # Define colors for the plotted points
  plot <- plot + ggplot2::scale_color_manual(
    name = "Legend",
    values = palette,
    breaks = all_categories
  )

  # Define color for the reference range
  plot <- plot + ggplot2::scale_fill_manual(
    name = "Legend",
    values = palette
  )

  return(plot)
}

#' Add vertical lines to plot
#'
#' @param plot A ggplot2 object
#' @param plot_data Data of the plot
#' @param vline_vars A list used to define the label and position of dashed lines in the plots
#'
#' @keywords internal
#'
#' @return A ggplot2 object
create_vlines <- function(plot, plot_data, vline_vars, vline_day_numbers) {
  # Days are CDISC-based which means anything >=0 has been incremented by 1, so we undo that
  cdisc_to_continuous_day_number <- function(day) day - (day >= 0)

  vline_x_data <- lapply(vline_vars, function(vline) {
    if (inherits(plot_data[[vline]], "Date")) {
      # The logical space of the plot _does_ use 0 as the reference date, so no transformation is necessary for dates
      plot_data[[vline]] - plot_data[["trt_start_date"]]
    } else {
      if (length(plot_data[[vline]]) > 1) browser()
      cdisc_to_continuous_day_number(plot_data[[vline]])
    }
  })

  vline_x_data <- append(vline_x_data, cdisc_to_continuous_day_number(vline_day_numbers))

  # Filter out the nulls from the vline x data
  vline_x_data <- Filter(Negate(is.null), vline_x_data)

  x <- unlist(vline_x_data, use.names = FALSE)
  v_labels <- names(vline_x_data)

  if (length(v_labels) && length(x)) { # this guard only to prevent ggplotly from failing if on empty vline spec
    vline_data <- data.frame(
      x = x,
      v_labels = v_labels,
      tooltip = paste0(
        "<div style='background-color:#FFE4B3; color:black; border:1px solid black; padding:2px;'>",
        v_labels,
        "</div>"
      )
    )

    plot <- plot +
      ggiraph::geom_vline_interactive(
        data = vline_data,
        ggplot2::aes(
          xintercept = x,
          tooltip = .data[["tooltip"]],
          data_id = .data[["v_labels"]],
          color = .data[["v_labels"]]
        ),
        linetype = "dashed",
        alpha = 0.5,
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  return(plot)
}
