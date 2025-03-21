#' Function to create ae or cm plot
#'
#' @param data Data frame containing the data for the plot
#' @param limits Vector that contains the limits of the plot
#' @param palette Named vector that contains the colors that are used in the plot
#'
#' @keywords internal
#'
#' @return A ggplot2 object
create_ae_cm_plot <- function(data, x_limits, palette, sl_info, vline_vars, vline_day_numbers,
                              ref_date) {
  # set column for title banner
  data[["title_banner"]] <- " "

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

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["start_day_z"]], y = .data[["decode"]]))
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::geom_rect(
    ggplot2::aes(
      xmin = .data[["start_day_z"]],
      xmax = .data[["end_day_z"]],
      ymin = .data[["decode"]],
      ymax = .data[["decode"]],
      color = grading
    ),
    size = 3
  )

  if ("serious_ae" %in% names(data)) {
    sae_labels <- ifelse(data[["serious_ae"]], "SAE", "")
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["start_day_z"]],
        y = .data[["decode"]],
        label = sae_labels
      ),
      colour = "red", nudge_y = 0.25, nudge_x = 1, size = 3
    )
  }

  # hack to allow hover info at the beginning of the range through transparent dots
  p <- p + ggplot2::geom_point(
    ggplot2::aes(
      x = .data[["start_day_z"]],
      y = .data[["decode"]],
      color = grading
    ),
    size = 5, alpha = 0, show.legend = FALSE
  )

  # check if grading value mapped to a colour in palette, else assign a colour.
  unmapped_grading_vals <- setdiff(unique(data[["grading"]]), c(names(palette), NA))
  if (length(unmapped_grading_vals)) {
    selected_colours <- sample(setdiff(colours(), palette),
                               length(unmapped_grading_vals))
    names(selected_colours) <- unmapped_grading_vals
    message(
      sprintf(
        "The grading values - %s - have been assigned the colours %s respectively.",
        paste0("'", unmapped_grading_vals, "'", collapse = ", "),
        paste0("'", selected_colours, "'", collapse = ", ")
      )
    )
    palette <- c(palette, selected_colours)
    rm(unmapped_grading_vals, selected_colours)
  }

  p <- p + ggplot2::scale_colour_manual(name = "Legend", values = palette)
  p <- p + ggplot2::scale_fill_manual(name = "Legend", values = palette)

  if (any(!is.na(data[["arrow_left"]]))) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["arrow_left_z"]] - .5,
        y = .data[["decode"]],
        label = sprintf("\u2190"),
        color = grading
      ),
      size = 10, show.legend = FALSE
    )
  }

  if (any(!is.na(data[["arrow_right"]]))) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["arrow_right_z"]] + .7,
        y = .data[["decode"]],
        label = sprintf("\u2192"),
        color = grading
      ),
      size = 10, show.legend = FALSE
    )
  }

  p <- p + ggplot2::facet_wrap(~ .data[["title_banner"]], ncol = 1, scales = "free_y")
  p <- p + ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.justification = "top",
    legend.position = "right",
    axis.text.y = ggplot2::element_text(size = 7),
    strip.text = ggplot2::element_text(size = 4),
    panel.spacing = ggplot2::unit(0, "lines")
  )

  as_CDISC_days <- function(days) days + (days >= 0)
  p <- p + ggplot2::scale_x_continuous(
    labels = function(days) {
      dates <- ref_date + days
      days <- as_CDISC_days(days)
      sprintf("%s\nDay %s", dates, days)
    },
    limits = x_limits_z
  )

  p <- create_vlines(p, sl_info, vline_vars, vline_day_numbers)

  return(p)
}

#' Function to create lab or vital sign plot
#'
#' @param data Data frame containing the data for the plot
#' @param lb_selected_params Vector containing the values of the selected parameters
#' @param day Character name of the variable that contains the analyze day
#' @param val Character name of the variable that contains analyze value
#' @param low_limit Character name of the variable that contains the values of the low limit of the normal range
#' @param high_limit Character name of the variable that contains the values of the high limit of the normal range
#' @param params Character name of the variable that contains the values of the analyze parameters
#' @param analysis_indicator Character name of the variable that contains the values analysis indicator
#' @param summary_stats Character name of the variable that contains the values of the summary statistic
#' @param limits Vector that contains the limits of the plot
#'
#' @keywords internal
#'
#' @return A ggplot2 object
create_lb_vs_plot <- function(data, date, val, low_limit, high_limit, param, summary_stats, x_limits,
                              palette, sl_info, vline_vars, vline_day_numbers, ref_date) {
  # NOTE(miguel): The following song and dance courtesy of plotly::layout not supporting dates on axes
  # column names that end with '_z' are days that represent ref_date as zero (unlike CDISC)
  data[["date_z"]] <- as.numeric(as.Date(data[[date]]) - ref_date)
  x_limits_z <- x_limits - ref_date

  # initiate plot object
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["date_z"]], y = .data[[val]], group = 1)) +
    ggplot2::theme_bw()

  # reference range
  if (!is.null(low_limit) && !is.null(high_limit)) {
    # TODO: Assert that low and high limits are the same ( maybe even outside this function ? )
    low_limit <- unique(data[[low_limit]])[[1]]
    high_limit <- unique(data[[high_limit]])[[1]]

    # Plotly doesn't support ggplot -Inf,+Inf ranges for geom_rect (https://github.com/plotly/plotly.R/issues/1559)
    # so we're forced to derive a range ourselves
    plot <- plot +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = x_limits_z[[1]], xmax = x_limits_z[[2]], ymin = low_limit, ymax = high_limit,
        fill = "REFERENCE RANGE"
      ), alpha = 0.3, inherit.aes = FALSE)
  }

  plot <- create_vlines(plot, sl_info, vline_vars, vline_day_numbers)


  # line or summary line
  if (is.null(summary_stats)) {
    plot <- plot + ggplot2::geom_line(ggplot2::aes(linetype = .data[[param]]), size = 0.1)
  } else {
    summary_stats_label <- attr(data[[summary_stats]], "label")
    plot <- plot + ggplot2::geom_line(
      ggplot2::aes(
        x = .data[["date_z"]],
        y = .data[[summary_stats]],
        linetype = summary_stats_label
      ),
      size = 0.1
    )
  }

  # dots (plain or analysis indicator)
  if ("analysis_indicator" %in% names(data)) {
    # NOTE: if original order is desired, app creator should provide sorted analysis_indicator factor levels
    plot <- plot + ggplot2::geom_point(ggplot2::aes(
      color = .data[["analysis_indicator"]],
      fill = .data[["analysis_indicator"]]
    ), size = 1)
  } else {
    plot <- plot + ggplot2::geom_point(color = "black", size = 1)
  }


  plot <- plot + ggplot2::scale_color_manual(name = "Legend", values = palette)
  plot <- plot + ggplot2::scale_fill_manual(name = "Legend", values = palette)

  as_CDISC_days <- function(days) days + (days >= 0)

  # get facet plots and set formats
  plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[[param]]), ncol = 1, scales = "free_y") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), # disable legend title
      legend.justification = "top",
      axis.text.y = ggplot2::element_text(size = 7), # y-axis text size
      strip.text = ggplot2::element_text(size = 6), # title text/banner size
      panel.spacing.y = ggplot2::unit(0, "lines") # distance between plots in facet_wrap
    ) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_x_continuous(
      labels = function(days) {
        dates <- ref_date + days
        days <- as_CDISC_days(days)
        sprintf("%s\nDay %s", dates, days)
      },
      limits = x_limits_z
    )

  # HACK: Offset the limits of the y axis to account for label strips
  # TODO: Remove during transition away from ggplot2+plotly
  data_range <- range(data[[val]])
  range_width <- data_range[[2]] - data_range[[1]]
  data_range <- data_range + c(-0.15 * range_width, 0)
  plot <- plot + ggplot2::coord_cartesian(
    ylim = data_range, expand = TRUE, default = TRUE, clip = "on"
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

  not_null_vline_x_data <- sapply(vline_x_data, function(x) {
    !is.null(x)
  })
  vline_x_data <- setdiff(vline_x_data, not_null_vline_x_data)
  x <- unlist(vline_x_data, use.names = FALSE)
  colors <- names(vline_x_data)

  if (length(colors) && length(x)) { # this guard only to prevent ggplotly from failing if on empty vline spec
    vline_data <- data.frame(x, colors)
    plot <- plot + ggplot2::geom_vline(
      data = vline_data, ggplot2::aes(xintercept = x, color = colors),
      linetype = "dashed", alpha = 1
    )
  }

  return(plot)
}
