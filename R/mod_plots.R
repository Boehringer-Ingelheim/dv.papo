PID <- poc(
  PLOT_CONTAINER = "plot_container",
  SELECTOR_CONTAINER = "selector_container",
  PLOT_MESSAGES = "plot_messages",
  PLOT = "plot"
)

PCONF_FIELDS <- poc(
  DATASET_NAME = "dataset",
  VARS = "vars",
  RANGE_PLOTS = "range_plots",
  VALUE_PLOTS = "value_plots",
  VLINE_VARS = "vline_vars",
  TIMELINE_INFO = "timeline_info",
  VLINE_DAY_NUMBERS = "vline_day_numbers",
  PALETTE = "palette",
  X_AXIS_UNIT = "x_axis_unit",
  X_AXIS_BREAKS = "x_axis_breaks",
  START_DATE = "start_date",
  END_DATE = "end_date",
  DECODE = "decode",
  GRADING = "grading",
  SERIOUS_AE = "serious_ae",
  ARROW_LEFT = "arrow_left",
  ARROW_RIGHT = "arrow_right",
  TOOLTIP = "tooltip",
  ANALYSIS_PARAM = "analysis_param",
  ANALYSIS_DATE = "analysis_date",
  ANALYSIS_VAL = "analysis_val",
  ANALYSIS_INDICATOR = "analysis_indicator",
  RANGE_LOW_LIMIT = "range_low_limit",
  RANGE_HIGH_LIMIT = "range_high_limit",
  SUMMARY_STATS = "summary_stats",
  DEFAULT_ANALYSIS_PARAMS = "default_analysis_params"
)

SL_INFO_FIELDS <- poc(
  TRT_START_DATE = "trt_start_date",
  TRT_END_DATE = "trt_end_date",
  ICF_DATE = "icf_date",
  PART_END_DATE = "part_end_date"
)

#' Calculate the timeline limits
#'
#' Initialized to treatment start and end dates, but takes informed consent and participation end dates into account if
#' available. If end dates are missing then set to today's date.
#'
#' @param rfxstdt Treatment start date.
#' @param rfxendt Treatment end date.
#' @param rficdt Informed consent date (`NULL` value allowed).
#' @param rfpendt Participation end date  (`NULL` value allowed).
#'
#' @return 2-element vector of timeline minimum and maximum limits.
#'
#' @keywords internal
calc_timeline_limits <- function(rfxstdt, rfxendt, rficdt = NULL, rfpendt = NULL) {
  checkmate::assert_date(rfxstdt, min.len = 1, max.len = 1)
  checkmate::assert_date(rfxendt, min.len = 1, max.len = 1)
  checkmate::assert_date(rficdt, min.len = 1, max.len = 1, null.ok = TRUE)
  checkmate::assert_date(rfpendt, min.len = 1, max.len = 1, null.ok = TRUE)

  min_total <- rfxstdt
  if (!is.null(rficdt) && is.finite(rficdt)) min_total <- rficdt

  max_total <- as.Date(-Inf)
  if (is.finite(rfxendt)) max_total <- rfxendt
  if (!is.null(rfpendt)) {
    if (is.finite(rfpendt)) max_total <- rfpendt
    else max_total <- as.Date(Inf)
  }
  if (!is.finite(max_total)) max_total <- Sys.Date()

  c(min_total, max_total)
}


#' Create user interface for patient plot shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of \code{patient_plot_server()}.
#' @param title character: Title of plot module
#'
#' @keywords internal
#'
patient_plot_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns(PID$PLOT_CONTAINER))
}


#' Create server for patient plot shiny module of \pkg{dv.papo}
#'
#' TODO: Document params (inherit them)
#'
#' @keywords internal
#'
patient_plot_server <- function(id, subjid_var,
                                subject_level_dataset, timeline_info,
                                extra_datasets, range_plots, value_plots,
                                vline_vars, vline_day_numbers, palette, x_axis_unit, x_axis_breaks) {
  
  testing <- isTRUE(getOption("shiny.testmode"))
  
  # Ensure font "Liberation Sans" is registered, so it can be used by {{ggiraph}}
  gdtools::register_liberationsans()

  palette <- unlist(utils::modifyList(
    as.list(CONST$DEFAULT_PALETTE),
    as.list(palette)
  )) # user palette complements default

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

      # When testing reactivity, the usual way of looking at the state of a module is to isolate variables of interest
      # into reactives of their own and then expose them through exportTestValues. This is a less invasive approach.
      # We create a regular, non-reactive list and ... [continued in #ipahbo]
      
      if (testing) {
        exported_test_data <- list()
        shiny::exportTestValues(test_plot_data = exported_test_data)
      } else {
        exported_test_data <- NULL
      }

      v_extra_datasets <- ODGE[["A"]][["sm_mr2"]]({
        extra_datasets <- extra_datasets()
        for (df in extra_datasets) {
          for (plot in c(range_plots, value_plots)) {
            plot_cols <- append(plot[[PCONF_FIELDS$VARS]], subjid_var)
            ensure_columns_exist(
              extra_datasets[[plot[[PCONF_FIELDS$DATASET_NAME]]]],
              unlist(plot_cols)
            )

            date_cols <- c(
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$START_DATE]],
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$END_DATE]],
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$ANALYSIS_DATE]]
            )
            ensure_columns_are_dates_or_datetimes(
              extra_datasets[[plot[[PCONF_FIELDS$DATASET_NAME]]]],
              date_cols
            )

            numeric_cols <- c(
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$ANALYSIS_VAL]],
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$RANGE_LOW_LIMIT]],
              plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$RANGE_HIGH_LIMIT]]
            )
            ensure_columns_are_numeric(
              extra_datasets[[plot[[PCONF_FIELDS$DATASET_NAME]]]],
              numeric_cols
            )
          }
        }
        return(ODGE[["A"]][["sm_me"]](extra_datasets))
      }, varname = "v_extra_datasets")

      output[[PID$PLOT_CONTAINER]] <- shiny::renderUI({
        shiny::req(!is.null(timeline_info))
        shiny::tagList(
          shiny::h3("Graphical Display"),

          shiny::uiOutput(ns(PID$SELECTOR_CONTAINER)),

          shiny::htmlOutput(ns(PID$PLOT_MESSAGES)),
          shiny::div(
            style = "height: 800px; overflow-y: scroll; border: 1px solid #eee; padding: 10px;",
            gdtools::liberationsansHtmlDependency(),
            ggiraph::girafeOutput(ns(PID$PLOT), width = "100%", height = "auto")
          ),
          shiny::br()
        )
      })

      # Selectors for vs+lab plots.
      output[[PID$SELECTOR_CONTAINER]] <- shiny::renderUI({
        extra_datasets <- v_extra_datasets()
        selectors <- list()

        for (plot_name in names(value_plots)) {
          plot <- value_plots[[plot_name]]

          dataset_name <- plot[[PCONF_FIELDS$DATASET_NAME]]
          param_col <- plot[[PCONF_FIELDS$VARS]][[PCONF_FIELDS$ANALYSIS_PARAM]]
          choices <- sort(unique(extra_datasets[[dataset_name]][[param_col]])) # TODO: Enforce factor and use levels in the original order

          selector_id <- sanitize_id(plot_name)

          # Get the previously selected values, if null then assign to defaults
          selected <- shiny::isolate(input[[selector_id]])
          if (is.null(selected)) {
            selected <- plot[[PCONF_FIELDS$DEFAULT_ANALYSIS_PARAMS]]
          }

          selectors[[length(selectors) + 1]] <- shinyWidgets::pickerInput(
            inputId = ns(selector_id),
            label = paste("Select", plot_name, "Parameters:"),
            choices = choices,
            selected = selected,
            multiple = TRUE,
            options = list("live-search" = TRUE, "actions-box" = TRUE)
          )
        }

        shiny::div(
          style = "display: flex; flex-wrap: wrap; gap: 20px;",
          selectors
        )
      })

      if (length(range_plots) > 0 || length(value_plots) > 0) {
        plots_and_messages <- ODGE[["A"]][["sm_mr2"]]({
        
          # Depend on the datasets before we run the loop below. Unsure if this is a side case or required but keeps
          # previous behavior
          shiny::req(subject_level_dataset(), v_extra_datasets())

          vs_lb_selected <- local({
              ids <- sanitize_id(names(value_plots))
              res <- Map(function(id) input[[id]], ids)
              can_proceed <- setequal(
                intersect(ids, shiny::isolate(names(input))),
                ids
              )
              shiny::req(isTRUE(can_proceed))
              return(res)
            })

          res <- ODGE[["A"]][["sm_me"]]({                  
            sl_ds <- ..(subject_level_dataset())
            eds <- ..(v_extra_datasets())
            dv.papo:::compute_plots_and_messages(
              sl_ds,
              eds,
              ..(vs_lb_selected),
              ..(timeline_info),
              ..(exported_test_data),
              ..(range_plots),
              ..(value_plots),
              ..(vline_vars),
              ..(vline_day_numbers),
              ..(x_axis_unit),
              ..(x_axis_breaks),
              ..(palette)
            )                  
          })            

          if (testing) {
            exported_test_data <<- res[["exported_test_data"]]
          }

          return(res)
        },
          varname = "plots_and_messages"
        )        
      } else {
        plots_and_messages <- ODGE[["A"]][["sm_mr"]](
          {
            list(
              plot_list = list(),
              messages = "* No range or value plots configured"
            )
          },
          varname = "plots_and_messages"
        )
      }

      output[[PID$PLOT]] <- ggiraph::renderGirafe({
        plot_list <- plots_and_messages()[["plot_list"]]
        shiny::req(length(plot_list) > 0)

        # Theme application across all plots
        plot_list <- lapply(plot_list, function(p) {
          p +
            ggplot2::theme(
              plot.margin = ggplot2::margin(0, 0, 1, 0, unit = "pt"),
              plot.background = ggplot2::element_blank(),
              legend.title = ggplot2::element_blank(),
              legend.justification = "top",
              legend.position = "right"
            )
        })

        plots <- patchwork::wrap_plots(plot_list, ncol = 1)

        plot_height_ratios <- plots_and_messages()[["plot_height_ratios"]]

        plots <- plots +
          patchwork::plot_layout(
            guides = "collect",
            heights = plot_height_ratios
          )

        # Calculate plot height by summing the ratios, adding 0.2 for x-axis space, and multiplying result by 2
        plot_height <- (sum(plot_height_ratios) + 0.2) * 2

        ggiraph::girafe(
          ggobj = plots,
          width_svg = 12,
          height_svg = plot_height,
          options = list(
            ggiraph::opts_selection(type = "none"),
            ggiraph::opts_sizing(rescale = TRUE),
            ggiraph::opts_tooltip(css = "border:none; padding:0px;"),
            ggiraph::opts_zoom(min = 0.5, max = 5)
          )
        )
      })

      output[[PID$PLOT_MESSAGES]] <- shiny::renderUI({
        messages <- plots_and_messages()[["messages"]]
        shiny::HTML(paste(messages, collapse = "<br>"))
      })

      to_odg <- list(
        patient_plots = list(
          label = "Patient Plots",
          metareactive = list(
            html = ODGE[["A"]][["sm_mr"]](
              {
                ..(plots_and_messages()[["plot_list"]])
              },
              varname = "patient_plots"
            ),
            pdf = ODGE[["A"]][["sm_mr"]](
              {
                ..(plots_and_messages()[["plot_list"]])
              },
              varname = "patient_plots"
            )
          )
        ),
        patient_plots_messages = list(
          label = "Patient Plot messages",
          metareactive = list(
            html = ODGE[["A"]][["sm_mr"]](
              {
                ..(plots_and_messages())[["messages"]]
              },
              varname = "patient_plots_messages"
            ),
            pdf = ODGE[["A"]][["sm_mr"]](
              {
                ..(plots_and_messages())[["messages"]]
              },
              varname = "patient_plots_messages"
            )
          )
        )
      )

    }
  )
}

  build_tooltip <- function(
    tooltip_spec,
    df,
    color_key = NULL,
    palette = NULL
  ) {
    if (!is.null(color_key)) {
      fill_colors <- palette[as.character(df[[color_key]])]
      fill_colors[is.na(fill_colors)] <- "darkgray"
    } else {
      fill_colors <- rep("darkgray", nrow(df))
    }

    # Convert to hex coded RGB color values for use in CSS style
    fill_rgb_matrix <- grDevices::col2rgb(fill_colors)
    fill_colors_hex <- grDevices::rgb(
      red = fill_rgb_matrix[1, ],
      green = fill_rgb_matrix[2, ],
      blue = fill_rgb_matrix[3, ],
      maxColorValue = 255
    )

    # W3C formula for relative luminance
    # Multiply the RGB channels by their perceived brightness weights
    luminance <- (0.299 * fill_rgb_matrix[1, ]) +
      (0.587 * fill_rgb_matrix[2, ]) +
      (0.114 * fill_rgb_matrix[3, ])

    # If luminance is high (> 150-186 range), background is light -> use black text
    # If luminance is low, background is dark -> use white text
    text_colors <- ifelse(luminance > 160, "black", "white")

    res <- list()
    for (i_row in seq_len(nrow(df))) {
      fill_color <- fill_colors_hex[[i_row]]
      text_color <- text_colors[[i_row]]
      res_elem <- sprintf(
        "<div style='background-color:%s; color:%s; border:1px solid %s; padding:2px;'>",
        fill_color,
        text_color,
        text_color
      )
      for (i_line in seq_along(tooltip_spec)) {
        prefix <- names(tooltip_spec)[[i_line]] # NOTE: App creators can specify breaking lines through '<br>'
        col <- tooltip_spec[[i_line]]
        res_elem <- paste0(res_elem, prefix, df[[col]][[i_row]], "<br>")
      }
      res_elem <- paste0(res_elem, "</div>")
      res[[i_row]] <- res_elem
    }

    return(res)
  }

  compute_plots_and_messages_ <- function(
    subject_level_dataset,
    extra_datasets,
    vs_lb_selected,
    timeline_info,
    exported_test_data = NULL,
    range_plots,
    value_plots,
    vline_vars,
    vline_day_numbers,
    x_axis_unit,
    x_axis_breaks,
    palette
  ) {
    # TODO: Remove the messages already guarded against by check_papo_call
    messages <- character(0)

    # Process subject_level_dataset ----
    err <- ensure_columns_exist(
      subject_level_dataset,
      timeline_info,
      flag_column_function = flag_columns_capture_error
    )
    if (!is.null(err)) {
      return(list(plot_list = list(), messages = err))
    } # fatal error

    timeline_info_names <- names(timeline_info)
    date_cols <- timeline_info[endsWith(timeline_info_names, "date")]
    err <- ensure_columns_are_dates_or_datetimes(
      subject_level_dataset,
      date_cols,
      flag_column_function = flag_columns_capture_error
    )
    if (!is.null(err)) {
      return(list(plot_list = list(), messages = err))
    } # fatal error

    sl_info <- local({
      # map date-times to dates and warn about loss of precision
      res <- subject_level_dataset
      for (col in date_cols) {
        if (inherits(res[[col]], "POSIXt")) {
          date <- as.Date(res[[col]])
          if (!is.na(date) && as.POSIXct(date) != res[[col]]) {
            # just a warning
            messages[[length(messages) + 1]] <<- paste0(
              "* Date-time column `",
              col,
              "` rounded to nearest date."
            )
          }
          res[[col]] <- date
        }
      }

      # use internal names for timeline columns (copied and not renamed to cope with repeat elements)
      for (i_col in seq_along(timeline_info)) {
        dest <- names(timeline_info)[[i_col]]
        orig <- timeline_info[[i_col]]
        res[[dest]] <- res[[orig]]
      }

      return(res)
    })

    # Compute plots ----

    # Treatment start date is required as the reference date for plotting x-axis
    if (is.na(sl_info[[SL_INFO_FIELDS$TRT_START_DATE]])) {
      messages[[
        length(messages) + 1
      ]] <- "* Plot cannot be created: No treatment start date available."
      range_plots <- NULL
      value_plots <- NULL
    }

    # start...end, but takes icf and part_end dates into account if available
    timeline_limits <- calc_timeline_limits(
      rfxstdt = sl_info[[SL_INFO_FIELDS$TRT_START_DATE]],
      rfxendt = sl_info[[SL_INFO_FIELDS$TRT_END_DATE]],
      rficdt = sl_info[[SL_INFO_FIELDS$ICF_DATE]],
      rfpendt = sl_info[[SL_INFO_FIELDS$PART_END_DATE]]
    )

    x_limits <- local({
      # we need to compute combined limits first because ggplot+plotly need them before layout
      # calculation the +/-1 avoids clipping the left and right arrows on the plot
      timeline_limit_lower <- timeline_limits[[1]]
      timeline_limit_upper <- timeline_limits[[2]]
      diff <- timeline_limit_upper - timeline_limit_lower
      offset <- max(1L, round(diff / 10))
      c(timeline_limit_lower - offset, timeline_limit_upper + offset)
    })

    plot_list <- local({
      res <- list()

      # Preview all plot data to exclude plots with no data.
      # X-axis tick marks and labels need to be shown on the last plot.

      for (plot_name in names(range_plots)) {
        plot_params <- range_plots[[plot_name]]
        df <- extra_datasets[[plot_params[[PCONF_FIELDS$DATASET_NAME]]]]

        if (nrow(df) > 0) {
          last_plot_name <- plot_name
        } else {
          messages[[length(messages) + 1]] <<- paste0(
            "* No Data for ",
            plot_name,
            "."
          )
          range_plots[[plot_name]] <- NULL
        }
      }

      last_param <- NULL
      for (plot_name in names(value_plots)) {
        plot_info <- value_plots[[plot_name]]
        params <- vs_lb_selected[[sanitize_id(plot_name)]]

        if (length(params) > 0) {
          last_plot_name <- plot_name

          for (i_param in seq_along(params)) {
            param <- params[[i_param]]
            df <- extra_datasets[[plot_info[[PCONF_FIELDS$DATASET_NAME]]]]
            param_mask <- df[[
              plot_info[[PCONF_FIELDS$VARS]][[
                PCONF_FIELDS$ANALYSIS_PARAM
              ]]
            ]] %in%
              param
            df <- df[param_mask, ]

            if (nrow(df) > 0) last_param <- param
          }
        } else {
          messages[[length(messages) + 1]] <<- paste(
            "* No Parameter for",
            plot_name,
            "selected."
          )
          value_plots[[plot_name]] <- NULL
        }
      }

      # AE, CM
      for (plot_name in names(range_plots)) {
        plot_params <- range_plots[[plot_name]]
        df <- extra_datasets[[plot_params[[PCONF_FIELDS$DATASET_NAME]]]]

        # Column aliases (copied and not renamed to cope with repeat elements)
        vars <- plot_params[[PCONF_FIELDS$VARS]]
        df[[PCONF_FIELDS$START_DATE]] <- as.Date(df[[vars[[
          PCONF_FIELDS$START_DATE
        ]]]])
        df[[PCONF_FIELDS$END_DATE]] <- as.Date(df[[vars[[
          PCONF_FIELDS$END_DATE
        ]]]])
        df[[PCONF_FIELDS$DECODE]] <- df[[vars[[PCONF_FIELDS$DECODE]]]]
        if (PCONF_FIELDS$GRADING %in% names(vars)) {
          df[[PCONF_FIELDS$GRADING]] <- df[[vars[[PCONF_FIELDS$GRADING]]]]
        }
        if (PCONF_FIELDS$SERIOUS_AE %in% names(vars)) {
          # FIXME: This is a temporal patch while we fix the modular API part
          if (!is.logical(df[[vars[[PCONF_FIELDS$SERIOUS_AE]]]])) {
            df[[PCONF_FIELDS$SERIOUS_AE]] <- df[[vars[[
              PCONF_FIELDS$SERIOUS_AE
            ]]]] ==
              "Y"
          } else {
            df[[PCONF_FIELDS$SERIOUS_AE]] <- df[[vars[[
              PCONF_FIELDS$SERIOUS_AE
            ]]]]
          }
        }

        # Wrap decode column into no more than two lines (to avoid overlap). Increment width one-by-one,
        # from the larger of the preset constant or an estimate (mid-point of longest decode text), until
        # all decode texts fit over a maximum of two lines.
        width_estimate <- ceiling(
          nchar(as.character(df[[PCONF_FIELDS$DECODE]])) / 2
        )
        max_width <- max(
          width_estimate,
          CONST$decode_max_width_before_wrap_in_characters
        )
        repeat {
          wrapped <- strwrap(
            df[[PCONF_FIELDS$DECODE]],
            width = max_width,
            simplify = FALSE
          )
          if (max(lengths(wrapped)) <= 2) {
            break
          }
          max_width <- max_width + 1
        }
        df[[PCONF_FIELDS$DECODE]] <- sapply(wrapped, function(x) {
          paste(x, collapse = "\n")
        })

        df <- df[
          intersect(
            names(df),
            c(
              PCONF_FIELDS$START_DATE,
              PCONF_FIELDS$END_DATE,
              PCONF_FIELDS$DECODE,
              PCONF_FIELDS$GRADING,
              PCONF_FIELDS$SERIOUS_AE
            )
          )
        ]

        # Add `arrow_left` and `arrow_right` columns to range_plots
        unknown_start_date <- is.na(df[[PCONF_FIELDS$START_DATE]])
        predate_study_start_date <- df[[PCONF_FIELDS$START_DATE]] <
          timeline_limits[[1]]

        df[[PCONF_FIELDS$ARROW_LEFT]] <- as.Date(NA) # no arrow
        df[
          unknown_start_date | predate_study_start_date,
          PCONF_FIELDS$ARROW_LEFT
        ] <- timeline_limits[[1]]
        df[
          unknown_start_date | predate_study_start_date,
          PCONF_FIELDS$START_DATE
        ] <- timeline_limits[[1]]

        unknown_end_date <- is.na(df[[PCONF_FIELDS$END_DATE]])
        outlast_study_end_date <- timeline_limits[[2]] <
          df[[PCONF_FIELDS$END_DATE]]

        df[[PCONF_FIELDS$ARROW_RIGHT]] <- as.Date(NA) # no arrow
        df[
          unknown_end_date | outlast_study_end_date,
          PCONF_FIELDS$ARROW_RIGHT
        ] <- timeline_limits[[2]]
        df[
          unknown_end_date | outlast_study_end_date,
          PCONF_FIELDS$END_DATE
        ] <- timeline_limits[[2]]

        df[[PCONF_FIELDS$TOOLTIP]] <- build_tooltip(
          tooltip_spec = plot_params[[PCONF_FIELDS$TOOLTIP]],
          df = extra_datasets[[plot_params[[PCONF_FIELDS$DATASET_NAME]]]],
          color_key = vars[[PCONF_FIELDS$GRADING]],
          palette = palette
        )

        # The last plot to be shown must have x-axis annotations
        annotate_x_axis <- last_plot_name == plot_name &&
          is.null(last_param)

        ggplot <- create_ae_cm_plot(
          data = df,
          x_limits = x_limits,
          palette = palette,
          sl_info = sl_info,
          vline_vars = vline_vars,
          vline_day_numbers = vline_day_numbers,
          x_axis_unit = x_axis_unit,
          x_axis_breaks = x_axis_breaks,
          ref_date = sl_info[[SL_INFO_FIELDS$TRT_START_DATE]],
          plot_name = plot_name,
          annotate_x_axis = annotate_x_axis
        )

        # Attach the height ratio to be passed to `patchwork::plot_layout(heights = ...)`.
        # Count the number of unique terms that will appear on the y-axis, add one for
        # banner space, then divide by six to adjust relative to value plot heights
        # which have a fixed height ratio of 1.
        attr(ggplot, "plot_height") <- (length(unique(df[[
          PCONF_FIELDS$DECODE
        ]])) +
          1) /
          6

        # ... [continued from #ipahbo] we just dump stuff into it from inside reactives wherever the
        # variable of interest becomes available. Then ... [continued on tests/testthat/test-all.R:#umeega]
        # using <<- because we are inside a local
        if (!is.null(exported_test_data)) {
          exported_test_data[[paste0("tooltips/", plot_name)]] <<- df[[
            PCONF_FIELDS$TOOLTIP
          ]]
          exported_test_data[[paste0(
            "plot_first_line_color/",
            plot_name
          )]] <<-
            ggplot2::ggplot_build(ggplot)$data[[1]][["fill"]][[1]]
          exported_test_data[[paste0("arrow_right/", plot_name)]] <<- df[[
            PCONF_FIELDS$ARROW_RIGHT
          ]]
          exported_test_data[[paste0("serious_ae/", plot_name)]] <<- df[[
            PCONF_FIELDS$SERIOUS_AE
          ]]
        }

        res[[length(res) + 1]] <- ggplot
      }

      # VS, LAB
      for (plot_name in names(value_plots)) {
        plot_info <- value_plots[[plot_name]]
        params <- vs_lb_selected[[sanitize_id(plot_name)]]

        for (i_param in seq_along(params)) {
          local_palette <- palette

          param <- params[[i_param]]
          df <- extra_datasets[[plot_info[[PCONF_FIELDS$DATASET_NAME]]]]

          param_mask <- df[[plot_info[[PCONF_FIELDS$VARS]][[
            PCONF_FIELDS$ANALYSIS_PARAM
          ]]]] %in%
            param
          df <- df[param_mask, ]

          if (nrow(df) == 0) {
            next
          }

          analysis_indicator_col <- plot_info[[PCONF_FIELDS$VARS]][[
            PCONF_FIELDS$ANALYSIS_INDICATOR
          ]]
          if (!is.null(analysis_indicator_col)) {
            values <- df[[analysis_indicator_col]]
            if (is.character(values)) {
              values <- as.factor(values)
              sprintf(
                paste(
                  "* Analysis indicator column `%s` on dataset `%s` promoted to factor automatically.",
                  "Make it a factor beforehand to avoid this message."
                ),
                analysis_indicator_col,
                plot_info[[PCONF_FIELDS$DATASET_NAME]]
              ) |>
                warning()
            }

            df[[PCONF_FIELDS$ANALYSIS_INDICATOR]] <- values

            analysis_indicator_levels <- levels(values)
            if ("" %in% trimws(analysis_indicator_levels)) {
              sprintf(
                "* Analysis indicator column `%s` on dataset `%s` Contains empty levels.",
                analysis_indicator_col,
                plot_info[[PCONF_FIELDS$DATASET_NAME]]
              ) |>
                warning()
            }

            levels_wo_palette_colors <- setdiff(
              levels(values),
              names(local_palette)
            )
            if (length(levels_wo_palette_colors)) {
              auto_color <- CONST$color_for_missing_analysis_indicator_levels
              sprintf(
                paste0(
                  "* Missing palette colors for analysis indicator column levels: ",
                  paste(levels_wo_palette_colors, collapse = ", "),
                  ". Will paint them as %s."
                ),
                auto_color
              ) |>
                warning()
              local_palette[levels_wo_palette_colors] <- auto_color
            }
          }

          df[[PCONF_FIELDS$TOOLTIP]] <- local({
            mask <- df[[plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$ANALYSIS_PARAM
            ]]]] ==
              param
            build_tooltip(
              tooltip_spec = plot_info[[PCONF_FIELDS$TOOLTIP]],
              df = df[mask, ],
              color_key = analysis_indicator_col,
              palette = palette
            )
          })

          # The last plot to be shown must have x-axis annotations
          annotate_x_axis <- last_plot_name == plot_name &&
            last_param == param

          ggplot <- create_lb_vs_plot(
            data = df,
            date = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$ANALYSIS_DATE
            ]],
            val = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$ANALYSIS_VAL
            ]],
            low_limit = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$RANGE_LOW_LIMIT
            ]],
            high_limit = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$RANGE_HIGH_LIMIT
            ]],
            param_var = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$ANALYSIS_PARAM
            ]],
            param_val = param,
            summary_stats = plot_info[[PCONF_FIELDS$VARS]][[
              PCONF_FIELDS$SUMMARY_STATS
            ]],
            x_limits = x_limits,
            palette = local_palette,
            sl_info = sl_info,
            vline_vars = vline_vars,
            x_axis_unit = x_axis_unit,
            x_axis_breaks = x_axis_breaks,
            vline_day_numbers = vline_day_numbers,
            ref_date = sl_info[[SL_INFO_FIELDS$TRT_START_DATE]],
            plot_name = plot_name,
            annotate_x_axis = annotate_x_axis
          )

          # Attach the height metadata to be passed to `patchwork::plot_layout(heights = ...)`.
          # Assign a fixed height ratio of 1 for all value plots.
          attr(ggplot, "plot_height") <- 1

          res[[length(res) + 1]] <- ggplot
        }
      }

      return(res)
    })

    if (!is.null(exported_test_data)) {
      exported_test_data[["plot_messages"]] <- messages
    }

    # Extract the 'plot_height' attribute from every plot in the list
    plot_height_ratios <- sapply(plot_list, function(p) {
      attr(p, "plot_height")
    })

    return(list(
      plot_list = plot_list,
      messages = messages,
      plot_height_ratios = plot_height_ratios,
      exported_test_data = exported_test_data
    ))
  }

compute_plots_and_messages <- function(...) shiny::maskReactiveContext(compute_plots_and_messages_(...))

# TODO: Brittle approach some hashing, or unique GUID would be better, maybe even a counter
  sanitize_id <- function(id) gsub("[^a-zA-Z0-9_]", "", id)
