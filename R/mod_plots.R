#' Create user interface for patient plot shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of \code{patient_plot_server()}.
#' @param title character: Title of plot module
#'
#' @keywords internal
#'
patient_plot_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("ui"))
}


#' Create server for patient plot shiny module of \pkg{dv.papo}
#'
#' TODO: Document params (inherit them)
#'
#' @keywords internal
#'
patient_plot_server <- function(id, subject_var,
                                subject_level_dataset, timeline_info,
                                extra_datasets, range_plots, value_plots,
                                vline_vars, vline_day_numbers, palette, x_axis_unit, x_axis_breaks) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

      # Ensure font "Liberation Sans" is registered, so it can be used by {{ggiraph}}
      gdtools::register_liberationsans()

      # When testing reactivity, the usual way of looking at the state of a module is to isolate variables of interest
      # into reactives of their own and then expose them through exportTestValues. This is a less invasive approach.
      # We create a regular, non-reactive list and ... [continued in #ipahbo]
      testing <- isTRUE(getOption("shiny.testmode"))
      if (testing) {
        exported_test_data <- list()
        shiny::exportTestValues(test_plot_data = exported_test_data)
      }

      palette <- unlist(utils::modifyList(as.list(CONST[["default_palette"]]), as.list(palette))) # user palette complements default

      v_extra_datasets <- shiny::reactive({
        extra_datasets <- extra_datasets()
        for (df in extra_datasets) {
          for (plot in c(range_plots, value_plots)) {
            plot_cols <- append(plot$vars, subject_var)
            ensure_columns_exist(extra_datasets[[plot$dataset]], unlist(plot_cols))

            date_cols <- c(plot$vars$start_date, plot$vars$end_date, plot$vars$analysis_date)
            ensure_columns_are_dates_or_datetimes(extra_datasets[[plot$dataset]], date_cols)

            numeric_cols <- c(
              plot$vars$analysis_val, plot$vars$range_low_limit,
              plot$vars$range_high_limit
            )
            ensure_columns_are_numeric(extra_datasets[[plot$dataset]], numeric_cols)
          }
        }
        return(extra_datasets)
      })

      sanitize_id <- function(id) gsub("[^a-zA-Z0-9_]", "", id)

      output[["ui"]] <- shiny::renderUI({
        shiny::req(!is.null(timeline_info))
        shiny::tagList(
          shiny::h3("Graphical Display"),
          shiny::fluidRow(
            shiny::uiOutput(ns("selectors"))
          ),
          shiny::htmlOutput(ns("text")),
          shiny::div(
            style = "height: 800px; overflow-y: scroll; border: 1px solid #eee; padding: 10px;",
            gdtools::liberationsansHtmlDependency(),
            ggiraph::girafeOutput(ns("plot"), width = "100%", height = "auto")
          ),
          shiny::br()
        )
      })

      # Selectors for vs+lab plots.
      output[["selectors"]] <- shiny::renderUI({
        extra_datasets <- v_extra_datasets()
        selectors <- list()

        for (plot_name in names(value_plots)) {
          plot <- value_plots[[plot_name]]

          dataset_name <- plot[["dataset"]]
          param_col <- plot[["vars"]][["analysis_param"]]
          choices <- sort(unique(extra_datasets[[dataset_name]][[param_col]])) # TODO: Enforce factor and use levels in the original order

          selector_id <- sanitize_id(plot_name)

          # Get the previously selected values, if null then assign to defaults
          selected <- shiny::isolate(input[[selector_id]])
          if (is.null(selected)) selected <- plot[["default_analysis_params"]]

          selectors[[length(selectors) + 1]] <- shiny::column(
            3,
            shinyWidgets::pickerInput(
              inputId = ns(selector_id),
              label = paste0("Please Select Parameter for ", plot_name, ":"),
              choices = choices,
              selected = selected,
              multiple = TRUE,
              options = list("live-search" = TRUE, "actions-box" = TRUE)
            )
          )
        }

        return(selectors)
      })

      build_tooltip <- function(tooltip_spec, df, color_key = NULL, palette = NULL) {

        if (!is.null(color_key)) {
          fill_colors <- palette[as.character(df[[color_key]])]
          fill_colors[is.na(fill_colors)] <- "darkgray"
        } else {
          fill_colors <- rep("darkgray", nrow(df))
        }

        # Convert to hex coded RGB color values for use in CSS style
        fill_rgb_matrix <- grDevices::col2rgb(fill_colors)
        fill_colors_hex <- grDevices::rgb(red = fill_rgb_matrix[1, ],
                                          green = fill_rgb_matrix[2, ],
                                          blue = fill_rgb_matrix[3, ],
                                          maxColorValue = 255)

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

      compute_plots_and_messages <- function(subject_level_dataset, extra_datasets, vs_lb_selected) {
        # TODO: Remove the messages already guarded against by check_papo_call
        messages <- character(0)
        plots <- list()
        plot_height_ratios <- NULL

        # Process subject_level_dataset ----
        err <- ensure_columns_exist(subject_level_dataset, timeline_info,
          flag_column_function = flag_columns_capture_error
        )
        if (!is.null(err)) {
          return(list(plots = list(), messages = err))
        } # fatal error

        timeline_info_names <- names(timeline_info)
        date_cols <- timeline_info[endsWith(timeline_info_names, "date")]
        err <- ensure_columns_are_dates_or_datetimes(subject_level_dataset, date_cols,
          flag_column_function = flag_columns_capture_error
        )
        if (!is.null(err)) {
          return(list(plots = list(), messages = err))
        } # fatal error

        sl_info <- local({
          # map date-times to dates and warn about loss of precision
          res <- subject_level_dataset
          for (col in date_cols) {
            if (inherits(res[[col]], "POSIXt")) {
              date <- as.Date(res[[col]])
              if (!is.na(date) && as.POSIXct(date) != res[[col]]) {
                # just a warning
                messages[[length(messages) + 1]] <<- paste0("* Date-time column `", col, "` rounded to nearest date.")
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

        timeline_limits <- local({ # start...end, but takes icf and part_end dates into account if available
          min_total <- sl_info[["trt_start_date"]]
          if ("icf_date" %in% names(timeline_info)) {
            icf_date <- sl_info[["icf_date"]]
            if (is.finite(icf_date)) min_total <- icf_date
          }

          max_total <- as.Date(-Inf)
          trt_end_date <- sl_info[["trt_end_date"]]
          if (is.finite(trt_end_date)) {
            max_total <- trt_end_date
          }
          if ("part_end_date" %in% names(timeline_info)) {
            part_end_date <- sl_info[["part_end_date"]]
            if (is.finite(part_end_date)) max_total <- part_end_date
          }
          if (!is.finite(max_total)) max_total <- Sys.Date()

          c(min_total, max_total)
        })

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
            df <- extra_datasets[[plot_params$dataset]]

            if (nrow(df) > 0) {
              last_plot_name <- plot_name
            } else {
              messages[[length(messages) + 1]] <<- paste0("* No Data for ", plot_name, ".")
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
                df <- extra_datasets[[plot_info$dataset]]
                param_mask <- df[[plot_info[["vars"]][["analysis_param"]]]] %in% param
                df <- df[param_mask, ]

                if (nrow(df) > 0) last_param <- param
              }
            } else {
              messages[[length(messages) + 1]] <<- paste("* No Parameter for", plot_name, "selected.")
              value_plots[[plot_name]] <- NULL
            }
          }

          # AE, CM
          for (plot_name in names(range_plots)) {
            plot_params <- range_plots[[plot_name]]
            df <- extra_datasets[[plot_params$dataset]]

            # Column aliases (copied and not renamed to cope with repeat elements)
            vars <- plot_params[["vars"]]
            df[["start_date"]] <- as.Date(df[[vars[["start_date"]]]])
            df[["end_date"]] <- as.Date(df[[vars[["end_date"]]]])
            df[["decode"]] <- df[[vars[["decode"]]]]
            if ("grading" %in% names(vars)) df[["grading"]] <- df[[vars[["grading"]]]]
            if ("serious_ae" %in% names(vars)) {
              # FIXME: This is a temporal patch while we fix the modular API part
              if (!is.logical(df[[vars[["serious_ae"]]]])) {
                df[["serious_ae"]] <- df[[vars[["serious_ae"]]]] == "Y"
              } else {
                df[["serious_ae"]] <- df[[vars[["serious_ae"]]]]
              }
            }

            # wrap decode column
            df[["decode"]] <- strwrap(df[["decode"]],
              width = CONST$decode_max_width_before_wrap_in_characters,
              simplify = FALSE
            ) |> sapply(function(x) paste(x, collapse = "\n"))

            df <- df[intersect(names(df), c("start_date", "end_date", "decode", "grading", "serious_ae"))]

            # Add `arrow_left` and `arrow_right` columns to range_plots
            unknown_start_date <- is.na(df[["start_date"]])
            predate_study_start_date <- df[["start_date"]] < timeline_limits[[1]]

            df[["arrow_left"]] <- as.Date(NA) # no arrow
            df[unknown_start_date | predate_study_start_date, "arrow_left"] <- timeline_limits[[1]]
            df[unknown_start_date | predate_study_start_date, "start_date"] <- timeline_limits[[1]]

            unknown_end_date <- is.na(df[["end_date"]])
            outlast_study_end_date <- timeline_limits[[2]] < df[["end_date"]]

            df[["arrow_right"]] <- as.Date(NA) # no arrow
            df[unknown_end_date | outlast_study_end_date, "arrow_right"] <- timeline_limits[[2]]
            df[unknown_end_date | outlast_study_end_date, "end_date"] <- timeline_limits[[2]]

            df[["tooltip"]] <- build_tooltip(
              tooltip_spec = plot_params[["tooltip"]],
              df = extra_datasets[[plot_params$dataset]],
              color_key = vars[["grading"]],
              palette = palette
            )

            # The last plot to be shown must have x-axis annotations
            annotate_x_axis <- last_plot_name == plot_name && is.null(last_param)

            ggplot <- create_ae_cm_plot(
              data = df,
              x_limits = x_limits,
              palette = palette,
              sl_info = sl_info,
              vline_vars = vline_vars,
              vline_day_numbers = vline_day_numbers,
              x_axis_unit = x_axis_unit,
              x_axis_breaks = x_axis_breaks,
              ref_date = sl_info[["trt_start_date"]],
              plot_name = plot_name,
              annotate_x_axis = annotate_x_axis
            )

            # Attach the height ratio to be passed to `patchwork::plot_layout(heights = ...)`.
            # Count the number of unique terms that will appear on the y-axis, add one for
            # banner space, then divide by six to adjust relative to value plot heights
            # which have a fixed height ratio of 1.
            attr(ggplot, "plot_height") <- (length(unique(df[["decode"]])) + 1) / 6

            # ... [continued from #ipahbo] we just dump stuff into it from inside reactives wherever the
            # variable of interest becomes available. Then ... [continued on tests/testthat/test-all.R:#umeega]
            if (testing) {
              exported_test_data[[paste0("tooltips/", plot_name)]] <<- df[["tooltip"]]
              exported_test_data[[paste0("plot_first_line_color/", plot_name)]] <<-
                ggplot2::ggplot_build(ggplot)$data[[1]][["fill"]][[1]]
              exported_test_data[[paste0("arrow_right/", plot_name)]] <<- df[["arrow_right"]]
              exported_test_data[[paste0("serious_ae/", plot_name)]] <<- df[["serious_ae"]]
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
              df <- extra_datasets[[plot_info$dataset]]

              param_mask <- df[[plot_info[["vars"]][["analysis_param"]]]] %in% param
              df <- df[param_mask, ]

              if (nrow(df) == 0) next

              analysis_indicator_col <- plot_info[["vars"]][["analysis_indicator"]]
              if (!is.null(analysis_indicator_col)) {
                values <- df[[analysis_indicator_col]]
                if (is.character(values)) {
                  values <- as.factor(values)
                  sprintf(
                    paste(
                      "* Analysis indicator column `%s` on dataset `%s` promoted to factor automatically.",
                      "Make it a factor beforehand to avoid this message."
                    ),
                    analysis_indicator_col, plot_info$dataset
                  ) |> warning()
                }

                df[["analysis_indicator"]] <- values

                analysis_indicator_levels <- levels(values)
                if ("" %in% trimws(analysis_indicator_levels)) {
                  sprintf(
                    "* Analysis indicator column `%s` on dataset `%s` Contains empty levels.",
                    analysis_indicator_col, plot_info$dataset
                  ) |> warning()
                }

                levels_wo_palette_colors <- setdiff(levels(values), names(local_palette))
                if (length(levels_wo_palette_colors)) {
                  auto_color <- CONST$color_for_missing_analysis_indicator_levels
                  sprintf(
                    paste0(
                      "* Missing palette colors for analysis indicator column levels: ",
                      paste(levels_wo_palette_colors, collapse = ", "), ". Will paint them as %s."
                    ), auto_color
                  ) |> warning()
                  local_palette[levels_wo_palette_colors] <- auto_color
                }
              }

              df[["tooltip"]] <- local({
                mask <- df[[plot_info$vars[["analysis_param"]]]] == param
                build_tooltip(
                  tooltip_spec = plot_info[["tooltip"]],
                  df = df[mask, ],
                  color_key = analysis_indicator_col,
                  palette = palette
                )
              })

              # The last plot to be shown must have x-axis annotations
              annotate_x_axis <- last_plot_name == plot_name && last_param == param

              ggplot <- create_lb_vs_plot(
                data = df,
                date = plot_info$vars[["analysis_date"]],
                val = plot_info$vars[["analysis_val"]],
                low_limit = plot_info$vars[["range_low_limit"]],
                high_limit = plot_info$vars[["range_high_limit"]],
                param_var = plot_info$vars[["analysis_param"]],
                param_val = param,
                summary_stats = plot_info$vars[["summary_stats"]],
                x_limits = x_limits,
                palette = local_palette,
                sl_info = sl_info,
                vline_vars = vline_vars,
                x_axis_unit = x_axis_unit,
                x_axis_breaks = x_axis_breaks,
                vline_day_numbers = vline_day_numbers,
                ref_date = sl_info[["trt_start_date"]],
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

        if (length(plot_list)) {

          # Extract the 'plot_height' attribute from every plot in the list
          plot_height_ratios <- sapply(plot_list, function(p) attr(p, "plot_height"))

          # Theme application across all plots
          plot_list <- lapply(plot_list, function(p) {
            p + ggplot2::theme(
              plot.margin = ggplot2::margin(0, 0, 1, 0, unit = "pt"),
              plot.background = ggplot2::element_blank(),
              legend.title = ggplot2::element_blank(),
              legend.justification = "top",
              legend.position = "right"
            )
          })

          plots <- patchwork::wrap_plots(plot_list, ncol = 1)

          plots <- plots + patchwork::plot_layout(
            guides = "collect",
            heights = plot_height_ratios
          )
        }

        return(list(plots = plots,
                    messages = messages,
                    plot_height_ratios = plot_height_ratios))
      }

      plots_and_messages <- shiny::reactive({
        if (length(range_plots) > 0 || length(value_plots) > 0) {
          subject_level_dataset <- subject_level_dataset()
          extra_datasets <- v_extra_datasets()
          vs_lb_selected <- local({
            ids <- sanitize_id(names(value_plots))
            res <- Map(function(id) input[[id]], ids)
            can_proceed <- setequal(intersect(ids, shiny::isolate(names(input))), ids)
            shiny::req(isTRUE(can_proceed))
            return(res)
          })

          res <- shiny::maskReactiveContext(
            compute_plots_and_messages(subject_level_dataset, extra_datasets, vs_lb_selected)
          )
        } else {
          res <- list(messages = "* No range or value plots configured")
        }

        if (testing) {
          exported_test_data[["plot_messages"]] <<- res[["messages"]]
        }

        return(res)
      })

      output[["plot"]] <- ggiraph::renderGirafe({
        plots <- plots_and_messages()[["plots"]]
        shiny::req(length(plots) > 0)

        # Calculate plot height by summing the ratios, adding 0.2 for x-axis space, and multiplying result by 2
        plot_height_ratios <- plots_and_messages()[["plot_height_ratios"]]
        plot_height <- (sum(plot_height_ratios) + 0.2) * 2

        ggiraph::girafe(
          ggobj = plots,
          width_svg = 12,
          height_svg = plot_height,
          options = list(
            ggiraph::opts_selection(type = "none"),
            ggiraph::opts_sizing(rescale = TRUE),
            ggiraph::opts_tooltip(css = "border:none; padding:0px;")
          )
        )
      })

      output[["text"]] <- shiny::renderUI({
        messages <- plots_and_messages()[["messages"]]
        shiny::HTML(paste(messages, collapse = "<br>"))
      })
    }
  )
}
