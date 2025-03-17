#' Create user interface for patient plot shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#' \code{patient_plot_server()}.
#' @param title character: Title of plot module
#'
#' @keywords internal
#'
patient_plot_UI <- function(id) { # nolint
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("ui"))
}


#' Create server for patient plot shiny module of \pkg{dv.papo}
#'
#' TODO: Document params (inherit them)
#'
#' @keywords internal
#'
#'
patient_plot_server <- function(id, subject_var,
                                subject_level_dataset, timeline_info,
                                extra_datasets, range_plots, value_plots,
                                vline_vars, vline_day_numbers, palette) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

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
          plotly::plotlyOutput(ns("plot")),
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
          selectors[[length(selectors) + 1]] <- shiny::column(
            3,
            shinyWidgets::pickerInput(
              inputId = ns(selector_id),
              label = paste0("Please Select Parameter for ", plot_name, ":"),
              choices = choices,
              selected = shiny::isolate(input[[selector_id]]),
              multiple = TRUE,
              options = list("live-search" = TRUE, "actions-box" = TRUE)
            )
          )
        }

        return(selectors)
      })

      compute_plots_and_messages <- function(subject_level_dataset, extra_datasets, vs_lb_selected) {
        # TODO: Remove the messages already guarded against by check_papo_call
        messages <- character(0)
        plots <- list()

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
              if (as.POSIXct(date) != res[[col]]) {
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
          min_total <- subject_level_dataset[[timeline_info[["trt_start_date"]]]]
          if ("icf_date" %in% names(timeline_info)) {
            icf_date <- subject_level_dataset[[timeline_info[["icf_date"]]]]
            if (is.finite(icf_date)) min_total <- icf_date
          }

          max_total <- as.Date(-Inf)
          trt_end_date <- subject_level_dataset[[timeline_info[["trt_end_date"]]]]
          if (is.finite(trt_end_date)) {
            max_total <- trt_end_date
          }
          if ("part_end_date" %in% names(timeline_info)) {
            part_end_date <- subject_level_dataset[[timeline_info[["part_end_date"]]]]
            if (is.finite(part_end_date)) max_total <- part_end_date
          }
          if (!is.finite(max_total)) max_total <- Sys.Date()

          c(min_total, max_total)
        })

        x_limits <- local({ # we need to compute combined limits first because ggplot+plotly need them before layout calculation
          # the +/-1 avoids clipping the left and right arrows on the plot
          diff <- timeline_limits[[2]] - timeline_limits[[1]]
          offset_left <- min(-1L, round(-diff / 10))
          offset_right <- max(1L, round(diff / 10))
          c(timeline_limits[[1]] + offset_left, timeline_limits[[2]] + offset_right)
        })

        plot_list <- local({
          res <- list()

          build_tooltip <- function(tooltip_spec, df) {
            # TODO: color = .data[[plots[[set]]$vars$grading]] ??

            res <- list()
            for (i_row in seq_len(nrow(df))) {
              res_elem <- ""
              for (i_line in seq_along(tooltip_spec)) {
                prefix <- names(tooltip_spec)[[i_line]] # NOTE: App creators can specify breaking lines through '<br>'
                col <- tooltip_spec[[i_line]]
                res_elem <- paste0(res_elem, prefix, df[[col]][[i_row]], "<br>")
              }
              res[[i_row]] <- res_elem
            }

            return(res)
          }

          # AE, CM
          for (plot_name in names(range_plots)) {
            plot_params <- range_plots[[plot_name]]
            df <- extra_datasets[[plot_params$dataset]]

            if (nrow(df) == 0) {
              messages[[length(messages) + 1]] <<- paste0("* No Data for ", plot_name, ".")
              next
            }

            # Column aliases (copied and not renamed to cope with repeat elements)
            vars <- plot_params[["vars"]]
            df[["start_date"]] <- as.Date(df[[vars[["start_date"]]]])
            df[["end_date"]] <- as.Date(df[[vars[["end_date"]]]])
            df[["decode"]] <- df[[vars[["decode"]]]]
            if ("grading" %in% names(vars)) df[["grading"]] <- df[[vars[["grading"]]]]
            if ("serious_ae" %in% names(vars)) df[["serious_ae"]] <- df[[vars[["serious_ae"]]]]

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

            y_count <- length(unique(df[["decode"]]))
            height <- max(y_count + 2, 5)

            ggplot <- create_ae_cm_plot(
              data = df, x_limits = x_limits, palette = palette,
              sl_info, vline_vars = vline_vars, vline_day_numbers = vline_day_numbers,
              ref_date = sl_info[["trt_start_date"]]
            )

            tooltip_text <- build_tooltip(
              tooltip_spec = plot_params$tooltip,
              df = extra_datasets[[plot_params$dataset]]
            )

            ggplot <- ggplot + ggplot2::aes(text = tooltip_text)

            plot <- plotly::ggplotly(
              ggplot,
              height = height * 32, tooltip = c("text")
            )
            plot <- plotly::add_annotations(
              plot,
              text = plot_name,
              x = 0,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "left",
              yanchor = "top",
              xshift = 0,
              yshift = 22,
              showarrow = FALSE,
              font = list(size = 15)
            )

            # ... [continued from #ipahbo] we just dump stuff into it from inside reactives wherever the
            # variable of interest becomes available. Then ... [continued on tests/testthat/test-all.R:#umeega]
            if (testing) {
              exported_test_data[[paste0("tooltips/", plot_name)]] <<- tooltip_text
              exported_test_data[[paste0("plot_first_line_color/", plot_name)]] <<-
                plot[["x"]][["data"]][[1]][["line"]][["color"]]
              exported_test_data[[paste0("arrow_right/", plot_name)]] <<- df[["arrow_right"]]
            }

            # tweak legend manually - adapted from dv.papo 1; maybe there's a documented way of achieving the same?
            extract_first <- function(s) sub("\\(([^,]*).*\\)", "\\1", s)

            for (i in seq_along(plot$x$data)) {
              s <- plot$x$data[[i]]$name
              if (!is.null(s)) plot$x$data[[i]]$name <- extract_first(s)
            }
            res[[length(res) + 1]] <- plot
          }

          # VS, LAB
          for (plot_name in names(value_plots)) {
            plot_info <- value_plots[[plot_name]]
            params <- vs_lb_selected[[sanitize_id(plot_name)]]

            if (length(params) == 0) {
              messages[[length(messages) + 1]] <<- paste("* No Parameter for", plot_name, "selected.")
              next
            }

            for (i_param in seq_along(params)) {
              local_palette <- palette

              param <- params[[i_param]]
              df <- extra_datasets[[plot_info$dataset]]

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

              param_mask <- df[[plot_info[["vars"]][["analysis_param"]]]] %in% param
              df <- df[param_mask, ]

              ggplot <- create_lb_vs_plot(
                data = df,
                date = plot_info$vars[["analysis_date"]],
                val = plot_info$vars[["analysis_val"]],
                low_limit = plot_info$vars[["range_low_limit"]],
                high_limit = plot_info$vars[["range_high_limit"]],
                param = plot_info$vars[["analysis_param"]],
                summary_stats = plot_info$vars[["summary_stats"]],
                x_limits = x_limits,
                palette = local_palette,
                sl_info, vline_vars,
                vline_day_numbers = vline_day_numbers,
                ref_date = sl_info[["trt_start_date"]]
              )

              tooltip_text <- local({
                mask <- df[[plot_info$vars[["analysis_param"]]]] == param # TODO: this could precede the create_lb_vs_plot call #peizai
                build_tooltip(tooltip_spec = plot_info$tooltip, df = df[mask, ])
              })

              ggplot <- ggplot + ggplot2::aes(text = tooltip_text)

              plot <- plotly::ggplotly(ggplot, height = 160, tooltip = c("text"))

              plot <- plotly::add_annotations(
                plot,
                text = ifelse(i_param == 1, plot_name, ""),
                x = 0,
                y = 1,
                yref = "paper",
                xref = "paper",
                xanchor = "left",
                yanchor = "top",
                xshift = 0,
                yshift = 22,
                showarrow = FALSE,
                font = list(size = 15)
              )

              res[[length(res) + 1]] <- plot
            }
          }

          return(res)
        })

        if (length(plot_list)) {
          # stack plots
          heights <- sapply(plot_list, function(x) x[["height"]], simplify = "array")
          heights <- heights + 80 / length(heights) # (HACK to cope with plotly) Divide space dedicated to footer equally among all plots
          plots <- plotly::subplot(plot_list,
                                   shareX = TRUE, titleX = TRUE, nrows = length(plot_list), margin = 0,
                                   heights = heights / sum(heights)
          )

          x_limits_z <- x_limits - sl_info[["trt_start_date"]]
          plots <- silence_warning(
            plotly::layout(plots, height = sum(heights), xaxis = list(range = x_limits_z)),
            "Specifying width/height in layout() is now deprecated.\nPlease specify in ggplotly() or plot_ly()"
            # Bypass deprecation warning, because the alternative is using the size of one of the subplots to dictate the size
            # of the stacked plot. This issue predates the covid pandemic and remains unresolved:
            # https://github.com/plotly/plotly.R/issues/1613
          )

          # strip unwanted portions of text (e.g. "(SCREENING 1,1)" -> "SCREENING 1") # TODO: where do these come from?
          plots <- local({
            first_elem_before_comma_in_parens_re <- "\\(([^,]*),.*\\)"
            for (i in seq_along(plots$x$data)) {
              plots$x$data[[i]]$name <- sub(first_elem_before_comma_in_parens_re, "\\1", plots$x$data[[i]]$name)
            }
            plots
          })

          # remove legend duplicates # TODO: avoid including them in the first place?
          plots <- local({
            names_seen <- character(0)
            for (i in seq_along(plots$x$data)) {
              name <- plots$x$data[[i]][["name"]]
              if (name %in% names_seen || name == "<no grading>") { # no grading is set if no grading column was defined
                # but it shouldn't be displayed in the legend, since it doesn't come out of the data and might confuse users
                plots$x$data[[i]]$showlegend <- FALSE
              } else {
                names_seen <- c(names_seen, name)
              }
            }
            plots
          })

          # title also gets duplicated when subplot joins legends # TODO: Avoid it to begin with
          plots[["x"]][["layout"]][["legend"]][["title"]][["text"]] <- "<b> Legend </b>"
        }

        return(list(plots = plots, messages = messages))
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

      output[["plot"]] <- plotly::renderPlotly({
        plots <- plots_and_messages()[["plots"]]
        shiny::req(length(plots) > 0)
        return(plots)
      })

      output[["text"]] <- shiny::renderUI({
        messages <- plots_and_messages()[["messages"]]
        shiny::HTML(paste(messages, collapse = "<br>"))
      })
    }
  )
}
