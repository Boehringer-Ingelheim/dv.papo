#' Create user interface for patient listings shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#' \code{patient_plot_server()}.
#'
#' @keywords internal
#'
patient_listing_UI <- function(id) { # nolint
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste(".btn-papo_listing_data_selector_status:active,",
                ".btn-papo_listing_data_selector_status.active,",
                ".open>.btn-papo_listing_data_selector_status.dropdown-toggle {
                    color: #fff;
                    background-color: #274AB3;
                    border-color: #274AB3;
                }")
          ),
      )
    ),
    shiny::uiOutput(ns("ui"))
  )
}

#' Create server for patient listings shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#' \code{patient_listing_UI()}.
#' @param data_list List of data frames containing data for each listing of selected patient.
#' @param key_value Character: Value of selected patient
#' @inheritParams mod_patient_profile
#'
#' @keywords internal
#'
patient_listing_server <- function(id, data_list, key_value, listings) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      testing <- isTRUE(getOption("shiny.testmode"))
      if (testing) {
        exported_test_data <- list()
        shiny::exportTestValues(test_data = exported_test_data)
      }

      shiny::setBookmarkExclude(c(
        "table_cell_clicked",
        "table_rows_current",
        "table_rows_all",
        "table_search",
        "table_search_columns",
        "table_rows_selected",
        "table_cells_selected",
        "table_columns_selected",
        "table_state"
      ))

      max_width <- 12

      output[["ui"]] <- shiny::renderUI({
        shiny::req(length(listings) > 0)
        ui <- list(
          shiny::fluidRow(
            shiny::column(
              max_width,
              shiny::h3("Data Listings"),
              shiny::uiOutput(ns("data_selector_ui"))
            ),
            # column selector
            shiny::uiOutput(ns("column_selector")),
            shiny::column(
              max_width,
              # data listing
              shiny::uiOutput(ns("listing_ui"))
            )
          ),
          shiny::br()
        )
        return(ui)
      })

      output[["data_selector_ui"]] <- shiny::renderUI({
        shiny::req(data_list())
        choices <- c("N/A" = NA_character_)
        if (length(listings) > 0) choices <- sapply(listings, function(listing) listing[["dataset"]])

        shinyWidgets::radioGroupButtons(
          inputId = ns("data_selector"),
          label = "Select Domain:",
          status = "papo_listing_data_selector_status",
          selected = shiny::isolate(input[["data_selector"]]),
          choices = choices
        )
      })

      column_selector_first_pass <- TRUE

      output[["column_selector"]] <- shiny::renderUI({
        shiny::req(data_list())

        ui <- list()

        for (listing in listings) {
          dataset_name <- listing[["dataset"]]

          dataset <- data_list()[[dataset_name]]
          choices <- names(dataset)
          labels <- sapply(choices, function(col) {
            res <- attr(dataset[[col]], "label")
            if (is.null(res)) res <- ""
            return(res)
          })

          default_vars <- listing[["default_vars"]]
          if (column_selector_first_pass) {
            # if app creator specifies no columns, default selection is first six columns
            if (length(default_vars) == 0) default_vars <- utils::head(choices, n = 6)
          }

          sel_id <- paste0("column_selector_", dataset_name)

          selected <- default_vars
          if (is.null(selected)) selected <- shiny::isolate(input[[sel_id]])

          ui[[length(ui) + 1]] <- shiny::column(
            max_width,
            shiny::conditionalPanel(
              paste0("input.data_selector == ", "'", dataset_name, "'"),
              ns = ns,
              shinyWidgets::pickerInput(ns(sel_id),
                label = "Select Extra Columns",
                choices = choices,
                selected = selected,
                choicesOpt = list(subtext = labels),
                multiple = TRUE,
                options = list("live-search" = TRUE, "actions-box" = TRUE)
              )
            )
          )
        }

        if (column_selector_first_pass) {
          # Consume defaults after first pass
          for (i_listing in seq_along(listings)) {
            listings[[i_listing]][["default_vars"]] <<- NULL
          }
          column_selector_first_pass <<- FALSE
        }

        return(ui)
      })

      listing_contents <- shiny::reactive({
        dataset_name <- input[["data_selector"]]
        shiny::req(dataset_name)
        datasets <- data_list()
        shiny::req(datasets)
        data <- datasets[[dataset_name]]
        shiny::req(data)

        columns <- input[[paste0("column_selector_", dataset_name)]]

        data <- data[columns]

        col_labels <- get_labels(data, columns)

        # replace NA labels with column Names
        for (i in seq_along(col_labels)) {
          if (is.na(col_labels[i])) col_labels[i] <- paste0(columns[i], " (No Label)")
        }

        scroll_y <- if (nrow(data) > 10) "300" else FALSE

        if (testing) {
          exported_test_data[["filtered_data"]] <<- data
        }

        # turn character type into factor (to offer column filter options)
        for (name in names(data)) {
          if (is.character(data[[name]])) {
            data[[name]] <- as.factor(data[[name]])
          }
        }

        # styler: off
        restore_original_order_js <-  r"----(
          function(e, dt, node, config) {
            dt.iterator('table', function(s) {
              s.aaSorting.length = 0;
              s.aiDisplay.sort(function(a,b) {
                 return a-b;
              });
              s.aiDisplayMaster.sort(function(a,b) {
                 return a-b;
              });
            }).draw();
          }
        )----" |> structure(class = c("character", "JS_EVAL"))
        # styler: on

        res <- DT::datatable(
          data = data, colnames = col_labels,
          selection = "single", rownames = TRUE,
          filter = "top", extensions = "Buttons",
          options = list(
            searching = TRUE, paging = FALSE,
            scrollX = TRUE, scrollY = scroll_y, ordering = TRUE,
            columnDefs = list(list(className = "dt-center", targets = "_all")),
            dom = "Bfrtip", buttons = list(list(
              extend = "collection",
              text = "Reset Columns Order",
              action = restore_original_order_js
            ))
          )
        )

        return(res)
      })

      output[["listing_ui"]] <- shiny::renderUI({
        # DT::dataTableOutput can occupy a lot of space even if unpopulated, so we gate
        # its inclusion by first checking if there is content to display in it
        shiny::req(listing_contents())
        return(DT::dataTableOutput(ns("listing")))
      })

      output[["listing"]] <- DT::renderDataTable({
        return(listing_contents())
      })
    }
  )
}
