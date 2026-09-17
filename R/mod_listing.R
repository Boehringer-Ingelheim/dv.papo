LID <- poc(
  CONTAINER = "container",
  DATASETNAME_SELECTOR_CONTAINER = "dataset_selector_container",
  COLUMN_SELECTOR_CONTAINER = "column_selector_container",
  LISTING_CONTAINER = "listing_container",
  DATASETNAME_SELECTOR = "dataset_selector",
  COLUMN_SELECTOR_FMT = "column_selector_%s",
  LISTING = "listing"
)

LCONF_FIELDS <- poc(
  DATASET_NAME = "dataset",
  DEFAULT_VARS = "default_vars"
)

#' Create user interface for patient listings shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#' \code{patient_plot_server()}.
#'
#' @keywords internal
#'
patient_listing_UI <- function(id) { # nolint
  ns <- shiny::NS(id)    
  shiny::uiOutput(ns(LID$CONTAINER))
}

#' Create server for patient listings shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#' \code{patient_listing_UI()}.
#' @param dataset_list List of data frames containing data for each listing of selected patient.
#' @param subject_id Character: Value of selected patient
#' @inheritParams mod_patient_profile
#'
#' @keywords internal
#'
patient_listing_server <- function(id, dataset_list, subject_id, listings) {
  # Replace by Alias. Allows parameter inheritance but clarifies following code, original name is too vague.
  listings_conf <- listings
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

      output[[LID$CONTAINER]] <- shiny::renderUI({
        shiny::req(length(listings_conf) > 0)

        shiny::tagList(
          # Header, and domain selection
          shiny::h3("Data Listings"),
          shiny::uiOutput(ns(LID$DATASETNAME_SELECTOR_CONTAINER)),

          # Column selection
          shiny::uiOutput(ns(LID$COLUMN_SELECTOR_CONTAINER)),

          # Data listings
          shiny::uiOutput(ns(LID$LISTING_CONTAINER)),

          shiny::br()
        )
      })

      output[[LID$DATASETNAME_SELECTOR_CONTAINER]] <- shiny::renderUI({
        shiny::req(dataset_list())
        choices <- c("N/A" = NA_character_)
        if (length(listings_conf) > 0) {
          choices <- sapply(listings_conf, function(listing_conf) {
            listing_conf[[LCONF_FIELDS$DATASET_NAME]]
          })
        }

        shinyWidgets::radioGroupButtons(
          inputId = ns(LID$DATASETNAME_SELECTOR),
          label = "Select Domain:",
          selected = shiny::isolate(input[[LID$DATASETNAME_SELECTOR]]),
          choices = choices
        )
      })

      column_selector_first_pass <- TRUE

      output[[LID$COLUMN_SELECTOR_CONTAINER]] <- shiny::renderUI({
        shiny::req(dataset_list())

        ui <- list()

        for (this_listing_conf in listings_conf) {
          dataset_name <- this_listing_conf[[LCONF_FIELDS$DATASET_NAME]]

          dataset <- dataset_list()[[dataset_name]]
          choices <- names(dataset)
          labels <- sapply(choices, function(col) {
            res <- attr(dataset[[col]], "label")
            if (is.null(res)) {
              res <- ""
            }
            return(res)
          })

          default_vars <- this_listing_conf[[LCONF_FIELDS$DEFAULT_VARS]]
          if (column_selector_first_pass) {
            # if app creator specifies no columns, default selection is first six columns
            if (length(default_vars) == 0) {
              default_vars <- utils::head(choices, n = 6)
            }
          }

          col_sel_id <- sprintf(LID$COLUMN_SELECTOR_FMT, dataset_name)
          selected <- intersect(default_vars, choices)

          if (is.null(selected)) {
            selected <- shiny::isolate(input[[col_sel_id]])
          }

          ui[[length(ui) + 1]] <- shiny::conditionalPanel(
            sprintf("input.%s=='%s'", LID$DATASETNAME_SELECTOR, dataset_name),
            ns = ns,
            shinyWidgets::pickerInput(
              ns(col_sel_id),
              label = "Select Extra Columns:",
              choices = choices,
              selected = selected,
              choicesOpt = list(subtext = labels),
              multiple = TRUE,
              options = list("live-search" = TRUE, "actions-box" = TRUE)
            )
          )
        }

        if (column_selector_first_pass) {
          # Consume defaults after first pass
          for (i_listing in seq_along(listings_conf)) {
            listings_conf[[i_listing]][[LCONF_FIELDS$DEFAULT_VARS]] <<- NULL
          }
          column_selector_first_pass <<- FALSE
        }

        return(ui)
      })

      listing_contents <- shiny::reactive({
        r_dataset_name <- input[[LID$DATASETNAME_SELECTOR]]
        shiny::req(checkmate::test_string(r_dataset_name))

        r_dataset_list <- dataset_list()
        shiny::req(checkmate::test_list(r_dataset_list, min.len = 1))
        shiny::req(r_dataset_list)

        dataset <- r_dataset_list[[r_dataset_name]]
        shiny::req(checkmate::test_data_frame(dataset))

        columns <- input[[sprintf(LID$COLUMN_SELECTOR_FMT, r_dataset_name)]]

        subset_data <- dataset[columns]

        col_labels <- get_labels(subset_data, columns)

        # replace NA labels with column Names
        for (i in seq_along(col_labels)) {
          if (is.na(col_labels[i])) {
            col_labels[i] <- paste0(columns[i], " (No Label)")
          }
        }

        scroll_y <- if (nrow(subset_data) > 10) "300" else FALSE

        if (testing) {
          exported_test_data[["filtered_data"]] <<- subset_data
        }

        # turn character type into factor (to offer column filter options)        
        for (var_name in names(subset_data)) {
          if (is.character(subset_data[[var_name]])) {
            subset_data[[var_name]] <- as.factor(subset_data[[var_name]])
          }
        }

        # styler: off
        restore_original_order_js <- r"----(
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
        )----" |>
          structure(class = c("character", "JS_EVAL"))
        # styler: on

        res <- DT::datatable(
          data = subset_data,
          colnames = col_labels,
          selection = "single",
          rownames = TRUE,
          filter = "top",
          extensions = "Buttons",
          options = list(
            searching = TRUE,
            paging = FALSE,
            scrollX = TRUE,
            scrollY = scroll_y,
            ordering = TRUE,
            columnDefs = list(list(className = "dt-center", targets = "_all")),
            dom = "Bfrtip",
            buttons = list(list(
              extend = "",
              text = "Reset Rows Order",
              action = restore_original_order_js,
              className = "btn btn-light"
            ))
          )
        )

        return(res)
      })

      output[[LID$LISTING_CONTAINER]] <- shiny::renderUI({
        # DT::dataTableOutput can occupy a lot of space even if unpopulated, so we gate
        # its inclusion by first checking if there is content to display in it
        shiny::req(listing_contents())
        return(DT::dataTableOutput(ns(LID$LISTING)))
      })

      output[[LID$LISTING]] <- DT::renderDataTable({
        return(listing_contents())
      })
    }
  )
}
