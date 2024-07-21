# NOTE(miguel): This shouldn't be a module. It has almost no functionality. It's only called once.

#' Create user interface for the patient information shiny module of \pkg{dv.papo}
#'
#' @param id A unique ID string to create a namespace. Must match the ID of
#'   \code{patient_info_server()}.
#'
#' @keywords internal
#'
#' @return A shiny \code{uiOutput} element.
patient_info_UI <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui_out"))
}

#' Create server for the patient information shiny module of \pkg{dv.papo}
#' @param id A unique ID string to create a namespace. Must match the ID of
#'   \code{patient_info_UI()}.
#' @param record row object of dataframe: row record for patient.
#' @param column_count A number indicating how many items should be in one row
#'
#' @keywords internal
#'
#' @inheritParams mod_patient_profile
patient_info_server <- function(id, record, subjid_var, column_count = 3) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # parts in ui_out
      output$ui_out <- shiny::renderUI({
        shiny::req(nrow(record()) == 1 && ncol(record()) > 0)

        # content
        digit_len <- 12 %/% column_count
        pp_data <- record()
        pp_cols <- names(pp_data)
        pp_labels <- get_labels(pp_data)

        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::tagList(
              shiny::h3("Patient Information"),
              shiny::fluidRow(
                lapply(seq_len(ncol(pp_data)), function(i) {
                  shiny::column(
                    digit_len,
                    shiny::tags$b(ifelse(is.na(pp_labels[i]), names(pp_data)[i], pp_labels[i])),
                    shiny::tags$b(": "), pp_data[[pp_cols[i]]]
                  )
                })
              )
            ),
            shiny::br()
          )
        )
      })
    }
  )
}
