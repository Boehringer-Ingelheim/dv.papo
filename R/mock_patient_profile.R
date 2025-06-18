#' Mock UI for papo module
#'
#' @param id A unique identifier.
#'
#' @return Shiny UI for the papomodule.
#'
#' @keywords internal
#'
#' @export
#'
mock_patient_profile_UI <- function(request) { # nolint
  ui <- shiny::fluidPage(
    shiny::tags$h1("Patient profile", class = "mod-title"),
    shiny::actionButton("jump", "Click to jump"),
    mod_patient_profile_UI(id = "papo")
  )
  return(ui)
}


#' Mock server for papo module
#'
#' Uses adsl, adae, and adcm data of roche dataset.
#'
#' @param input,output,session Internal parameters for shiny.
#'
#' @keywords internal
#'
#' @export
#'
mock_patient_profile_server <- function(input, output, session) {
  data_list <- prep_safety_data()

  subject_level_dataset <- shiny::reactive(data_list[["adsl"]])
  extra_datasets <- shiny::reactive(data_list[setdiff(names(data_list), "adsl")])

  # exclude all non-papo inputs from bookmarking
  # plotly does not namespace its inputs, so this can't be done from inside the module
  shiny::observe({
    all <- names(input)
    papo_filter <- startsWith(all, "papo")
    non_papo_inputs <- all[!papo_filter]
    shiny::setBookmarkExclude(non_papo_inputs)
  })

  mod_patient_profile_server(
    id = "papo",
    subject_level_dataset = subject_level_dataset,
    extra_datasets = extra_datasets,
    subjid_var = "USUBJID",
    sender_ids = list("test" = shiny::reactive({ # recreate sender_ids structure of Module Manager
      list(subj_id = shiny::reactive({
        shiny::req(input$jump)
        "01-701-1033"
      }))
    })),
    summary = list(
      vars = c(
        "SITEID",
        "AGE",
        "SEX",
        "RACE",
        "ETHNIC",
        "ARM",
        "DCREASCD",
        "TRT01A"
      ),
      column_count = 3
    ),
    listings = list(
      "Adverse Event" = list(dataset = "adae", default_vars = NULL),
      "Concomitant Medication" = list(dataset = "cm", default_vars = NULL)
    ),
    plots = list(
      x_unit = "weeks", # Week or Date
      x_axis_breaks = 20, # x axis by
      timeline_info = c(
        trt_start_date = "TRTSDT",
        trt_end_date = "TRTEDT",
        icf_date = "RFICDT",
        part_end_date = "RFENDT"
      ),
      vline_vars = list(
        "Informed Consent Day" = "RFICDT",
        "Study Treatment Stop Day" = "TRTEDT"
      ),
      vline_day_numbers = c(
        "Study Treatment Start Day : Day 1" = 1,
        "Test" = 50,
        "Test2" = 75,
        "Test3" = 100
      ),
      range_plots = list(
        "Adverse Events Plot" = list(
          dataset = "adae",
          vars = c(
            start_date = "ASTDT",
            end_date = "AENDT",
            decode = "AEDECOD",
            grading = "AESEV",
            serious_ae = "AESER"
          ),
          tooltip = c(
            "AE Term: " = "AEDECOD",
            "AE Reported Term: " = "AETERM",
            "Primary SOC: " = "AESOC",
            "Intensity: " = "AESEV",
            "Serious Event: " = "AESER",
            "AE Start Date: " = "ASTDT",
            "AE Stop Date: " = "AENDT",
            "AE Start Day: " = "ASTDY",
            "AE Stop Day: " = "AENDY"
          )
        ),
        "Concomitant Medication Plot" = list(
          dataset = "cm",
          vars = c(
            start_date = "CMSTDT",
            end_date = "CMENDT",
            decode = "CMDECOD",
            grading = "CMINDC"
          ),
          tooltip = c(
            "Standardized Medication Name: " = "CMDECOD",
            "Indication: " = "CMINDC",
            "CM Dose: " = "CMDOSE",
            "CM Dose Unit: " = "CMDOSU",
            "CM START Date: " = "CMSTDTC",
            "CM End Date: " = "CMENDTC",
            "CM START Day: " = "CMSTDY",
            "CM END Day: " = "CMENDY"
          )
        )
      ),
      value_plots = list(
        "Lab plot" = list(
          dataset = "lb",
          vars = c(
            analysis_param = "PARAM",
            analysis_val = "AVAL",
            analysis_date = "ADT",
            analysis_indicator = "ANRIND",
            range_low_limit = "A1LO",
            range_high_limit = "A1HI"
          ),
          tooltip = c(
            "Lab Parameter: " = "PARAM",
            "Lab Test Date: " = "ADT",
            "Lab Test Visit :" = "AVISIT",
            "<br>High Limit: " = "A1HI",
            "Lab Standard Value: " = "AVAL",
            "Lower Limit: " = "A1LO",
            "<br>Analysis Indicator: " = "ANRIND"
          )
        ),
        "Vital Sign Plot" = list(
          dataset = "vs",
          vars = c(
            analysis_param = "PARAM",
            analysis_val = "AVAL",
            analysis_date = "ADT",
            analysis_indicator = "VISIT",
            range_low_limit = NULL,
            range_high_limit = NULL,
            summary_stats = "AVAL_MEAN"
          ),
          tooltip = c(
            "Vital sign Parameter: " = "PARAM",
            "Vital sign Date: " = "ADT",
            "Vital sign Visit: " = "AVISIT",
            "<br>Vital sign Value: " = "AVAL",
            "Vital sign mean value by visits: " = "AVAL_MEAN"
          )
        )
      )
    )
  )
}



#' Run papo mock
#'
#' \code{mock_patient_profile_app()} runs the \pkg{dv.papo} module
#' with dummy data.
#'
#' @keywords internal
#'
#' @export
#'
mock_patient_profile_app <- function(auto_update_query_string = FALSE) {
  server <- mock_patient_profile_server
  enable_bookmarking <- "disable"

  if (auto_update_query_string) {
    server <- function(input, output, session) {
      shiny::observe({
        shiny::reactiveValuesToList(input) # Trigger this observer every time an input changes
        session$doBookmark()
      })
      shiny::onBookmarked(shiny::updateQueryString)

      return(mock_patient_profile_server(input, output, session))
    }
    enable_bookmarking <- "url"
  }

  shiny::shinyApp(
    ui = mock_patient_profile_UI,
    server = server,
    enableBookmarking = enable_bookmarking
  )
}
