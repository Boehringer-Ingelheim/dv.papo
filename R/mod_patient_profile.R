#' Patient Profile Module UI
#'
#' (For use outside of the DaVinci framework)\cr
#' Places the Patient Profile module UI at the call site of this function. A matching call to [mod_patient_profile_server()]
#' is necessary.\cr
#'
#' @param id `[character]` Unique shiny ID. Must match the ID provided to [mod_patient_profile_server()].
#'
#' @return Shiny UI.
#'
#' @seealso [mod_patient_profile()] and [mod_patient_profile_server()]
#' @export
mod_patient_profile_UI <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ui")),
    patient_info_UI(ns("pp_ui_out")),
    patient_listing_UI(ns("listings")),
    patient_plot_UI(ns("plot_contents"))
  )
}


#' Patient Profile Module server
#'
#' (For use outside of the DaVinci framework)\cr
#' Runs the server that populates the UI returned by [mod_patient_profile_UI()].\cr
#' Requires a matching call to that function.
#'
#' @param id `[character]` Unique shiny ID. Must match the ID provided to [mod_patient_profile_UI()].
#' @param subject_level_dataset `[reactive(data.frame)]` Visit-independent subject information.
#' @param extra_datasets `[reactive(data.frame(n))]` Visit-dependent subject datasets.
#' @inheritParams mod_patient_profile
#' @seealso [mod_patient_profile()] and [mod_patient_profile_UI()]
#'
#' @export
mod_patient_profile_server <- function(id, subject_level_dataset, extra_datasets, subjid_var, sender_ids,
                                       summary, listings, plots) {
  local({
    ac <- checkmate::makeAssertCollection() # nolint
    checkmate::assert_class(subject_level_dataset, "reactive", add = ac)
    checkmate::reportAssertions(ac)
  })

  timeline_info <- plots[["timeline_info"]]
  range_plots <- plots[["range_plots"]]
  value_plots <- plots[["value_plots"]]
  vline_vars <- plots[["vline_vars"]]
  vline_day_numbers <- plots[["vline_day_numbers"]]
  palette <- plots[["palette"]]

  # NOTE: simplifies downstream code because list[[optional_missing_element]] returns NULL
  for (i_plot in seq_along(range_plots)) {
    if ("vars" %in% names(range_plots[[i_plot]])) {
      range_plots[[i_plot]][["vars"]] <- as.list(range_plots[[i_plot]][["vars"]])
    }
  }
  for (i_plot in seq_along(value_plots)) {
    if ("vars" %in% names(value_plots[[i_plot]])) {
      value_plots[[i_plot]][["vars"]] <- as.list(value_plots[[i_plot]][["vars"]])
    }
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

      output[["ui"]] <- shiny::renderUI({
        res <- NULL
        if (is.null(summary) && is.null(listings) && is.null(plots)) {
          res <- list(
            shiny::h2("Welcome to dv.papo"),
            shiny::h3("Please provide at least one of these module parameters:"),
            shiny::h4("\u2022 summary", style = "text-indent:2rem;"),
            shiny::h4("\u2022 listings", style = "text-indent:2rem;"),
            shiny::h4("\u2022 plots", style = "text-indent:2rem;"),
            shiny::h3("They are all optional, but if none is provided there won't be much to look at.")
          )
        } else {
          res <- shiny::fluidRow(
            shiny::column(
              CONST$width_of_patient_selector_in_columns,
              shiny::uiOutput(ns("selector"))
            )
          )
        }
        return(res)
      })

      outputOptions(output, "ui", suspendWhenHidden = FALSE)

      output[["selector"]] <- shiny::renderUI({
        subject_level_dataset <- subject_level_dataset()
        shiny::req(subject_level_dataset, cancelOutput = TRUE)
        shiny::selectInput(ns("patient_selector"),
          label = "Select Patient ID:",
          selected = input[["patient_selector"]],
          choices = unique(subject_level_dataset[[subjid_var]])
        )
      })

      outputOptions(output, "selector", suspendWhenHidden = FALSE)

      # change selected patient based on sender_ids
      if (!is.null(sender_ids)) {
        lapply(sender_ids, function(x) {
          shiny::observeEvent(x()[["subj_id"]](), {
            pid_passed <- x()[["subj_id"]]()
            if (!identical(pid_passed, character(0))) {
              shiny::updateSelectInput(
                session = session,
                inputId = "patient_selector",
                selected = pid_passed
              )
            }
          })
        })
      }

      assert <- function(condition, message) shiny::validate(shiny::need(condition, message))

      # patient info section
      # reactive data for patient info
      filtered_pt_info_data <- shiny::reactive({
        df <- subject_level_dataset()
        excess_cols <- setdiff(summary$vars, names(df))
        assert(
          length(excess_cols) == 0,
          paste0(
            "Error: `summary$vars` should be a subset of the columns available in ",
            "the currently loaded subject-level dataframe (",
            paste(names(df), collapse = ", "), ").\n",
            "It contains the following unavailable columns: (",
            paste(excess_cols, collapse = ", "), ")"
          )
        )

        pt_info_data_filter(df, subjid_var, summary$vars, input$patient_selector)
      })

      patient_info_server(
        id = "pp_ui_out",
        record = filtered_pt_info_data,
        subjid_var = subjid_var,
        column_count = summary$column_count
      )

      # reactive data for listings
      filtered_listings_data <- shiny::reactive({
        shiny::req(input[["patient_selector"]])
        dataset_names <- sapply(listings, function(listing) listing[["dataset"]])
        out_list <- lapply(dataset_names, function(name) {
          df <- extra_datasets()[[name]]
          filter_with_mask(df, df[[subjid_var]] == input[["patient_selector"]])
        })
        names(out_list) <- dataset_names
        return(out_list)
      })


      # listings section
      patient_listing_server(
        id = "listings",
        data_list = filtered_listings_data,
        key_value = shiny::reactive(input$patient_selector),
        listings = listings
      )

      # plots section
      filtered_subject_level_dataset <- shiny::reactive({
        subject_id <- input[["patient_selector"]]
        shiny::req(subject_id)
        sl <- subject_level_dataset()
        assert(subjid_var %in% names(sl), sprintf("Error: `subjid_var` variable %s not present in subject-level dataset", subjid_var))
        mask <- sl[[subjid_var]] == subject_id
        assert(sum(mask) > 0, sprintf("Error: Selected patient returns no data"))
        sl <- sl[sl[[subjid_var]] == subject_id, ]
        return(sl)
      })

      filtered_extra_datasets <- shiny::reactive({
        subject_id <- input[["patient_selector"]]
        shiny::req(subject_id)

        res <- list()
        for (name in names(extra_datasets())) {
          df <- extra_datasets()[[name]]
          assert(subjid_var %in% names(df), sprintf("Error: `subjid_var` variable %s not present in dataset %s", subjid_var, name))
          res[[name]] <- df[df[[subjid_var]] == subject_id, ]
          # rescue attributes dropped during subsetting
          for (i in seq_along(res[[name]])) attributes(res[[name]][[i]]) <- attributes(df[[i]])
        }
        return(res)
      })

      patient_plot_server(
        id = "plot_contents", subjid_var,
        subject_level_dataset = filtered_subject_level_dataset,
        timeline_info,
        extra_datasets = filtered_extra_datasets,
        range_plots = range_plots,
        value_plots = value_plots,
        vline_vars = vline_vars, vline_day_numbers = vline_day_numbers,
        palette = palette
      )

      return(NULL)
    }
  )
}

#' Patient Profile Module
#'
#' @description
#'
#' `mod_patient_profile` is a DaVinci Shiny module that displays patient information on a subject-level basis. It
#' consists of three sections: summary, listings and plots.
#' \itemize{
#'   \item The `summary` section shows visit-independent demographic information.
#'   \item The `listings` section offers listings for visit-dependent information.
#'   \item The `plots` section displays charts for events that happen over a span of time (adverse events, concomitant
#'         medications, ...) or line plots of point-like events (laboratory values, vital sign measurements).
#' }
#'
#' <details>
#' <summary>Show/hide usage example </summary>
#' <div>
#' <br>
#' What follows is a sample call to the <span style='font-family:monospace'>mod_patient_profile</span> function.
#' See the <a href="../articles/a00-papo.html">main article of this package</a> for a tutorial on how to parameterize it.
#' <pre>
#' dv.papo::mod_patient_profile(
#'   module_id = "papo",
#'   subject_level_dataset_name = "adsl",
#'   subjid_var = "USUBJID",
#'   summary = list(
#'     vars = c("SUBJID", "SITEID", "ARM", "TRTSDT", "TRTEDT", "AGE", "RACE", "SEX", "BMIBL"),
#'     column_count = 3L
#'   ),
#'   listings = list(
#'     "Concomitant Medication" = list(
#'       dataset = "cm"
#'     ),
#'     "Adverse Events" = list(
#'       dataset = "adae",
#'       default_vars = c("ASTDT", "ASTDY", "AENDT", "AENDY", "AEDECOD", "AESEV")
#'     )
#'   ),
#'   plots = list(
#'     timeline_info = c(trt_start_date = "TRTSDT", trt_end_date = "TRTEDT"),
#'     range_plots = list(
#'       "Concomitant Medication" = list(
#'         dataset = "cm",
#'         vars = c(
#'           start_date = "CMSTDT", end_date = "CMENDT",
#'           decode = "CMDECOD", grading = "CMDECOD"
#'         ),
#'         tooltip = c()
#'       ),
#'       "Adverse Events" = list(
#'         dataset = "adae",
#'         vars = c(
#'           start_date = "ASTDT", end_date = "AENDT", decode = "AEDECOD",
#'           grading = "AESEV", serious_ae = "AESER"
#'         ),
#'         tooltip = c("AE Start Day: " = "ASTDY", "AE End Day: " = "AENDY")
#'       )
#'     ),
#'     value_plots = list(
#'       "Lab Values" = list(
#'         dataset = "lb",
#'         vars = c(
#'           analysis_param = "PARAM", analysis_val = "AVAL", analysis_date = "ADT",
#'           analysis_indicator = "LBNRIND", range_low_limit = "A1LO", range_high_limit = "A1HI"
#'         ),
#'         tooltip = c()
#'       )
#'     ),
#'     vline_vars = c(
#'       "Informed Consent Date" = "RFICDT"
#'     )
#'   )
#' )
#' </pre>
#' </div>
#' </details>
#' <hr>
#'
#'
#' @inheritParams mod_patient_profile_params
#'
#' @return A list composed of the following elements:
#' \itemize{
#'   \item{`ui`}: Shiny UI function.
#'   \item{`server`}: Shiny server function.
#'   \item{`module_id`}: Shiny unique identifier.
#' }
#'
#' @seealso [mod_patient_profile_UI()], [mod_patient_profile_server()]
#' @export
mod_patient_profile <- function(module_id = "",
                                subject_level_dataset_name = NULL,
                                subjid_var = NULL,
                                sender_ids = NULL,
                                summary = NULL,
                                listings = NULL,
                                plots = NULL) {
  args <- as.list(match.call()) # preserves `missing` behavior through reactives, saves us some typing

  # NOTE(miguel): These two lines allow the caller to provide lists whenever `mod_patient_profile_server`
  #               requires atomic arrays
  args <- T_honor_as_array_flag(mod_patient_profile_API, args)
  list2env(args[setdiff(seq_along(args), 1)], environment()) # overwrite current arguments with modified `args`

  mod <- list(
    # UI function
    ui = function(module_id) {
      app_creator_feedback_ui(module_id) # NOTE: original UI gated by app_creator_feedback_server
    },
    # Server function
    server = function(afmm) {
      fb <- shiny::reactive({
        # NOTE: We check the call here and not inside the module server function because:
        #       - app creators interact with the davinci module and not with the ui-server combo, so
        #         errors reported with respect to the module signature will make sense to them.
        #         The module server function might use a different function signature.
        #       - Here we also have access to the unfiltered dataset, which allows us to ensure call
        #         correctness independent of filter state or operation.
        #         Also, as long as the unfiltered dataset does not change (and to date no davinci app
        #         changes it dynamically) this check only runs once at the beginning of the application
        #         and has no further impact on performance.
        #       - "catch errors early"

        # Overwrite first "argument" (the function call, in fact) with the datasets provided to module manager
        names(args)[[1]] <- "datasets"
        args[[1]] <- afmm[["unfiltered_dataset"]]()

        do.call(check_papo_call, args)
      })

      fb_warn <- shiny::reactive(fb()[["warnings"]])
      fb_err <- shiny::reactive(fb()[["errors"]])

      app_creator_feedback_server(
        id = module_id,
        warning_messages = fb_warn,
        error_messages = fb_err,
        ui = dv.papo::mod_patient_profile_UI(module_id)
      )

      filtered_mapped_datasets <- shiny::reactive(
        T_honor_map_to_flag(afmm$filtered_dataset(), mod_patient_profile_API, args)
      )

      subject_level_dataset <- shiny::reactive({
        ds <- filtered_mapped_datasets()[[subject_level_dataset_name]]
        shiny::validate(
          shiny::need(!is.null(ds), paste("Could not find dataset", subject_level_dataset_name))
        )
        return(ds)
      })

      extra_datasets <- shiny::reactive({
        datasets <- filtered_mapped_datasets()
        plot_dataset_names <- names(datasets)
        return(datasets[plot_dataset_names])
      })

      dv.papo::mod_patient_profile_server(
        id = module_id,
        subject_level_dataset = subject_level_dataset,
        extra_datasets = extra_datasets,
        subjid_var = subjid_var,
        sender_ids = lapply(sender_ids, function(x) {
          shiny::reactive(afmm[["module_output"]]()[[x]])
        }),
        summary = summary,
        listings = listings,
        plots = plots
      )
    },

    # Module ID
    module_id = module_id
  )
  return(mod)
}
