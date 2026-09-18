local({
  root_app <- start_app_driver(quote(dv.papo::mock_patient_profile_app(auto_update_query_string = TRUE)))
  root_app_url <- if (!is.null(root_app)) root_app$get_url() else NULL

  test_that(
    "Column labels are shown as names in data listings if present" |>
      vdoc[["add_spec"]](c(specs$listings$column_labels)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)

      column_selector_adae_id <- ns_id("papo", ID$LISTINGS, sprintf(LID$COLUMN_SELECTOR_FMT, "adae"))
      test_data_id <- ns_id("papo", ID$LISTINGS, "test_data")

      app$wait_for_js(sprintf("document.querySelector('#%s') !== null", column_selector_adae_id))
      app$set_inputs(!!column_selector_adae_id := c("USUBJID"))

      app$wait_for_value(export = test_data_id)
      selected_data <- app$get_value(export = test_data_id)[["filtered_data"]]

      expect_equal(attr(selected_data[["USUBJID"]], "label"), "Unique Subject Identifier")

      app$stop()
    }
  )

  test_that(
    "functional subject_selector" |>
      vdoc[["add_spec"]](c(specs$common$subject_selector)),
    {
      test_pid <- "01-701-1028"
      test_tab <- "cm"

      shiny::testServer(mod_patient_profile_server, args = list(
        id = "test",
        subject_level_dataset = shiny::reactive(testd1_sl),
        extra_datasets = shiny::reactive(testd1_extra),
        subjid_var = "USUBJID",
        sender_ids = c(reactive(list("subj_id" = reactive("test")))),
        summary = summary,
        listings = listings,
        plots = list(
          timeline_info = timeline_info,
          vline_vars = vline_vars,
          range_plots = range_plots,
          value_plots = value_plots,
          palette = NULL
        )
      ), {
        session$setInputs(patient_selector = test_pid)
        session$flushReact()
        expect_equal(
          pt_summary_data()[["result"]] |>
            select_columns_by_name("USUBJID") |>
            unlist(use.names = FALSE),
          test_pid
        )
      })
    }
  )

  test_that(
    "functional bookmarking" |>
      vdoc[["add_spec"]](c(specs$common$bookmarking)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)

      sel_id <- ns_id("papo", ID$PATIENT_SELECTOR)
      test_pid <- "01-701-1028"
      default_pid <- "01-701-1015"

      app$wait_for_value(input = sel_id)

      app$set_inputs(!!sel_id := test_pid)
      app$wait_for_value(input = sel_id, ignore = list(NULL, "", "01-701-1015"))

      bmk_url <- app$get_js("window.location.href")
      bookmark_app <- shinytest2::AppDriver$new(bmk_url)
      bookmark_app$wait_for_value(input = sel_id, ignore = list(NULL, "", "01-701-1015"))

      app_input_value <- app$get_value(input = sel_id)
      bmk_input_value <- bookmark_app$get_value(input = sel_id)

      expect_equal(app_input_value, test_pid)
      expect_equal(bmk_input_value, test_pid)

      app$stop()
      bookmark_app$stop()
    }
  )

  test_that(
    "functional jump-to-subject" |>
      vdoc[["add_spec"]](c(specs$common$jump_to_subject)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)

      sel_id <- ns_id("papo", ID$PATIENT_SELECTOR)

      app$wait_for_value(input = sel_id)

      app$click(input = "jump")
      app$wait_for_value(input = sel_id, ignore = list(NULL, "", "01-701-1015"))

      expected <- "01-701-1033"
      actual <- app$get_value(input = sel_id)
      expect_equal(actual, expected)

      app$stop()
    }
  )

  test_that(
    "helpful configuration feedback" |>
      vdoc[["add_spec"]](c(specs$common$misconfiguration_feedback)),
    {
      skip_if_not_running_shiny_tests()

      app <- start_app_driver(quote({
        dataset_list <- list("demo" = dv.papo:::prep_safety_data(5))

        dv.manager::run_app(
          data = dataset_list,
          module_list = list(
            "Papo" = dv.papo::mod_patient_profile(module_id = "papo", sender_ids = "random1")
          ),
          filter_data = "adsl"
        )
      }))

      app$wait_for_js(
        "document.querySelector('div[value=\"papo\"]').innerText.includes('non-empty string')",
        timeout = 30000
      )

      validation_errors <- app$get_html(selector = 'div[value="papo"]')

      expect_match(validation_errors, "`subject_level_dataset_name` should be a non-empty string", fixed = TRUE)
      expect_match(validation_errors, "`subjid_var` should be a non-empty string", fixed = TRUE)
      expect_match(validation_errors, "The `sender_ids` - 'random1' - are not available.", fixed = TRUE)

      app$stop()
    }
  )

  test_that(
    "patient selector updates after dataset change" |>
      vdoc[["add_spec"]](c(specs$common$dataset_change)),
    {
      skip_if_not_running_shiny_tests()

      app <- start_app_driver(quote({
        dataset_list <- list(
          "demo" = dv.papo:::prep_safety_data(5),
          "demo2" = dv.papo:::prep_safety_data(10)
        )

        module_list <- list(
          "Papo" = dv.papo::mod_patient_profile(
            module_id = "mock_app",
            subjid_var = "USUBJID",
            sender_ids = NULL,
            subject_level_dataset_name = "adsl",
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
              timeline_info = c(
                trt_start_date = "TRTSDT",
                trt_end_date = "TRTEDT",
                icf_date = "RFICDT",
                part_end_date = "RFENDT"
              ),
              vline_vars = c(
                "Informed Consent Date" = "RFICDT",
                "Study Treatment Stop Date" = "TRTEDT"
              ),
              vline_day_numbers = c(
                "Study Treatment Start Day : Day 1" = 1
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
                    "Serious Event: " = "AESER",
                    "<br>AE Start Date: " = "ASTDT",
                    "AE Stop Date: " = "AENDT"
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
                    "<br>CM Dose: " = "CMDOSE",
                    "CM Dose Unit: " = "CMDOSU",
                    "<br>CM START Date: " = "CMSTDTC",
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
                    analysis_date = "ADT",
                    analysis_val = "AVAL",
                    analysis_param = "PARAM",
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
                    analysis_date = "ADT",
                    analysis_val = "AVAL",
                    analysis_param = "PARAM",
                    analysis_indicator = "AVISIT",
                    summary_stats = "AVAL_MEAN"
                  ),
                  tooltip = c(
                    "Vital sign Parameter: " = "PARAM",
                    "Vital sign Date: " = "ADT",
                    "Vital sign Visit: " = "VISIT",
                    "<br>Vital sign Value: " = "AVAL",
                    "Vital sign mean value by visits: " = "AVAL_MEAN"
                  )
                )
              )
            )
          )
        )

        dv.manager::run_app(
          data = dataset_list,
          module_list = module_list,
          filter_data = "adsl"
        )
      }))

      sel_id <- ns_id("mock_app", ID$PATIENT_SELECTOR)

      filter_state_json_fmt <- r"--(
  {
    "filters": {
      "datasets_filter": {
        "children": []
      },
      "subject_filter": {
        "children": [
          {
            "kind": "row_operation",
            "operation": "and",
            "children": [
              {
                "kind": "filter",
                "dataset": "adsl",
                "operation": "select_subset",
                "variable": "SEX",
                "values": [%s],
                "include_NA": true
              }
            ]
          }
        ]
      }
    },
    "dataset_list_name": "demo"
  }
      )--"

      # Check if the first male patient was selected when filtered accordingly
      app$set_inputs(
        `filter-filter_state_json_input` = sprintf(filter_state_json_fmt, '"M"'),
        allow_no_input_binding_ = TRUE, priority_ = "event"
      )
      app$wait_for_idle(duration = wait_for_idle_ms)
      testthat::expect_equal(app$get_value(input = sel_id), "01-701-1023")

      # Check if no patient is selected when filtered accordingly
      app$set_inputs(
        `filter-filter_state_json_input` = sprintf(filter_state_json_fmt, ""),
        allow_no_input_binding_ = TRUE, priority_ = "event"
      )
      pat_id <- app$wait_for_value(input = sel_id, ignore = list(NULL, "01-701-1023"))
      testthat::expect_equal(app$get_value(input = sel_id), "")

      app$stop()
    }
  )

  column_count_tst <- 4

  test_that(
    "Subject level information will be shown in patient information section,
    the total number and content of elements is customizable. The default number
    of elements in one row is also customizable." |>
      vdoc[["add_spec"]](c(specs$summary$columns)),
    {
      test_pid <- "01-701-1015"
      summary_tst <- utils::modifyList(summary, list(column_count = column_count_tst))

      shiny::testServer(mod_patient_profile_server, args = list(
        id = "test",
        subject_level_dataset = shiny::reactive(testd1_sl),
        extra_datasets = shiny::reactive(testd1_extra),
        subjid_var = "USUBJID",
        sender_ids = c(reactive(list("subj_id" = reactive("test")))),
        summary = summary_tst,
        listings = listings,
        plots = list(
          timeline_info = timeline_info,
          vline_vars = vline_vars,
          range_plots = range_plots,
          value_plots = value_plots,
          palette = NULL
        )
      ), {
        session$setInputs(patient_selector = test_pid)
        session$flushReact()

        div_class <- paste0("col-sm-", 12 %/% column_count_tst)

        # check if the number of items equals the number of configured summary vars, and
        # if the div class is correct to ensure the number of items per row
        # matches the customized column_count
        expect_equal(
          length(gregexpr(div_class, output[[ID$SUMMARY]]$html, fixed = TRUE)[[1]]),
          length(summary_tst[["vars"]])
        )
      })
    }
  )

  test_that(
    "Tab controls allow listing switching" |>
      vdoc[["add_spec"]](c(specs$listings$switching, specs$listings$extra_column_selection)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(duration = wait_for_idle_ms)

      dataset_selector_id <- ns_id("papo", ID$LISTINGS, LID$DATASETNAME_SELECTOR)
      column_selector_cm_id <- ns_id("papo", ID$LISTINGS, sprintf(LID$COLUMN_SELECTOR_FMT, "cm"))
      test_data_id <- ns_id("papo", ID$LISTINGS, "test_data")

      app$set_inputs(!!dataset_selector_id := "cm")
      app$wait_for_idle()

      target_columns <- c("USUBJID", "CMTRT")
      app$set_inputs(!!column_selector_cm_id := target_columns)
      app$wait_for_idle()

      selected_data <- app$get_values()[["export"]][[test_data_id]][["filtered_data"]]
      testthat::expect_equal(names(selected_data), target_columns)
      testthat::expect_equal(unique(selected_data[["USUBJID"]]), "01-701-1015")
      testthat::expect_equal(
        unique(selected_data[["CMTRT"]]),
        c("ASPIRIN", "CALCIUM", "HYDROCORTISONE", "NEOSPORIN /USA/", "PREMARIN", "TYLENOL")
      )
      app$stop()
    }
  )

  test_that(
    "The content of data listing can be filtered by inputing related text in the
    search box on the top right of the listing. It can also be filtered for each
    column by inputing/selecting values from the text box below the column
    header." |>
      vdoc[["add_spec"]](c(specs$listings$filtering, specs$listings$sorting)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(1500)

      dataset_selector_id <- ns_id("papo", ID$LISTINGS, LID$DATASETNAME_SELECTOR)
      column_selector_cm_id <- ns_id("papo", ID$LISTINGS, sprintf(LID$COLUMN_SELECTOR_FMT, "cm"))
      listing_id <- ns_id("papo", ID$LISTINGS, LID$LISTING)

      app$set_inputs(!!dataset_selector_id := "cm")
      app$wait_for_idle(1500)

      target_columns <- c("USUBJID", "CMTRT")
      app$set_inputs(!!column_selector_cm_id := target_columns)
      app$wait_for_idle(1500)

      listing_properties <- jsonlite::fromJSON(app$get_values()[["output"]][[listing_id]])[["x"]]

      testthat::expect_equal(listing_properties[["filter"]], "top")
      testthat::expect_equal(listing_properties[["options"]][["searching"]], TRUE)
      testthat::expect_equal(listing_properties[["options"]][["ordering"]], TRUE)
      testthat::expect_equal(listing_properties[["options"]][["buttons"]][["text"]], "Reset Rows Order")

      filter_html <- listing_properties[["filterHTML"]]
      testthat::expect_true(grepl("select multiple", filter_html) && grepl('data-type="factor"', filter_html))

      app$stop()
    }
  )

  test_that(
    "Column selectors show labels if available" |>
      vdoc[["add_spec"]](c(specs$listings$extra_column_selection_labels)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(duration = wait_for_idle_ms)

      # adae is the default-selected listing and carries labelled demographic columns (unlike cm)
      column_selector_adae_id <- ns_id("papo", ID$LISTINGS, sprintf(LID$COLUMN_SELECTOR_FMT, "adae"))

      select_extra_columns <- app$get_html(selector = paste0("#", column_selector_adae_id))

      matches <- gregexpr('data-subtext="([^"]*)"', select_extra_columns)
      data_subtexts <- regmatches(select_extra_columns, matches)[[1]]
      data_subtexts <- gsub('data-subtext="|"', "", data_subtexts)
      expect_contains(data_subtexts, c("Study Identifier", "Age", "Race"))

      app$stop()
    }
  )

  test_that(
    "Message explains the lack of data for listings" |>
      vdoc[["add_spec"]](c(specs$listings$no_data_message)),
    {
      skip("DT listings provide a 'No data' message by default")
    }
  )

  test_that(
    "Column selectors show labels if available" |>
      vdoc[["add_spec"]](c(specs$plots$common$tooltips)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(duration = wait_for_idle_ms)

      plots_test_data_id <- ns_id("papo", ID$PLOTS, "test_plot_data")

      #  ... [continued from R/mod_plots.R:#umeega] we recover the data from the export + slot
      tooltips <- app$get_values()[["export"]][[plots_test_data_id]][["tooltips/Adverse Events Plot"]]
      expect_true(length(tooltips) == 3 && all(nchar(tooltips) > 200))

      app$stop()
    }
  )

  test_that(
    "Mild adverse events are colored as such" |>
      vdoc[["add_spec"]](c(specs$plots$common$palettes, specs$plots$range$grading)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(duration = wait_for_idle_ms)

      target_color <- CONST$DEFAULT_PALETTE[["MILD"]]
      plots_test_data_id <- ns_id("papo", ID$PLOTS, "test_plot_data")

      ae_plot_first_color <-
        app$get_values()[["export"]][[plots_test_data_id]][["plot_first_line_color/Adverse Events Plot"]]
      expect_equal(target_color, ae_plot_first_color)

      app$stop()
    }
  )

  test_that(
    "Message explains the lack of data for plots" |>
      vdoc[["add_spec"]](c(specs$plots$common$no_data_message)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(wait_for_idle_ms)

      patient_selector_id <- ns_id("papo", ID$PATIENT_SELECTOR)
      plots_test_data_id <- ns_id("papo", ID$PLOTS, "test_plot_data")
      plot_messages_key <- PID$PLOT_MESSAGES
      labplot_id <- ns_id("papo", ID$PLOTS, "Labplot")
      vitalsignplot_id <- ns_id("papo", ID$PLOTS, "VitalSignPlot")

      app$set_inputs(!!patient_selector_id := "01-701-1429")
      app$wait_for_idle(duration = wait_for_idle_ms)

      plot_messages <- app$get_values()[["export"]][[plots_test_data_id]][[plot_messages_key]]
      expect_contains(plot_messages, "* No Data for Adverse Events Plot.")
      expect_contains(plot_messages, "* No Data for Concomitant Medication Plot.")
      expect_no_match(plot_messages, "\\* No Parameter for Lab plot selected\\.")
      expect_no_match(plot_messages, "\\* No Parameter for Vital Sign Plot selected\\.")

      # Deselect all lab and vitals parameters
      app$set_inputs(!!labplot_id := character(0), !!vitalsignplot_id := character(0))
      app$wait_for_idle(duration = wait_for_idle_ms)

      plot_messages <- app$get_values()[["export"]][[plots_test_data_id]][[plot_messages_key]]
      expect_contains(plot_messages, "* No Parameter for Lab plot selected.")
      expect_contains(plot_messages, "* No Parameter for Vital Sign Plot selected.")

      app$stop()
    }
  )

  test_that(
    "Parameters for value plots can be selected from a drop-down menu" |>
      vdoc[["add_spec"]](c(specs$plots$value$parameter_selection)),
    {
      test_pid <- "01-701-1028"
      subjid_var <- "USUBJID"
      lb_tests <- c("Alanine Aminotransferase", "Chloride", "Basophils")
      vs_tests <- c("Pulse Rate", "Diastolic Blood Pressure", "Systolic Blood Pressure")

      shiny::testServer(
        patient_plot_server,
        arg = list(
          id = "test",
          subjid_var = subjid_var,
          subject_level_dataset = shiny::reactive(testd1_sl),
          timeline_info = timeline_info,
          extra_datasets = shiny::reactive(testd1_extra),
          range_plots = range_plots,
          value_plots = value_plots,
          vline_vars = vline_vars,
          palette = NULL
        ),
        {
          session$setInputs(patient_selector = test_pid)
          session$setInputs(Lab_plot = lb_tests)
          session$setInputs(Vital_Sign_Plot = vs_tests)
          session$flushReact()
          expect_equal(input[["Lab_plot"]], lb_tests)
          expect_equal(input[["Vital_Sign_Plot"]], vs_tests)
        }
      )
    }
  )

  test_that(
    "default parameter selection" |>
      vdoc[["add_spec"]](c(specs$plots$value$default_parameter_selection)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(wait_for_idle_ms)

      patient_selector_id <- ns_id("papo", ID$PATIENT_SELECTOR)
      labplot_id <- ns_id("papo", ID$PLOTS, "Labplot")
      vitalsignplot_id <- ns_id("papo", ID$PLOTS, "VitalSignPlot")

      # Expected default analysis parameters
      expected_1 <- list(input = rlang::list2(
        !!labplot_id := c("Alkaline Phosphatase (U/L)", "Bilirubin (umol/L)"),
        !!vitalsignplot_id := "Weight (kg)"
      ))

      # Select another patient
      app$set_inputs(!!patient_selector_id := "01-701-1028")
      app$wait_for_idle(wait_for_idle_ms)

      actual_1 <- app$get_values(input = c(labplot_id, vitalsignplot_id))

      # Expect default analysis parameters have been retained
      testthat::expect_identical(actual_1, expected_1)

      # Select different analysis parameters
      app$set_inputs(
        !!labplot_id := c("Bilirubin (umol/L)", "Calcium (mmol/L)"),
        !!vitalsignplot_id := "Pulse Rate (BEATS/MIN)"
      )
      app$wait_for_idle(wait_for_idle_ms)

      # Capture these selected analysis parameters
      expected_2 <- app$get_values(input = c(labplot_id, vitalsignplot_id))

      # Select another patient
      app$set_inputs(!!patient_selector_id := "01-701-1047")
      app$wait_for_idle(wait_for_idle_ms)

      actual_2 <- app$get_values(input = c(labplot_id, vitalsignplot_id))

      # Expect selected analysis parameters to be retained
      testthat::expect_identical(actual_2, expected_2)

      app$stop()
    }
  )

  test_that(
    "Events that exceed ranges get labelled with arrows" |>
      vdoc[["add_spec"]](c(specs$plots$range$arrows)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app_url)
      app$wait_for_idle(wait_for_idle_ms)
      plots_test_data_id <- ns_id("papo", ID$PLOTS, "test_plot_data")
      arrows <- app$get_values()[["export"]][[plots_test_data_id]][[paste0(PCONF_FIELDS$ARROW_RIGHT, "/Adverse Events Plot")]]
      expect_equal(arrows, as.Date(c("2014-07-02", "2014-07-02", NA)))
      app$stop()
    }
  )

  test_that(
    "Color palette is filled when there are missing entries for grading values" |>
      vdoc[["add_spec"]](c(specs$plots$common$palette_is_filled)),
    {
      skip_if_not_running_shiny_tests()

      app <- start_app_driver(quote({
        dataset_list <- list("demo" = dv.papo:::prep_safety_data(5))

        module_list <- list(
          "Papo" = dv.papo::mod_patient_profile(
            module_id = "grading_app", subjid_var = "USUBJID", sender_ids = NULL,
            subject_level_dataset_name = "adsl",
            summary = list(
              vars = c("SITEID", "AGE", "SEX", "RACE", "ETHNIC", "ARM", "DCREASCD", "TRT01A"),
              column_count = 3
            ),
            plots = list(
              timeline_info = c(
                trt_start_date = "TRTSDT",
                trt_end_date = "TRTEDT",
                icf_date = "RFICDT", # optional
                part_end_date = "RFENDT" # optional
              ),
              vline_vars = c(
                "Informed Consent Day" = "RFICDT", # because optional above
                "Study Treatment Stop Day" = "TRTEDT"
              ),
              vline_day_numbers = c("Study Treatment Start Day : Day 1" = 1), # optional
              range_plots = list(
                "Adverse Events Plot" = list(
                  dataset = "adae",
                  vars = c(
                    start_date = "ASTDT",
                    end_date = "AENDT",
                    decode = "AEDECOD",
                    grading = "AESEV", # optional
                    serious_ae = "AESER" # optional
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
        )

        dv.manager::run_app(
          data = dataset_list,
          module_list = module_list,
          filter_data = "adsl"
        )
      }))
      app$wait_for_idle(wait_for_idle_ms)

      app_grading_vals <- setdiff(app$get_value(export = "gradings"), NA)
      app_filled_palette <- app$get_value(export = "filled_palette")

      expect_true(all(app_grading_vals %in% names(app_filled_palette))) # check all grading vals present in palette.

      grading_palette <- app_filled_palette[app_grading_vals]
      expect_length(grading_palette |> unique(), length(app_grading_vals))

      #check colors were filled.
      # i. check which grading vals had no color assigned in CONST default palette
      unmapped_grading_vals <- setdiff(app_grading_vals, names(dv.papo:::CONST$DEFAULT_PALETTE))

      # ii. check a color was then assigned.
      expect_length(grading_palette[unmapped_grading_vals], length(unmapped_grading_vals))

      app$stop()
    }
  )
})
