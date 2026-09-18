local({
  logical_map_app_expr <- quote({
    dataset_list <- list("demo" = dv.papo:::prep_safety_data(5))
    dataset_list$demo$adae$AESER <- sample(c("Y", "N"), nrow(dataset_list$demo$adae), replace = TRUE)

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
  })

  test_that(
    "serious_ae is mapped from char into logical",
    {
      skip_if_not_running_shiny_tests()

      app <- start_app_driver(logical_map_app_expr)
      app$wait_for_idle(duration = wait_for_idle_ms)

      plots_test_data_id <- ns_id("mock_app", ID$PLOTS, "test_plot_data")
      aeser <-
        app$get_values()[["export"]][[plots_test_data_id]][[paste0(PCONF_FIELDS$SERIOUS_AE, "/Adverse Events Plot")]]
      expect_true(is.logical(aeser))

      app$stop()
    }
  )

  test_that(
    "serious_ae is mapped from factor into logical",
    {
      skip_if_not_running_shiny_tests()

      app <- start_app_driver(logical_map_app_expr)
      app$wait_for_idle(duration = wait_for_idle_ms)

      plots_test_data_id <- ns_id("mock_app", ID$PLOTS, "test_plot_data")
      aeser <-
        app$get_values()[["export"]][[plots_test_data_id]][[paste0(PCONF_FIELDS$SERIOUS_AE, "/Adverse Events Plot")]]
      expect_true(is.logical(aeser))

      app$stop()
    }
  )
})
