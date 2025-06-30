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
        filtered_pt_info_data() |>
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
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)

    sel_id <- "papo-patient_selector"

    inputs <- list()
    inputs[[sel_id]] <- "01-701-1028"

    do.call(app$set_inputs, inputs)
    app$wait_for_idle(wait_for_idle_ms)

    bmk_url <- app$get_js("window.location.href")

    bookmark_app <- shinytest2::AppDriver$new(bmk_url)
    bookmark_app$wait_for_idle(wait_for_idle_ms)
    app_input_values <- app$get_values()[["input"]]
    bmk_input_values <- bookmark_app$get_values()[["input"]]

    expect_equal(app_input_values[[sel_id]], bmk_input_values[[sel_id]])
    app$stop()
  }
)

test_that(
  "functional jump-to-subject" |>
    vdoc[["add_spec"]](c(specs$common$jump_to_subject)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)
    app$click(input = "jump")
    app$wait_for_idle(wait_for_idle_ms)
    expected <- "01-701-1033"
    actual <- app$get_value(input = "papo-patient_selector")
    expect_equal(actual, expected)
    app$stop()
  }
)

test_that(
  "helpful configuration feedback" |>
    vdoc[["add_spec"]](c(specs$common$misconfiguration_feedback)),
  {
    app <- shinytest2::AppDriver$new(
      app_dir = "apps/misconfigured_app/",
      name = "misconfigured_app"
    )

    app$wait_for_idle(wait_for_idle_ms)

    validation_errors <- app$get_html(selector = "#papo-validator-ui")
    expect_true(grepl("`subject_level_dataset_name` missing", validation_errors, fixed = TRUE))
    expect_true(grepl("`subjid_var` missing", validation_errors, fixed = TRUE))
    expect_true(grepl("The `sender_ids` - 'random1' - are not available.",
                      validation_errors, fixed = TRUE))

    app$stop()
  }
)

test_that(
  "patient selector updates after dataset change" |>
    vdoc[["add_spec"]](c(specs$common$dataset_change)),
  {
    app <- shinytest2::AppDriver$new(
      app_dir = "apps/patient_selector_switching/",
      name = "patient_selector_switching"
    )

    app$set_inputs(`global_filter-vars` = "SEX")
    app$wait_for_idle()

    # Check if the first patient was selected (initial state)
    testthat::expect_equal(app$get_value(input = "mock_app-patient_selector"), "01-701-1015")

    # Check if the first male patient was selected when filtered accordingly
    app$set_inputs(`global_filter-SEX` = "M")
    # assign pat_id only to suppress print
    pat_id <- app$wait_for_value(input = "mock_app-patient_selector", ignore = list("01-701-1015"))
    testthat::expect_equal(app$get_value(input = "mock_app-patient_selector"), "01-701-1023")

    # Check if no patient is selected when filtered accordingly
    app$set_inputs(`global_filter-SEX` = character(0))
    app$wait_for_idle(2000)
    testthat::expect_equal(app$get_value(input = "mock_app-patient_selector"), "")

    app$stop()
  }
)

column_count_tst <- 4
records <- testd1_sl %>% dplyr::filter(USUBJID == "01-701-1015")

test_that(
  "Subject level information will be shown in patient information section,
  the total number and content of elements is customizable. The default number
  of elements in one row is also customizable." |>
    vdoc[["add_spec"]](c(specs$summary$columns)),
  {
    shiny::testServer(
      patient_info_server,
      arg = list(
        id = "test",
        record = reactive(records),
        subjid_var = "USUBJID",
        column_count = column_count_tst
      ),
      {
        session$setInputs(patient_selector = "01-701-1023")
        div_class <- paste0("col-sm-", 12 / column_count_tst)

        # check if the number of items equals to number of data columns, and
        # if the div class is correct to ensure the number of items per
        # is as expected
        expect_equal(
          length(gregexpr(div_class, output$ui_out$html, fixed = TRUE)[[1]]),
          ncol(records)
        )
      }
    )
  }
)

test_that(
  "Column labels are shown as names in data listings if present" |>
    vdoc[["add_spec"]](c(specs$listings$column_labels)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)
    app$set_inputs("papo-listings-column_selector_adae" = c("USUBJID"))
    app$wait_for_idle(wait_for_idle_ms)

    selected_data <- app$get_values()[["export"]][["papo-listings-test_data"]][["filtered_data"]]
    expect_equal(attr(selected_data[["USUBJID"]], "label"), "Unique Subject Identifier")

    app$stop()
  }
)

test_that(
  "Tab controls allow listing switching" |>
    vdoc[["add_spec"]](c(specs$listings$switching, specs$listings$extra_column_selection)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(duration = wait_for_idle_ms)
    app$set_inputs("papo-listings-data_selector" = "cm")
    app$wait_for_idle()

    target_columns <- c("USUBJID", "CMTRT")
    app$set_inputs("papo-listings-column_selector_cm" = target_columns)
    app$wait_for_idle()

    selected_data <- app$get_values()[["export"]][["papo-listings-test_data"]][["filtered_data"]]
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
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(1500)
    app$set_inputs("papo-listings-data_selector" = "cm")
    app$wait_for_idle(1500)

    target_columns <- c("USUBJID", "CMTRT")
    app$set_inputs("papo-listings-column_selector_cm" = target_columns)
    app$wait_for_idle(1500)

    listing_properties <- jsonlite::fromJSON(app$get_values()[["output"]][["papo-listings-listing"]])[["x"]]

    testthat::expect_equal(listing_properties[["filter"]], "top")
    testthat::expect_equal(listing_properties[["options"]][["searching"]], TRUE)
    testthat::expect_equal(listing_properties[["options"]][["ordering"]], TRUE)
    testthat::expect_equal(listing_properties[["options"]][["buttons"]][["text"]], "Reset Columns Order")

    filter_html <- listing_properties[["filterHTML"]]
    testthat::expect_true(grepl("select multiple", filter_html) && grepl('data-type="factor"', filter_html))

    app$stop()
  }
)

test_that(
  "Column selectors show labels if available" |>
    vdoc[["add_spec"]](c(specs$listings$extra_column_selection_labels)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(duration = wait_for_idle_ms)
    app$set_inputs("papo-listings-data_selector" = "cm")
    app$wait_for_idle()

    select_extra_columns <- app$get_html(selector = "#papo-listings-column_selector")

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
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(duration = wait_for_idle_ms)

    #  ... [continued from R/mod_plots.R:#umeega] we recover the data from the export + slot
    tooltips <- app$get_values()[["export"]][["papo-plot_contents-test_plot_data"]][["tooltips/Adverse Events Plot"]]
    expect_true(length(tooltips) == 3 && all(nchar(tooltips) > 200))

    app$stop()
  }
)

test_that(
  "Mild adverse events are colored as such" |>
    vdoc[["add_spec"]](c(specs$plots$common$palettes, specs$plots$range$grading)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(duration = wait_for_idle_ms)

    target_color <- grDevices::col2rgb(CONST$default_palette[["MILD"]])
    target_color_as_string <- paste0("rgba(", paste(target_color, collapse = ","), ",1)")

    ae_plot_first_color <-
      app$get_values()[["export"]][["papo-plot_contents-test_plot_data"]][["plot_first_line_color/Adverse Events Plot"]]
    expect_equal(target_color_as_string, ae_plot_first_color)

    app$stop()
  }
)

test_that(
  "Message explains the lack of data for plots" |>
    vdoc[["add_spec"]](c(specs$plots$common$no_data_message)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)
    app$click(input = "jump")
    app$wait_for_idle(duration = wait_for_idle_ms)
    expected <- "01-701-1033"
    plot_messages <- app$get_values()[["export"]][["papo-plot_contents-test_plot_data"]][["plot_messages"]]
    expect_contains(plot_messages, "* No Data for Adverse Events Plot.")
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
        subject_var = subjid_var,
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
    vdoc[["add_spec"]](c(plots$value$default_parameter_selection)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)

    # Expected default analysis parameters
    expected_1 <- list(input = list(`papo-plot_contents-Labplot` = c("Alkaline Phosphatase (U/L)",
                                                                     "Bilirubin (umol/L)"),
                                    `papo-plot_contents-VitalSignPlot` = "Weight (kg)"))

    # Select another patient
    app$set_inputs(`papo-patient_selector` = "01-701-1028")
    app$wait_for_idle(wait_for_idle_ms)

    actual_1 <- app$get_values(input = c("papo-plot_contents-Labplot",
                                         "papo-plot_contents-VitalSignPlot"))

    # Expect default analysis parameters have been retained
    testthat::expect_identical(actual_1, expected_1)

    # Select different analysis parameters
    app$set_inputs(`papo-plot_contents-Labplot` = c("Bilirubin (umol/L)", "Calcium (mmol/L)"),
                   `papo-plot_contents-VitalSignPlot` = "Pulse Rate (BEATS/MIN)")
    app$wait_for_idle(wait_for_idle_ms)

    # Capture these selected analysis parameters
    expected_2 <- app$get_values(input = c("papo-plot_contents-Labplot",
                                           "papo-plot_contents-VitalSignPlot"))

    # Select another patient
    app$set_inputs(`papo-patient_selector` = "01-701-1047")
    app$wait_for_idle(wait_for_idle_ms)

    actual_2 <- app$get_values(input = c("papo-plot_contents-Labplot",
                                         "papo-plot_contents-VitalSignPlot"))

    # Expect selected analysis parameters to be retained
    testthat::expect_identical(actual_2, expected_2)

    app$stop()
  }
)

test_that(
  "Events that exceed ranges get labelled with arrows" |>
    vdoc[["add_spec"]](c(specs$plots$range$arrows)),
  {
    app <- shinytest2::AppDriver$new(root_app_url)
    app$wait_for_idle(wait_for_idle_ms)
    arrows <- app$get_values()[["export"]][["papo-plot_contents-test_plot_data"]][["arrow_right/Adverse Events Plot"]]
    expect_equal(arrows, as.Date(c("2014-07-02", "2014-07-02", NA)))
    app$stop()
  }
)

test_that(
  "Color palette is filled when there are missing entries for grading values" |>
    vdoc[["add_spec"]](c(specs$plots$common$palette_is_filled)),
  {

    app <- shinytest2::AppDriver$new(
      app_dir = "apps/grading_palette_colors/",
      name = "grading_colors_app"
    )

    app_grading_vals <- setdiff(app$get_value(export = "gradings"), NA)
    app_filled_palette <- app$get_value(export = "filled_palette")

    expect_true(all(app_grading_vals %in% names(app_filled_palette))) # check all grading vals present in palette.

    grading_palette <- app_filled_palette[app_grading_vals]
    expect_length(grading_palette |> unique(), length(app_grading_vals))

    #check colors were filled.
    # i. check which grading vals had no color assigned in CONST default palette
    unmapped_grading_vals <- setdiff(app_grading_vals, names(dv.papo:::CONST$default_palette))

    # ii. check a color was then assigned.
    expect_length(grading_palette[unmapped_grading_vals], length(unmapped_grading_vals))

  }
)
