local({
  test_that(
    "serious_ae is mapped from char into logical",
    {
      app <- shinytest2::AppDriver$new(
        app_dir = "apps/logical_map_to/character",
        name = "patient_selector_switching"
      )

      aeser <-
        app$get_values()[["export"]][["mock_app-plot_contents-test_plot_data"]][["serious_ae/Adverse Events Plot"]]
      expect_true(is.logical(aeser))

      app$stop()
    }
  )

  test_that(
    "serious_ae is mapped from factor into logical",
    {
      app <- shinytest2::AppDriver$new(
        app_dir = "apps/logical_map_to/factor",
        name = "patient_selector_switching"
      )

      aeser <-
        app$get_values()[["export"]][["mock_app-plot_contents-test_plot_data"]][["serious_ae/Adverse Events Plot"]]
      expect_true(is.logical(aeser))

      app$stop()
    }
  )
})