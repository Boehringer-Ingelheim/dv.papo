
# TODO(miguel): Avoid having to install the package when testing locally by following
# dv.biomarker.general/browse/tests/testthat/app/app.R?at=dev
# to run the current version of the code

# wrapper around mock_patient_profile_app that updates URL to reflect state of inputs
shiny::shinyApp(
  ui = dv.papo::mock_patient_profile_UI,
  server = function(input, output, session) {
    dv.papo::mock_patient_profile_server(input, output, session)

    shiny::observe({
      shiny::reactiveValuesToList(input)
      session$doBookmark()
    })
    shiny::onBookmarked(shiny::updateQueryString)
  },
  enableBookmarking = "url"
)
