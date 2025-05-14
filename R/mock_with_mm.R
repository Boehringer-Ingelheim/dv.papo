#' Run an example for Patient profile integrated in the module manager
#'
#' Launches an example app that shows a patient profile module integrated in the
#' module manager surface. Displays data from the \pkg{safetyData} package.
#'
#' @keywords internal
#'
#' @export
#'
mock_with_mm_app <- function() {
  dataset_list <- list(
    "demo" = prep_safety_data(5),
    "demo2" = prep_safety_data(10)
  )

  module_list <- list(
    "Papo" = mod_patient_profile(
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
      # listings = list(
      #   "Adverse Event" = list(dataset = "adae", default_vars = NULL),
      #   "Concomitant Medication" = list(dataset = "cm", default_vars = NULL)
      # ),
      plots = "defaults"
    )
  )

  dv.manager::run_app(
    data = dataset_list,
    module_list = module_list,
    filter_data = "adsl"
  )
}
