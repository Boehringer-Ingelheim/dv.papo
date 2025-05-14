
dataset_list <- list("demo" = dv.papo:::prep_safety_data(5))

module_list <- list(
  "Papo" = dv.papo::mod_patient_profile(
    module_id = "mock_app", subjid_var = "USUBJID", sender_ids = NULL,
    subject_level_dataset_name = "adsl",
    summary = list(
      vars = c("SITEID", "AGE", "SEX", "RACE", "ETHNIC", "ARM", "DCREASCD", "TRT01A"),
      column_count = 3
    ),
    plots = "defaults"
  )
)

dv.manager::run_app(
  data = dataset_list,
  module_list = module_list,
  filter_data = "adsl"
)
