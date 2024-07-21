dataset_list <- list(
  "demo" = dv.papo:::prep_safety_data(5)
)

dv.manager::run_app(
  data = dataset_list,
  module_list = list("Papo" = dv.papo::mod_patient_profile(module_id = "papo")),
  filter_data = "adsl"
)
