mock_foo <- function() {

  # Step 2 - Preprocessing
  data_list <- list()

  data_list$adsl <- pharmaverseadam::adsl |>
    dplyr::mutate(RFICDTM = lubridate::ymd_hm(.data[["RFICDTC"]], truncated = 2),
                  RFXSTDTM = lubridate::ymd_hm(.data[["RFXSTDTC"]], truncated = 2),
                  RFXENDTM = lubridate::ymd_hm(.data[["RFXENDTC"]], truncated = 2),
                  RFPENDTM = lubridate::ymd_hm(.data[["RFPENDTC"]], truncated = 2))

  data_list$adae <- pharmaverseadam::adae
  data_list$adcm <- pharmaverseadam::adcm

  data_list$adlb <- pharmaverseadam::adlb |>
    dplyr::mutate(ADTM = lubridate::ymd_hm(.data[["LBDTC"]], truncated = 2))

  data_list$advs <- pharmaverseadam::advs |>
    dplyr::mutate(ADTM = lubridate::ymd_hm(.data[["VSDTC"]], truncated = 2))

  papo <- mod_patient_profile(
    module_id = "papo",
    subject_level_dataset_name = "adsl",
    subjid_var = "USUBJID",
    # Jumping from other modules is possible, provided that they are included as well:
    #sender_ids = c("clinlines", "lb_lineplot", "vs_lineplot", "qs_lineplot", "listing"),
    summary = list(
      vars = c("ARM", "SITEID", "RACE", "SEX", "RFICDTC", "RFXSTDTC", "RFXENDTC", "RFPENDTC"),
      column_count = 1
    ),
    listings = list(
      "Adverse Events" = list(dataset = "adae", default_vars = NULL),
      "Concomitant Medication" = list(dataset = "adcm", default_vars = NULL),
      "Demographics" = list(dataset = "adsl", default_vars = NULL),
      "Laboratory" = list(dataset = "adlb", default_vars = NULL),
      "Vital Signs" = list(dataset = "advs", default_vars = NULL)
    ),
    plots = list(
      timeline_info = c(
        icf_date = "RFICDTM",
        trt_start_date = "RFXSTDTM",
        trt_end_date = "RFXENDTM",
        part_end_date = "RFPENDTM"
      ),
      #range_plots = list(),
      range_plots = list(
        "Adverse Events Plot" = list(
          dataset = "adae",
          vars = c(
            start_date = "ASTDTM",
            end_date = "AENDTM",
            decode = "AEDECOD",
            grading = "AESEV",
            serious_ae = "AESER"
          ),
          tooltip = c(
            "Preferred Term: " = "AEDECOD",
            "Primary SOC: " = "AESOC",
            "Toxicity Grade: " = "AESEV",
            "Serious Event: " = "AESER",
            "<br><br>AE Start Date: " = "AESTDTC",
            "AE Stop Date: " = "AEENDTC",
            "AE Start Day: " = "AESTDY",
            "AE Stop Day: " = "AEENDY",
            "Action Taken with Study Treatment: " = "AEACN"
          )
        ),
        "Concomitant Medication" = list(
          dataset = "adcm",
          vars = c(
            start_date = "ASTDTM",
            end_date = "AENDTM",
            decode = "CMDECOD",
            grading = "CMINDC"
          ),
          tooltip = c(
            "Standardized Medication Name: " = "CMDECOD",
            "Indication: " = "CMINDC",
            "<br>CM Dose: " = "CMDOSE",
            "CM Dose Unit: " = "CMDOSU",
            "Route of Administration: " = "CMROUTE",
            "<br>CM Start Date: " = "CMSTDTC",
            "CM End Date: " = "CMENDTC",
            "CM Start Day: " = "CMSTDY",
            "CM End Day: " = "CMENDY"
          )
        )
      ),
      value_plots = list(
        "Lab Plot" = list(
          dataset = "adlb",
          vars = c(
            analysis_param = "PARAM",
            analysis_val = "LBSTRESN",
            analysis_date = "ADTM",
            analysis_indicator = "LBNRIND",
            range_low_limit = "LBSTNRLO",
            range_high_limit = "LBSTNRHI"
          ),
          tooltip = c(
            "Lab Parameter:" = "PARAM",
            "Lab Test Day: " = "LBDY",
            "Lab Test Date:" = "ADTM",
            "Lab Test Visit: " = "VISIT",
            "<br>High Limit: " = "LBSTNRHI",
            "Lab Original Value: " = "LBORRES",
            "Lower Limit: " = "LBSTNRLO",
            "Analysis Indicator: " = "LBNRIND"
          ),
          default_analysis_params = c("Anisocytes", "Color", "Ketones", "Ery. Mean Corpuscular Volume (f/L)", "Lymphocytes Abs (10^9/L)")
        ),
        "Vital Signs Plot" = list(
          dataset = "advs",
          vars = c(
            analysis_param = "PARAM",
            analysis_val = "VSSTRESN",
            analysis_date = "ADTM",
            analysis_indicator = "ANRIND",
            range_low_limit = "ANRLO",
            range_high_limit = "ANRHI"
          ),
          tooltip = c(
            "Vital Sign Parameter: " = "PARAM",
            "Vital Sign Day: " = "VSDY",
            "Vital Sign Visit: " = "VISIT",
            "<br>Vital Sign Value: " = "VSSTRESN",
            "Vital Sign Measurements Date/Time: " = "VSDTC"
          ),
          default_analysis_params = c("Pulse Rate (beats/min)", "Weight (kg)")
        )
      ),
      vline_vars = c("Study Treatment End Date" = "RFXENDTM"),
      vline_day_numbers = c(
        "Study Treatment Start Day: Day 1" = 1
      )
    )
  )

  # Step 5 - Run app
  dv.manager::run_app(
    data = list("DUMMY_TRIAL_01" = data_list),  # TODO: adapt name of data
    module_list = list("Patient Profiles" = papo),
    filter_data = "adsl" # global filtering
  )
}
