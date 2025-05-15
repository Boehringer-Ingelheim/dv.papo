# default values for plots
plot_default_vals <- list(
  timeline_info = c(
    trt_start_date = "TRTSDT",
    trt_end_date = "TRTEDT",
    icf_date = "RFICDT", # optional
    part_end_date = "RFENDT" # optional
  ),
  vline_vars = c(
    "Informed Consent Day" = "RFICDT", # because optional above
    # "Study Treatment Start Day" = "TRTSDT", #added by me
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

# mandatory parameters for plots parameter
plot_mandatory_params <- list(
  timeline_info = c("trt_start_date", "trt_end_date"),
  vline_vars = NA,
  range_plots = list(
    "Adverse Events Plot" = list(
      dataset = NA, vars = c("start_date", "end_date", "decode"), tooltip = NA
    ),
    "Concomitant Medication Plot" = list(
      dataset = NA, vars = c("start_date", "end_date", "decode"), tooltip = NA
    )
  ),
  value_plots = list(
    "Lab plot" = list(
      dataset = NA, vars = c("analysis_param", "analysis_val", "analysis_date"), tooltip = NA
    ),
    "Vital Sign Plot" = list(
      dataset = NA, vars = c("analysis_param", "analysis_val", "analysis_date"), tooltip = NA
    )
  )
)

CONST <- poc(
  width_of_patient_selector_in_columns = 4,
  decode_max_width_before_wrap_in_characters = 15,
  color_for_missing_analysis_indicator_levels = "darkgray",
  default_palette = c(
    `REFERENCE RANGE` = "#91eec2",
    `Reference range` = "#91eec2",
    `reference range` = "#91eec2",
    `NORMAL` = "green4", `LOW` = "red3", `HIGH` = "red2",
    `Normal` = "green4", `Low` = "red3", `High` = "red2",
    `normal` = "green4", `low` = "red3", `high` = "red2",
    `MILD` = "lightgreen", `MODERATE` = "gold1", `SEVERE` = "red",
    `Mild` = "lightgreen", `Moderate` = "gold1", `Severe` = "red",
    `mild` = "lightgreen", `moderate` = "gold1", `severe` = "red"
  ),
  plot_mandatory_params = plot_mandatory_params,
  plot_default_vals = plot_default_vals
)

rm(plot_mandatory_params, plot_default_vals)
