is_CI <- isTRUE(as.logical(Sys.getenv("CI")))

wait_for_idle_ms <- 1500 # Cope with slow tests

# validation (S)
vdoc <- local({
  #                      ##########
  # package_name is used # INSIDE # the sourced file below
  #                      ##########
  package_name <- "dv.papo" # hardcoded because devtools::check appears to use a different working directory
  utils_file_path <- system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE)
  source(utils_file_path, local = TRUE)[["value"]]
})
specs <- vdoc[["specs"]]
#  validation (F)

# data & params setup

# params set up
summary <- list(
  vars = c(
    "USUBJID",
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
)

listings <- list(
  "Adverse Events" = list(
    dataset = "adae",
    default_vars = c("USUBJID", "AETERM", "AEDECOD", "AESTDTC", "AEENDTC")
  ),
  "Concomitant Medication" = list(
    dataset = "cm",
    default_vars = c("USUBJID", "CMDECOD", "CMDOSE", "CMSTDTC", "CMENDTC")
  )
)

timeline_info <- c(
  trt_start_date = "TRTSDT",
  trt_end_date = "TRTEDT",
  icf_date = "RFICDT",
  part_end_date = "RFENDT"
)

vline_vars <- list(
  "Informed Consent Day" = "RFICDT",
  "Study Treatment Stop Day" = "TRTEDT"
)

range_plots <- list(
  "Adverse Events Plot" = list(
    dataset = "adae",
    vars = c(
      start_day = "AESTDY",
      end_day = "AEENDY",
      decode = "AEDECOD",
      grading = "AESEV",
      serious_ae = "AESER"
    ),
    tooltip = c(
      ## we have to think about how we can make it simpler
      "AE Term: " = "AEDECOD",
      "AE Reported Term:" = "AETERM",
      "Primary SOC: " = "AESOC",
      "Intensity: " = "ASEV",
      "Serious Event: " = "AESER",
      "add_break" = "",
      "Treatment Start Date: " = "TRTSDT",
      "Treatment Stop Date: " = "TRTEDT",
      "add_break" = "",
      "AE Start Date: " = "AESTDTC",
      "AE Stop Date: " = "AEENDTC",
      "AE Start Day: " = "AESTDY",
      "AE Stop Day: " = "AEENDY",
      "add_break" = "",
      "CM or add. Treatment: " = "ACONTRT",
      "Action Taken with Study Treatment: " = "AACN",
      "Drug Related: " = "AREL",
      "Comment for Discont of Treatment: " = "DCTREAS",
      "DLT Indicator: " = "ADLT"
    )
  ),
  "Concomitant Medication Plot" = list(
    dataset = "cm",
    vars = c(
      start_day = "CMSTDY",
      end_day = "CMENDY",
      decode = "CMDECOD",
      grading = "CMINDC"
    ),
    tooltip = c(
      "Standardized Medication Name: " = "CMDECOD",
      "Indication: " = "CMINDC",
      "Indication Term: " = "CMINDC0",
      "add_break" = "",
      "CM Dose: " = "CMDOSE",
      "CM Dose Unit: " = "CMDOSU",
      "add_break" = "",
      "Treatment Start Date: " = "TRTSDT",
      "Treatment Stop Date: " = "TRTEDT",
      "add_break" = "",
      "CM START Date: " = "CMSTDTC",
      "CM End Date: " = "CMENDTC",
      "CM START Day: " = "CMSTDY",
      "CM END Day: " = "CMENDY",
      "CM START Date Imputation Flag: " = "ASTDTF",
      "CM End Date Imputation Flag: " = "AENDTF",
      "add_break" = "",
      "Planned Sequence of Treatments: " = "TRTSEQP",
      "Actual Sequence of Treatments: " = "TRTSEQA"
    )
  )
)

value_plots <- list(
  "Lab plot" = list(
    dataset = "lb",
    vars = c(
      analysis_day = "LBDY",
      analysis_val = "LBORRES",
      analysis_param = "LBTEST",
      analysis_indicator = "LBNRIND",
      range_low_limit = "LBORNRLO",
      range_high_limit = "LBORNRHI"
    ),
    tooltip = c(
      "Lab Parameter: " = "LBTEST",
      "Lab Test Day: " = "LBDY",
      "Lab Test Visit :" = "VISIT",
      "add_break" = "",
      "High Limit: " = "LBSTNRHI",
      "Lab Standard Value: " = "LBORRES",
      "Lower Limit: " = "LBSTNRLO",
      "add_break" = "",
      "Analysis Indicator: " = "LBNRIND"
    )
  ),
  "Vital Sign Plot" = list(
    dataset = "vs",
    vars = c(
      analysis_day = "VSDY",
      analysis_val = "VSORRES",
      analysis_param = "VSTEST",
      analysis_indicator = "VISIT",
      range_low_limit = NULL,
      range_high_limit = NULL,
      summary_stats = "AVAL_MEAN"
    ),
    tooltip = c(
      "Vital sign Parameter: " = "VSTEST",
      "Vital sign Day: " = "VSDY",
      "Vital sign Visit: " = "VISIT",
      "add_break" = "",
      "Vital sign Value: " = "VSORRES",
      "Vital sign mean value by visits: " = "AVAL_MEAN"
    )
  )
)

# data
testd1 <- dv.papo:::prep_safety_data(5)
testd1_sl <- testd1[["adsl"]]
testd1_extra <- testd1[setdiff(names(testd1), "adsl")]
rm(testd1)

root_app <- shinytest2::AppDriver$new(app_dir = "apps/root/")
root_app_url <- root_app$get_url()

withr::defer(root_app$stop(), teardown_env())
