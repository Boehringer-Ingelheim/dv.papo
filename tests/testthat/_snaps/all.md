# ODG chunk replays the selected subject with the selections made in the app

    Code
      cat(odg_patient_profile_chunk(header = "HEADER\n\n", label = "LABEL", subjid = "SUBJID",
        subjid_var = "SUBJID_VAR", subject_level_dataset_name = "SL_DATASET",
        dataset_list_name = "DATASET_LIST", summary = list(vars = "SUMMARY_VAR"),
        listings = list(LISTING_1 = list(dataset = "DATASET_1", default_vars = c(
          "VAR_1", "VAR_2")), LISTING_2 = list(dataset = "DATASET_2", default_vars = "VAR_3")),
        plots = list(value_plots = list(`VALUE PLOT` = list(dataset = "DATASET_3",
          default_analysis_params = "PARAM_1"))), vs_lb_selected = list(VALUEPLOT = "PARAM_2"),
        listing_columns = list(LISTING_1 = "VAR_2", LISTING_2 = NULL)))
    Output
      HEADER
      
      ```{r LABEL, results = "asis"}
      local({
        subjid <- "SUBJID"
        params <- list(summary = list(vars = "SUMMARY_VAR"), listings = list(LISTING_1 = list(dataset = "DATASET_1", default_vars = "VAR_2"), LISTING_2 = list(dataset = "DATASET_2", default_vars = "VAR_3")), plots = list(value_plots = list("VALUE PLOT" = list(dataset = "DATASET_3", default_analysis_params = "PARAM_2"))), dataset_list = DATASET_LIST, subject_level_dataset_name = "SL_DATASET", subjid_var = "SUBJID_VAR")
      
        if (is.null(subjid)) {
          cat(paste0("\\alertwarning{No subject selected for module: ", "LABEL", "}\n\n"))
        } else {
          participant_chunk <- readLines(system.file("batcher/participant_chunk.Rmd", package = "dv.papo", mustWork = TRUE))
          res <- try(
            knitr::knit_child(
              text = gsub("{{LABEL}}", "LABEL-inner_chunk", participant_chunk, fixed = TRUE),
              envir = list2env(list(current_subjid = subjid, params = params)),
              quiet = TRUE
            ),
            silent = TRUE
          )
      
          if (inherits(res, "try-error")) {
            cat(paste0("\\alertwarning{Error producing output document generation for module ", "LABEL", "}\n\n"))
          } else {
            cat(res, sep = "\n")
          }      
        }
      })
      ```

# ODG chunk of the running app replays the selected subject, plot parameters and listing columns

    Code
      cat(app$get_value(output = "odg_chunk"))
    Output
      ```{r papo, results = "asis"}
      local({
        subjid <- "01-701-1028"
        params <- list(summary = list(vars = c("SITEID", "AGE", "SEX", "RACE", "ETHNIC", "ARM", "DCREASCD", "TRT01A"), column_count = 3), listings = list("Adverse Event" = list(dataset = "adae", default_vars = c("USUBJID", "AEDECOD")), "Concomitant Medication" = list(dataset = "cm", default_vars = c("STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMSPID", "CMTRT"))), plots = list(x_axis_unit = "weeks", x_axis_breaks = 20, timeline_info = c(trt_start_date = "TRTSDT", trt_end_date = "TRTEDT", icf_date = "RFICDT", part_end_date = "RFENDT" ), vline_vars = list("Informed Consent Day" = "RFICDT", "Study Treatment Stop Day" = "TRTEDT"), vline_day_numbers = c("Study Treatment Start Day : Day 1" = 1, Test = 50, Test2 = 75, Test3 = 100), range_plots = list("Adverse Events Plot" = list(dataset = "adae", vars = c(start_date = "ASTDT", end_date = "AENDT", decode = "AEDECOD", grading = "AESEV", serious_ae = "AESER"), tooltip = c("AE Term: " = "AEDECOD", "AE Reported Term: " = "AETERM", "Primary SOC: " = "AESOC", "Intensity: " = "AESEV", "Serious Event: " = "AESER",  "AE Start Date: " = "ASTDT", "AE Stop Date: " = "AENDT", "AE Start Day: " = "ASTDY", "AE Stop Day: " = "AENDY")), "Concomitant Medication Plot" = list(dataset = "cm", vars = c(start_date = "CMSTDT", end_date = "CMENDT", decode = "CMDECOD", grading = "CMINDC"), tooltip = c("Standardized Medication Name: " = "CMDECOD", "Indication: " = "CMINDC", "CM Dose: " = "CMDOSE", "CM Dose Unit: " = "CMDOSU", "CM START Date: " = "CMSTDTC", "CM End Date: " = "CMENDTC", "CM START Day: " = "CMSTDY", "CM END Day: " = "CMENDY" ))), value_plots = list("Lab plot" = list(dataset = "lb", vars = c(analysis_param = "PARAM", analysis_val = "AVAL", analysis_date = "ADT", analysis_indicator = "ANRIND", range_low_limit = "A1LO", range_high_limit = "A1HI"), tooltip = c("Lab Parameter: " = "PARAM", "Lab Test Date: " = "ADT", "Lab Test Visit: " = "AVISIT", "<br>High Limit: " = "A1HI", "Lab Standard Value: " = "AVAL", "Lower Limit: " = "A1LO", "<br>Analysis Indicator: " = "ANRIND"), default_analysis_params = "Bilirubin (umol/L)"), "Vital Sign Plot" = list(     dataset = "vs", vars = c(analysis_param = "PARAM", analysis_val = "AVAL", analysis_date = "ADT", analysis_indicator = "VISIT", summary_stats = "AVAL_MEAN"), tooltip = c("Vital sign Parameter: " = "PARAM", "Vital sign Date: " = "ADT", "Vital sign Visit: " = "AVISIT", "<br>Vital sign Value: " = "AVAL", "Vital sign mean value by visits: " = "AVAL_MEAN"), default_analysis_params = "Weight (kg)"))), dataset_list = filtered_dataset_list, subject_level_dataset_name = "adsl", subjid_var = "USUBJID")
      
        if (is.null(subjid)) {
          cat(paste0("\\alertwarning{No subject selected for module: ", "papo", "}\n\n"))
        } else {
          participant_chunk <- readLines(system.file("batcher/participant_chunk.Rmd", package = "dv.papo", mustWork = TRUE))
          res <- try(
            knitr::knit_child(
              text = gsub("{{LABEL}}", "papo-inner_chunk", participant_chunk, fixed = TRUE),
              envir = list2env(list(current_subjid = subjid, params = params)),
              quiet = TRUE
            ),
            silent = TRUE
          )
      
          if (inherits(res, "try-error")) {
            cat(paste0("\\alertwarning{Error producing output document generation for module ", "papo", "}\n\n"))
          } else {
            cat(res, sep = "\n")
          }      
        }
      })
      ```

