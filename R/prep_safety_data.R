#' Prepare safety data
#'
#' Modifiy safetyData's adsl, adae, and adcm dummy data for easy use within
#' \pkg{dv.clinlines}.
#'
#' @param n Number of rows to select from the adsl dataset. The first n rows will be
#' taken. Used to reduce runtime during development.
#'
#' @return A list of three data frames.
#'
#' @keywords internal
#'
#' @importFrom rlang .data

prep_safety_data <- function(n = 200) {
  if (!requireNamespace("dplyr")) {
    stop("This function requires `dplyr`")
  }

  adsl_info <- safetyData::adam_adsl[1:n, ]
  adsl_info[["TRTSDT"]] <- robust_ymd(adsl_info[["TRTSDT"]]) |> structure(label = "Treatment Start Date")
  adsl_info[["TRTEDT"]] <- robust_ymd(adsl_info[["TRTEDT"]], round_up = TRUE) |> structure(label = "Treatment End Date")
  adsl_info[["RFICDT"]] <- robust_ymd(adsl_info[["RFSTDTC"]]) |> structure(label = "Informed Consent Date")
  adsl_info[["TRTDUR"]] <- as.numeric(adsl_info[["TRTDUR"]]) |> structure(label = "Treatment duration (days)")

  subjects <- unique(adsl_info[["USUBJID"]])

  # adae
  known_adverse_event_start_date_mask <- !is.na(safetyData::adam_adae[["ASTDT"]])
  selected_subject_mask <- safetyData::adam_adae[["USUBJID"]] %in% subjects
  adae_info <- safetyData::adam_adae[known_adverse_event_start_date_mask & selected_subject_mask, ]
  aeser_attributes <- attributes(adae_info[["AESER"]])
  adae_info[["AESER"]] <- c("Y" = TRUE, "N" = FALSE)[adae_info[["AESER"]]]
  attributes(adae_info[["AESER"]]) <- aeser_attributes

  # cm
  known_cm_event_start_date_mask <- !is.na(safetyData::sdtm_cm[["CMSTDTC"]])
  selected_subject_mask <- safetyData::sdtm_cm[["USUBJID"]] %in% subjects
  cm_info <- safetyData::sdtm_cm[known_cm_event_start_date_mask & selected_subject_mask, ]

  cm_info[["CMSTDT"]] <- robust_ymd(cm_info[["CMSTDTC"]])
  cm_info[["CMENDT"]] <- robust_ymd(cm_info[["CMENDTC"]], round_up = TRUE)

  # vs
  vs_info <- safetyData::adam_advs |>
    dplyr::left_join(safetyData::adam_advs |>
      dplyr::group_by(.data[["USUBJID"]], .data[["PARAM"]], .data[["VISIT"]]) |>
      dplyr::summarise(AVAL_MEAN = mean(as.numeric(.data$AVAL)), .groups = "drop") |>
      dplyr::ungroup(), by = c("USUBJID", "PARAM", "VISIT"))
  attr(vs_info[["AVAL_MEAN"]], "label") <- "Mean Value"


  # lb
  lb_info <- safetyData::adam_adlbc |>
    dplyr::left_join(safetyData::adam_adlbc |>
      dplyr::group_by(.data[["USUBJID"]], .data[["PARAM"]], .data[["VISIT"]]) |>
      dplyr::summarise(AVAL_MEAN = mean(as.numeric(.data$AVAL)), .groups = "drop") |>
      dplyr::ungroup(), by = c("USUBJID", "PARAM", "VISIT")) |>
    dplyr::filter(
      .data$USUBJID %in% adsl_info$USUBJID
    )

  attr(lb_info[["AVAL_MEAN"]], "label") <- "Mean Value"

  return(list(adsl = adsl_info, adae = adae_info, cm = cm_info, lb = lb_info, vs = vs_info))
}
