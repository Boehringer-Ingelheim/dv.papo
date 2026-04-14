#' Prepare safety data
#'
#' Modifiy pharmaverseadam's adsl, adae, and adcm dummy data for easy use within
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

  adsl_info <- pharmaverseadam::adsl[1:n, ]
  adsl_info[["TRTSDT"]] <- robust_ymd(adsl_info[["TRTSDT"]]) |> structure(label = "Treatment Start Date")
  adsl_info[["TRTEDT"]] <- robust_ymd(adsl_info[["TRTEDT"]], round_up = TRUE) |> structure(label = "Treatment End Date")
  adsl_info[["RFICDT"]] <- robust_ymd(adsl_info[["RFSTDTC"]]) |> structure(label = "Informed Consent Date")
  adsl_info[["TRTDUR"]] <- as.numeric(adsl_info[["TRTDURD"]]) |> structure(label = "Treatment duration (days)")
  adsl_info[["RFENDT"]] <- robust_ymd(adsl_info[["RFENDTC"]])

  subjects <- unique(adsl_info[["USUBJID"]])

  # adae
  known_adverse_event_start_date_mask <- !is.na(pharmaverseadam::adae[["ASTDT"]])
  selected_subject_mask <- pharmaverseadam::adae[["USUBJID"]] %in% subjects
  adae_info <- pharmaverseadam::adae[known_adverse_event_start_date_mask & selected_subject_mask, ]
  aeser_attributes <- attributes(adae_info[["AESER"]])
  adae_info[["AESER"]] <- c("Y" = TRUE, "N" = FALSE)[adae_info[["AESER"]]]
  attributes(adae_info[["AESER"]]) <- aeser_attributes

  # cm
  known_cm_event_start_date_mask <- !is.na(pharmaversesdtm::cm[["CMSTDTC"]])
  selected_subject_mask <- pharmaversesdtm::cm[["USUBJID"]] %in% subjects
  cm_info <- pharmaversesdtm::cm[known_cm_event_start_date_mask & selected_subject_mask, ]

  cm_info[["CMSTDT"]] <- robust_ymd(cm_info[["CMSTDTC"]])
  cm_info[["CMENDT"]] <- robust_ymd(cm_info[["CMENDTC"]], round_up = TRUE)

  # vs
  vs_info <- pharmaverseadam::advs |>
    dplyr::left_join(pharmaverseadam::advs |>
      dplyr::group_by(.data[["USUBJID"]], .data[["PARAM"]], .data[["VISIT"]]) |>
      dplyr::summarise(AVAL_MEAN = mean(as.numeric(.data$AVAL)), .groups = "drop") |>
      dplyr::ungroup(), by = c("USUBJID", "PARAM", "VISIT"))
  attr(vs_info[["AVAL_MEAN"]], "label") <- "Mean Value"


  # lb
  lb_info <- pharmaverseadam::adlb |>
    dplyr::left_join(pharmaverseadam::adlb |>
      dplyr::group_by(.data[["USUBJID"]], .data[["PARAM"]], .data[["VISIT"]]) |>
      dplyr::summarise(AVAL_MEAN = mean(as.numeric(.data$AVAL)), .groups = "drop") |>
      dplyr::ungroup(), by = c("USUBJID", "PARAM", "VISIT")) |>
    dplyr::filter(
      .data$USUBJID %in% adsl_info$USUBJID
    )

  attr(lb_info[["AVAL_MEAN"]], "label") <- "Mean Value"

  return(list(adsl = adsl_info, adae = adae_info, cm = cm_info, lb = lb_info, vs = vs_info))
}
