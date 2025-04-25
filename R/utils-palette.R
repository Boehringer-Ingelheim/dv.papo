#' Create/complete a colour palette which maps colours to grading levels.
#'
#' @param input_data `[data.frame]` data.frame containing data of interest.
#' @param grading_cols `[character(1+)]` vector of grading column names.
#' @param existing_palette `[character(1+) | NULL]` named character vector mapping colour(s) to grading level(s).
#'
#' @return `[character(1+)]` an updated colour palette with colours mapped to ALL grading levels.
#' @internal
fill_palette <- function(input_data, grading_cols, existing_palette = NULL) {

  # input_data <- x$data
  # grading_cols <- x$grading_cols
  # existing_palette <- x$pal

  data_subset <- input_data[grading_cols]
  #data_subset <- input_data[c(grading_cols, "VISIT")]

  all_grading_vals <- sapply(data_subset, unique) |> unlist()
  unmapped_vals <- setdiff(all_grading_vals, c(names(existing_palette), NA))

  if (length(unmapped_vals) > 0) {
    pal_generator <- grDevices::colorRampPalette(existing_palette)
    new_palette <- structure(pal_generator(length(unmapped_vals)), names = unmapped_vals)
    return(c(existing_palette, new_palette))
  } else {
    return(existing_palette)
  }

}
