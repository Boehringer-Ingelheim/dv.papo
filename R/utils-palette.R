#' Create/complete a colour palette which maps colours to grading levels.
#'
#' @param input_data `[data.frame]` data.frame containing data of interest.
#' @param grading_cols `[character(1+)]` vector of grading column names.
#' @param existing_palette `[character(1+) | NULL]` named character vector mapping colour(s) to grading level(s).
#'
#' @return `[character(1+)]` an updated colour palette where colours are mapped to ALL grading levels.
#' @keywords internal
fill_palette <- function(input_data, grading_cols, existing_palette = NULL) {

  data_subset <- input_data[grading_cols]

  all_grading_vals <- sapply(data_subset, unique) |> unlist()
  unmapped_vals <- setdiff(all_grading_vals, c(names(existing_palette), NA))

  available_colours <- c("red",
                         "orange",
                         "yellow",
                         "green",
                         "cyan",
                         "blue",
                         "magenta",
                         "purple",
                         "black",
                         "pink",
                         "khaki",
                         "turquoise",
                         "navyblue",
                         "violet",
                         "yellowgreen",
                         "skyblue",
                         "indianred",
                         "cornsilk",
                         "chocolate",
                         "darkgoldenrod",
                         "coral",
                         "dodgerblue",
                         "firebrick",
                         "forestgreen",
                         "dimgrey",
                         "gold")

  if (length(unmapped_vals) > 0) {
    unused_colours <- setdiff(available_colours, existing_palette)

    #to ensure there is always a color that can be assigned.
    if (!length(unused_colours) || length(unused_colours) < length(unmapped_vals)) {
      unused_colours <- setdiff(colors(), c(existing_palette, "white"))
    }

    pal_generator <- colorRampPalette(unused_colours)
    new_palette   <- structure(pal_generator(length(unmapped_vals)), names = unmapped_vals)

    return(c(existing_palette, new_palette))

  } else {return(existing_palette)}
}

