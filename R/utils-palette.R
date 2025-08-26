#' Extract grading values using plot vars list and afmm data
#'
#' @param input_plots `[list(1+)]`
#' @param afmm_data `[list(1+)]`
#'
#' @return `[character(1+)]`
#' @keywords internal
get_grading_vals <- function(input_plots, afmm_data) {
  grading_vals <- sapply(afmm_data, function(dataset) {
    sapply(input_plots, function(plot_type) {
      if ("grading" %in% plot_type$vars)
      dataset[[plot_type$dataset]][plot_type$vars[["grading"]]]
    })
  }) |> unlist() |> unique()
  return(grading_vals)
}

#' Create/complete a colour palette which maps colours to grading levels.
#'
#' @param grading_vals `[character(1+)]` vector of grading values/levels.
#' @param user_palette `[character(1+) | NULL]` named character vector mapping colour(s) to grading level(s).
#'
#' @return `[character(1+)]` an updated colour palette where colours are mapped to ALL grading levels.
#' @keywords internal
fill_palette <- function(grading_vals, user_palette = NULL) {

  # user palette complements default
  existing_palette <- unlist(
    utils::modifyList(
      as.list(CONST[["default_palette"]]),
      as.list(user_palette)
    )
  )

  unmapped_vals <- setdiff(grading_vals, c(names(existing_palette), NA))

  available_colours <- c("orange",
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
                         "dodgerblue")

  if (length(unmapped_vals) > 0) {
    unused_colours <- setdiff(available_colours, existing_palette)

    #to ensure there is always a color that can be assigned.
    if (!length(unused_colours) || length(unused_colours) < length(unmapped_vals)) {
      unused_colours <- setdiff(colors(), c(existing_palette, "white"))
    }

    pal_generator <- colorRampPalette(unused_colours)
    new_palette   <- structure(pal_generator(length(unmapped_vals)), names = unmapped_vals)

    return(c(existing_palette, new_palette))

  } else {
    return(existing_palette)
  }
}

