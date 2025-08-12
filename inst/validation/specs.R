specs <- list(
  common = list(
    subject_selector = "The subject selector selects subjects",
    bookmarking = "Bookmarks bookmark state",
    jump_to_subject = "The jump-to-subject switches to subject when instructed",
    misconfiguration_feedback = "The module shows informative messages when misconfigured",
    dataset_change = "Patient selector updates after dataset change"
  ),
  summary = list(
    columns = "Summary data presented in custom column count"
  ),
  listings = list(
    column_labels = "Listings display column labels in headings if available",
    switching = "Tab buttons allow listing switching",
    filtering = "Listing content can be searched and filtered",
    sorting = "Listing content can sorted and the order can be reset",
    extra_column_selection = "User can modify columns included in listing",
    extra_column_selection_labels = "Column selector shows dataset column labels",
    no_data_message = "Listings explain there is no data when there is no data"
  ),
  plots = list(
    common = list(
      tooltips = "Hovering over salient plot elements provides extra information",
      palettes = "Plots can be customized by assigning specific colors to elements denoted by text strings",
      no_data_message = "Plots explain there is no data when there is no data",
      palette_is_filled = "Color palette will be completed with colors for all grading values if any are missing."
    ),
    value = list(
      parameter_selection = "User can select parameters for value plots",
      default_parameter_selection = "Default analysis parameters are retained between patients"
    ),
    range = list(
      arrows = "Arrows indicate events exceeding range limits",
      grading = "Plots colored according to grading"
    )
  )
)
