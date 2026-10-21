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
      palette_is_filled = "Color palette will be completed with colors for all grading values if any are missing.",
      timeline_limits = "Timeline limits determined by treatment start/end, informed consent, participation end, and today's date"
    ),
    value = list(
      parameter_selection = "User can select parameters for value plots",
      default_parameter_selection = "Default analysis parameters are retained between patients"
    ),
    range = list(
      arrows = "Arrows indicate events exceeding range limits",
      grading = "Plots colored according to grading"
    )
  ),
  batch = list(
    pdf_per_element = "Batch rendering writes one PDF per element of subjid_vals, holding its subjects",
    optional_sections = "Summary, listings and plots are optional and left out of the PDF when not configured",
    provenance = "PDFs can include data modification times and hashes, session info and rendering warnings",
    input_checks = "Invalid configurations or file names, and existing output files, stop the call before rendering",
    failure_report = "A failed PDF is replaced by an error report and listed in a manifest, without stopping the others",
    long_cells = paste(
      "Listing cells wrap at 50 characters per line. Those over 200 characters are cut and linked to a note, below",
      "the listing, with the whole text. The note links back and the whole text is the tooltip of the link"
    )
  )
)
