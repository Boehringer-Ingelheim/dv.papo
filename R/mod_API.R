# Patient profile module interface description ----
mod_patient_profile_API_docs <- list(
  "PAtient PrOfile",
  module_id = "",
  subject_level_dataset_name = "",
  subjid_var = "",
  sender_ids = "Identifiers of modules allowed to request the display of target patient IDs",
  summary = list(
    "Subject-level summary section",
    vars = "Values to display in the summary section",
    column_count = "Column count for the summary section"
  ),
  listings = list(
    "Listings section",
    dataset = "",
    default_vars = "Default columns to display. If not specified, the first six columns are selected by default"
  ),
  plots = list(
    "Plot section",
    x_axis_unit = 'Defines the time units of the x-axis in the plots. Expects `["weeks"/"days"]` values. Defaults to `"days"`',
    x_axis_breaks = "Defines how many breaks will be used in the x-axis. When a single integer is passed it will use `base::pretty` to compute that number of breaks. If more than one value is passed it will use those breaks in the x-axis (e.g. if `c(1, 2, 3)` is passed it will show breaks at days/weeks 1, 2 and 3). Defaults to `5`",
    timeline_info = list(
      "Start and end study dates",
      icf_date = "Informed Consent Form signing Date",
      trt_start_date = "Treatment Start Date, used also as Day 1 Reference Date",
      trt_end_date = "Treatment End Date",
      part_end_date = "Participation End Date"
    ),
    range_plots = list(
      "Plots for range-like events (adverse events, concomitant medications, ...)",
      dataset = "",
      vars = list(
        "Variables of interest for the event",
        start_date = "Start date",
        end_date = "End date",
        decode = "Descriptive event label",
        grading = "Degree of the event, used for color-coding the plot",
        serious_ae = "Marker of event seriousness, usually reserved for adverse event datasets"
      ),
      tooltip = "Block of text to display as hover information over the left edge of each range. The names of this list are included as literal text and honor three basic HTML formatting elements: `<b>`, `<i>`, `<br>`). The columns the values refer to are populated with the value on the dataset relevant to any given row"
    ),
    value_plots = list(
      "Plots for value-like (lab measurements, vital signs, ...) traces",
      dataset = "",
      vars = list(
        "Variables of interest for the trace",
        analysis_param = "Parameter",
        analysis_val = "Value",
        analysis_date = "Date",
        analysis_indicator = "Analysis reference range indicator",
        range_low_limit = "Lower limit of the reference range",
        range_high_limit = "Upper limit of the reference range",
        summary_stats = "Additional value column for summary statistics"
      ),
      tooltip = "Block of text to display as hover information over each point of the trace. The names of this list are included as literal text and honor three basic HTML formatting elements: `<b>`, `<i>`, `<br>`). The columns the values refer to are populated with the value on the dataset relevant to any given row",
      default_analysis_params = "A vector of character values specifying the default analysis parameters (values from the variable specified by `analysis_param`) to display"
    ),
    vline_vars = "Place vertical dashed lines on days indicated by these dataset columns",
    vline_day_numbers = "Place vertical dashed lines on days indicated by this parameter",
    palette = "If a name on this list matches the text on a plot element, the associated color will be applied to that element. This mapping takes precedence over the built-in palette"
  )
)

mod_patient_profile_API <- TC$group(
  module_id = TC$mod_ID(),
  subject_level_dataset_name = TC$dataset_name() |> TC$flag("subject_level_dataset_name"),
  subjid_var = TC$col("subject_level_dataset_name", TC$or(TC$factor(), TC$character())) |> TC$flag("subjid_var"),
  sender_ids = TC$character() |> TC$flag("zero_or_more", "optional", "ignore"),
  summary = TC$group(
    vars = TC$col("subject_level_dataset_name") |> TC$flag("zero_or_more", "as_array"),
    column_count = TC$integer(min = 1, max = 12)
  ) |> TC$flag("optional"),
  listings = TC$group(
    dataset = TC$dataset_name(),
    default_vars = TC$col("dataset") |> TC$flag("optional", "zero_or_more", "as_array")
  ) |> TC$flag("optional", "zero_or_more", "named"),
  plots = TC$group(
    x_axis_unit = TC$character() |> TC$flag("optional"),
    x_axis_breaks = TC$or(TC$integer(), TC$numeric()) |> TC$flag("zero_or_more") |> TC$flag("optional"),
    timeline_info = TC$group(
      icf_date = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())) |> TC$flag("optional"),
      trt_start_date = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())),
      trt_end_date = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())),
      part_end_date = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())) |> TC$flag("optional")
    ) |> TC$flag("as_array"),
    range_plots = TC$group(
      dataset = TC$dataset_name(),
      vars = TC$group(
        start_date = TC$col("dataset", TC$or(TC$date(), TC$datetime())),
        end_date = TC$col("dataset", TC$or(TC$date(), TC$datetime())),
        decode = TC$col("dataset", TC$or(TC$character(), TC$factor())),
        grading = TC$col("dataset", TC$or(TC$character(), TC$factor())) |> TC$flag("optional"),
        serious_ae = TC$col("dataset", TC$or(TC$logical(), TC$YN()) |> TC$map_to(TC$logical())) |> TC$flag("optional")
      ) |> TC$flag("as_array"),
      tooltip = TC$col("dataset") |> TC$flag("zero_or_more", "named", "as_array")
    ) |> TC$flag("zero_or_more", "named"),
    value_plots = TC$group(
      dataset = TC$dataset_name(),
      vars = TC$group(
        analysis_param = TC$col("dataset", TC$or(TC$character(), TC$factor())),
        analysis_val = TC$col("dataset", TC$numeric()),
        analysis_date = TC$col("dataset", TC$or(TC$date(), TC$datetime())),
        analysis_indicator = TC$col("dataset", TC$or(TC$character(), TC$factor())) |> TC$flag("optional"),
        range_low_limit = TC$col("dataset", TC$numeric()) |> TC$flag("optional"),
        range_high_limit = TC$col("dataset", TC$numeric()) |> TC$flag("optional"),
        summary_stats = TC$col("dataset", TC$numeric()) |> TC$flag("optional")
      ) |> TC$flag("as_array"),
      tooltip = TC$col("dataset") |> TC$flag("zero_or_more", "named", "as_array"),
      default_analysis_params = TC$character() |> TC$flag("zero_or_more") |> TC$flag("optional")
    ) |> TC$flag("zero_or_more", "named"),
    vline_vars = TC$col(
      "subject_level_dataset_name", TC$or(TC$CDISC_study_day(), TC$date(), TC$datetime())
    ) |> TC$flag("zero_or_more", "named", "as_array"),
    vline_day_numbers = TC$CDISC_study_day() |> TC$flag("optional", "zero_or_more", "named", "as_array"),
    palette = TC$color() |> TC$flag("optional", "zero_or_more", "named", "as_array")
  ) |> TC$flag("optional")
) |> TC$attach_docs(mod_patient_profile_API_docs)

# Available module specifications ----
module_specifications <- list(
  "dv.papo::mod_patient_profile" = mod_patient_profile_API # TODO: Add another module to demonstrate the generality of the approach
)
