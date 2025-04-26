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
    x_axis_unit = "Defines the units of the x axis in the plots",
    x_axis_breaks = "When a single integer is passed it will use `base::pretty` to compute a set of breakpoints. If more than one value is passed it will use those breaks in the x axis",
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
      tooltip = "Block of text to display as hover information over each point of the trace. The names of this list are included as literal text and honor three basic HTML formatting elements: `<b>`, `<i>`, `<br>`). The columns the values refer to are populated with the value on the dataset relevant to any given row"
    ),
    vline_vars = "Place vertical dashed lines on days indicated by this dataset columns",
    vline_day_numbers = "Place vertical dashed lines on days indicated by this parameter",
    palette = "If a name on this list matches the text on a plot element, the associated color will be applied to that element. This mapping takes precedence over the built-in palette"
  )
)

mod_patient_profile_API <- T_group(
  module_id = T_mod_ID(),
  subject_level_dataset_name = T_dataset_name() |> T_flag("subject_level_dataset_name"),
  subjid_var = T_col("subject_level_dataset_name", T_or(T_factor(), T_character())) |> T_flag("subjid_var"),
  sender_ids = T_character() |> T_flag("zero_or_more", "optional", "ignore"),
  summary = T_group(
    vars = T_col("subject_level_dataset_name") |> T_flag("zero_or_more", "as_array"),
    column_count = T_integer(min = 1, max = 12)
  ) |> T_flag("optional"),
  listings = T_group(
    dataset = T_dataset_name(),
    default_vars = T_col("dataset") |> T_flag("optional", "zero_or_more", "as_array")
  ) |> T_flag("optional", "zero_or_more", "named"),
  plots = T_group(
    x_axis_unit = T_character() |> T_flag("optional"),
    x_axis_breaks = T_integer(min = 1) |> T_flag("optional", "zero_or_more", "as_array"),
    timeline_info = T_group(
      icf_date = T_col("subject_level_dataset_name", T_or(T_date(), T_datetime())) |> T_flag("optional"),
      trt_start_date = T_col("subject_level_dataset_name", T_or(T_date(), T_datetime())),
      trt_end_date = T_col("subject_level_dataset_name", T_or(T_date(), T_datetime())),
      part_end_date = T_col("subject_level_dataset_name", T_or(T_date(), T_datetime())) |> T_flag("optional")
    ) |> T_flag("as_array"),
    range_plots = T_group(
      dataset = T_dataset_name(),
      vars = T_group(
        start_date = T_col("dataset", T_or(T_date(), T_datetime())),
        end_date = T_col("dataset", T_or(T_date(), T_datetime())),
        decode = T_col("dataset", T_or(T_character(), T_factor())),
        grading = T_col("dataset", T_or(T_character(), T_factor())) |> T_flag("optional"),
        serious_ae = T_col("dataset", T_or(T_logical(), T_YN()) |> T_map_to(T_logical())) |> T_flag("optional")
      ) |> T_flag("as_array"),
      tooltip = T_col("dataset") |> T_flag("zero_or_more", "named", "as_array")
    ) |> T_flag("zero_or_more", "named"),
    value_plots = T_group(
      dataset = T_dataset_name(),
      vars = T_group(
        analysis_param = T_col("dataset", T_or(T_character(), T_factor())),
        analysis_val = T_col("dataset", T_numeric()),
        analysis_date = T_col("dataset", T_or(T_date(), T_datetime())),
        analysis_indicator = T_col("dataset", T_or(T_character(), T_factor())) |> T_flag("optional"),
        range_low_limit = T_col("dataset", T_numeric()) |> T_flag("optional"),
        range_high_limit = T_col("dataset", T_numeric()) |> T_flag("optional"),
        summary_stats = T_col("dataset", T_numeric()) |> T_flag("optional")
      ) |> T_flag("as_array"),
      tooltip = T_col("dataset") |> T_flag("zero_or_more", "named", "as_array")
    ) |> T_flag("zero_or_more", "named"),
    vline_vars = T_col(
      "subject_level_dataset_name", T_or(T_CDISC_study_day(), T_date(), T_datetime())
    ) |> T_flag("zero_or_more", "named", "as_array"),
    vline_day_numbers = T_CDISC_study_day() |> T_flag("optional", "zero_or_more", "named", "as_array"),
    palette = T_color() |> T_flag("optional", "zero_or_more", "named", "as_array")
  ) |> T_flag("optional")
) |> T_attach_docs(mod_patient_profile_API_docs)

# Available module specifications ----
module_specifications <- list(
  "dv.papo::mod_patient_profile" = mod_patient_profile_API # TODO: Add another module to demonstrate the generality of the approach
)
