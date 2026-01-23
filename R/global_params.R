CONST <- poc(
  width_of_patient_selector_in_columns = 4,
  decode_max_width_before_wrap_in_characters = 15,
  color_for_missing_analysis_indicator_levels = "darkgray",
  default_palette = c(
    `REFERENCE RANGE` = "#91eec2",
    `Reference range` = "#91eec2",
    `reference range` = "#91eec2",
    `NORMAL` = "green4", `LOW` = "red3", `HIGH` = "red2",
    `Normal` = "green4", `Low` = "red3", `High` = "red2",
    `normal` = "green4", `low` = "red3", `high` = "red2",
    `MILD` = "lightgreen", `MODERATE` = "gold1", `SEVERE` = "red",
    `Mild` = "lightgreen", `Moderate` = "gold1", `Severe` = "red",
    `mild` = "lightgreen", `moderate` = "gold1", `severe` = "red"
  ),
  PLOT_X_AXIS_UNITS = poc(
    DAYS = "days",
    WEEKS = "weeks"
  ),
  PLOT_X_AXIS_DEFAULT_NUMBER_OF_BREAKS = 5
)
