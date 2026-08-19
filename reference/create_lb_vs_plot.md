# Function to create lab or vital sign plot

Function to create lab or vital sign plot

## Usage

``` r
create_lb_vs_plot(
  data,
  date,
  val,
  low_limit,
  high_limit,
  param_var,
  param_val,
  summary_stats,
  x_limits,
  x_axis_unit,
  x_axis_breaks,
  palette,
  sl_info,
  vline_vars,
  vline_day_numbers,
  ref_date,
  plot_name,
  annotate_x_axis
)
```

## Arguments

- data:

  Data frame containing the data for the plot

- val:

  Name of the variable that contains analyze value

- low_limit:

  Name of the variable that contains the values of the low limit of the
  normal range

- high_limit:

  Name of the variable that contains the values of the high limit of the
  normal range

- param_var:

  Name of the variable that contains the analysis parameter values

- param_val:

  Name of the analysis parameter

- summary_stats:

  Name of the variable that contains the values of the summary statistic

- plot_name:

  Name of plot

- annotate_x_axis:

  Logical indicating whether to annotate the x-axis

- lb_selected_params:

  Vector containing the values of the selected parameters

- day:

  Name of the variable that contains the analyze day

- analysis_indicator:

  Name of the variable that contains the values analysis indicator

- limits:

  Vector that contains the limits of the plot

## Value

A ggplot2 object
