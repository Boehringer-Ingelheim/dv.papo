# Function to create AE or CM plot

Function to create AE or CM plot

## Usage

``` r
create_ae_cm_plot(
  data,
  x_limits,
  palette,
  sl_info,
  vline_vars,
  vline_day_numbers,
  x_axis_unit,
  x_axis_breaks,
  ref_date,
  plot_name,
  annotate_x_axis
)
```

## Arguments

- data:

  Data frame containing the data for the plot

- palette:

  Named vector that contains the colors that are used in the plot

- plot_name:

  Name of plot

- annotate_x_axis:

  Logical indicating whether to annotate the x-axis

- limits:

  Vector that contains the limits of the plot

## Value

A ggplot2 object
