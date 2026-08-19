# Patient Profile Module

`mod_patient_profile` is a DaVinci Shiny module that displays patient
information on a subject-level basis. It consists of three sections:
summary, listings and plots.

- The `summary` section shows visit-independent demographic information.

- The `listings` section offers listings for visit-dependent
  information.

- The `plots` section displays charts for events that happen over a span
  of time (adverse events, concomitant medications, ...) or line plots
  of point-like events (laboratory values, vital sign measurements).

Show/hide usage example

\
What follows is a sample call to the mod_patient_profile function. See
the [main article of this
package](https://boehringer-ingelheim.github.io/dv.papo/articles/a00-papo.md)
for a tutorial on how to parameterize it.


    dv.papo::mod_patient_profile(
      module_id = "papo",
      subject_level_dataset_name = "adsl",
      subjid_var = "USUBJID",
      summary = list(
        vars = c("SUBJID", "SITEID", "ARM", "TRTSDT", "TRTEDT", "AGE", "RACE", "SEX", "BMIBL"),
        column_count = 3L
      ),
      listings = list(
        "Concomitant Medication" = list(
          dataset = "cm"
        ),
        "Adverse Events" = list(
          dataset = "adae",
          default_vars = c("ASTDT", "ASTDY", "AENDT", "AENDY", "AEDECOD", "AESEV")
        )
      ),
      plots = list(
        timeline_info = c(trt_start_date = "TRTSDT", trt_end_date = "TRTEDT"),
        range_plots = list(
          "Concomitant Medication" = list(
            dataset = "cm",
            vars = c(
              start_date = "CMSTDT", end_date = "CMENDT",
              decode = "CMDECOD", grading = "CMDECOD"
            ),
            tooltip = c()
          ),
          "Adverse Events" = list(
            dataset = "adae",
            vars = c(
              start_date = "ASTDT", end_date = "AENDT", decode = "AEDECOD",
              grading = "AESEV", serious_ae = "AESER"
            ),
            tooltip = c("AE Start Day: " = "ASTDY", "AE End Day: " = "AENDY")
          )
        ),
        value_plots = list(
          "Lab Values" = list(
            dataset = "lb",
            vars = c(
              analysis_param = "PARAM", analysis_val = "AVAL", analysis_date = "ADT",
              analysis_indicator = "LBNRIND", range_low_limit = "A1LO", range_high_limit = "A1HI"
            ),
            tooltip = c()
          )
        ),
        vline_vars = c(
          "Informed Consent Date" = "RFICDT"
        )
      )
    )

------------------------------------------------------------------------

## Usage

``` r
mod_patient_profile(
  module_id = "",
  subject_level_dataset_name = NULL,
  subjid_var = NULL,
  sender_ids = NULL,
  summary = NULL,
  listings = NULL,
  plots = NULL
)
```

## Arguments

- module_id:

  `[character]` Unique Shiny module identifier.

- subject_level_dataset_name:

  `[character]` Subject-level dataset name.

- subjid_var:

  `[character]` Unique subject identifier column.

- sender_ids:

  `[character(n)]` (optional) Identifiers of modules allowed to request
  the display of target patient IDs.

- summary:

  `[list]` (optional) Subject-level summary section. Composed of:

  - vars `[character(n)]` Values to display in the summary section.
    Indexes into dataset `subject_level_dataset_name`.

  - column_count `[integer]` Column count for the summary section.

- listings:

  `[list(n)]` (optional) Listings section. Composed of:

  - dataset `[character]` Dataset name.

  - default_vars `[character(n)]` (optional) Default columns to display.
    If not specified, the first six columns are selected by default.
    Indexes into dataset `dataset`.

- plots:

  `[list]` (optional) Plot section. Composed of:

  - x_axis_unit `[character]` (optional) Defines the time units of the
    x-axis in the plots. Expects `["weeks"/"days"]` values. Defaults to
    `"days"`.

  - x_axis_breaks `[integer|numeric(n)]` (optional) Defines how many
    breaks will be used in the x-axis. When a single integer is passed
    it will use [`base::pretty`](https://rdrr.io/r/base/pretty.html) to
    compute that number of breaks. If more than one value is passed it
    will use those breaks in the x-axis (e.g. if `c(1, 2, 3)` is passed
    it will show breaks at days/weeks 1, 2 and 3). Defaults to `5`.

  - timeline_info `[list]` Start and end study dates. Composed of:

    - icf_date `[character]` (optional) Informed Consent Form signing
      Date. Indexes into dataset `subject_level_dataset_name`. Expects
      `[Date|POSIXt]` values.

    - trt_start_date `[character]` Treatment Start Date, used also as
      Day 1 Reference Date. Indexes into dataset
      `subject_level_dataset_name`. Expects `[Date|POSIXt]` values.

    - trt_end_date `[character]` Treatment End Date. Indexes into
      dataset `subject_level_dataset_name`. Expects `[Date|POSIXt]`
      values.

    - part_end_date `[character]` (optional) Participation End Date.
      Indexes into dataset `subject_level_dataset_name`. Expects
      `[Date|POSIXt]` values.

  - range_plots `[list(n)]` Plots for range-like events (adverse events,
    concomitant medications, ...). Composed of:

    - dataset `[character]` Dataset name.

    - vars `[list]` Variables of interest for the event. Composed of:

      - start_date `[character]` Start date. Indexes into dataset
        `dataset`. Expects `[Date|POSIXt]` values.

      - end_date `[character]` End date. Indexes into dataset `dataset`.
        Expects `[Date|POSIXt]` values.

      - decode `[character]` Descriptive event label. Indexes into
        dataset `dataset`. Expects `[character|factor]` values.

      - grading `[character]` (optional) Degree of the event, used for
        color-coding the plot. Indexes into dataset `dataset`. Expects
        `[character|factor]` values.

      - serious_ae `[character]` (optional) Marker of event seriousness,
        usually reserved for adverse event datasets. Indexes into
        dataset `dataset`. Expects `[logical|"Y"/"N"]` values.

    - tooltip `[character(n)]` Block of text to display as hover
      information over the left edge of each range. The names of this
      list are included as literal text and honor three basic HTML
      formatting elements: `<b>`, `<i>`, `<br>`). The columns the values
      refer to are populated with the value on the dataset relevant to
      any given row. Indexes into dataset `dataset`.

  - value_plots `[list(n)]` Plots for value-like (lab measurements,
    vital signs, ...) traces. Composed of:

    - dataset `[character]` Dataset name.

    - vars `[list]` Variables of interest for the trace. Composed of:

      - analysis_param `[character]` Parameter. Indexes into dataset
        `dataset`. Expects `[character|factor]` values.

      - analysis_val `[character]` Value. Indexes into dataset
        `dataset`. Expects `[numeric]` values.

      - analysis_date `[character]` Date. Indexes into dataset
        `dataset`. Expects `[Date|POSIXt]` values.

      - analysis_indicator `[character]` (optional) Analysis reference
        range indicator. Indexes into dataset `dataset`. Expects
        `[character|factor]` values.

      - range_low_limit `[character]` (optional) Lower limit of the
        reference range. Indexes into dataset `dataset`. Expects
        `[numeric]` values.

      - range_high_limit `[character]` (optional) Upper limit of the
        reference range. Indexes into dataset `dataset`. Expects
        `[numeric]` values.

      - summary_stats `[character]` (optional) Additional value column
        for summary statistics. Indexes into dataset `dataset`. Expects
        `[numeric]` values.

    - tooltip `[character(n)]` Block of text to display as hover
      information over each point of the trace. The names of this list
      are included as literal text and honor three basic HTML formatting
      elements: `<b>`, `<i>`, `<br>`). The columns the values refer to
      are populated with the value on the dataset relevant to any given
      row. Indexes into dataset `dataset`.

    - default_analysis_params `[character(n)]` (optional) A vector of
      character values specifying the default analysis parameters
      (values from the variable specified by `analysis_param`) to
      display.

  - vline_vars `[character(n)]` Place vertical dashed lines on days
    indicated by these dataset columns. Indexes into dataset
    `subject_level_dataset_name`. Expects `[integer|Date|POSIXt]`
    values.

  - vline_day_numbers `[integer(n)]` (optional) Place vertical dashed
    lines on days indicated by this parameter. Represents a CDISC
    (non-zero) Study Day.

  - palette `[character(n)]` (optional) If a name on this list matches
    the text on a plot element, the associated color will be applied to
    that element. This mapping takes precedence over the built-in
    palette. Contains either an HTML (#xxxxxx) or an R color.

## Value

A list composed of the following elements:

- `ui`: Shiny UI function.

- `server`: Shiny server function.

- `module_id`: Shiny unique identifier.

## See also

[`mod_patient_profile_UI()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile_UI.md),
[`mod_patient_profile_server()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile_server.md)
