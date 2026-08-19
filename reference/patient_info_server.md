# Create server for the patient information shiny module of dv.papo

Create server for the patient information shiny module of dv.papo

## Usage

``` r
patient_info_server(id, record, subjid_var, column_count = 3)
```

## Arguments

- id:

  A unique ID string to create a namespace. Must match the ID of
  [`patient_info_UI()`](https://boehringer-ingelheim.github.io/dv.papo/reference/patient_info_UI.md).

- record:

  row object of dataframe: row record for patient.

- subjid_var:

  `[character]` Unique subject identifier column.

- column_count:

  A number indicating how many items should be in one row
