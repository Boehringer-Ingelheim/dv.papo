# Create server for patient listings shiny module of dv.papo

Create server for patient listings shiny module of dv.papo

## Usage

``` r
patient_listing_server(id, data_list, key_value, listings)
```

## Arguments

- id:

  A unique ID string to create a namespace. Must match the ID of
  [`patient_listing_UI()`](https://boehringer-ingelheim.github.io/dv.papo/reference/patient_listing_UI.md).

- data_list:

  List of data frames containing data for each listing of selected
  patient.

- key_value:

  Character: Value of selected patient

- listings:

  `[list(n)]` (optional) Listings section. Composed of:

  - dataset `[character]` Dataset name.

  - default_vars `[character(n)]` (optional) Default columns to display.
    If not specified, the first six columns are selected by default.
    Indexes into dataset `dataset`.
