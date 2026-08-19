# Interactive demo/configuration tool for [`mod_patient_profile()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile.md)

Launch an interactive configuration app for
[`mod_patient_profile()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile.md).
This application guides the user through the configuration of the
module, offering an experimental point-and-click interface to the module
API. Help is accessible by hovering over any of the provided
parameters.\
To try it using demo data, run
`dv.papo::explorer_app(dv.papo:::prep_safety_data())` in your R prompt.

## Usage

``` r
explorer_app(datasets = NULL)
```

## Arguments

- datasets:

  `[list(data.frame(n))]` (optional) Datasets available to the module.
  One of them should be a demographic subject-level dataset and the rest
  should be visit-dependent datasets. If not provided, the UI offers a
  file input selector that is functionally equivalent.
