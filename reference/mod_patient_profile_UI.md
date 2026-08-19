# Patient Profile Module UI

(For use outside of the DaVinci framework)\
Places the Patient Profile module UI at the call site of this function.
A matching call to
[`mod_patient_profile_server()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile_server.md)
is necessary.\

## Usage

``` r
mod_patient_profile_UI(id)
```

## Arguments

- id:

  `[character]` Unique shiny ID. Must match the ID provided to
  [`mod_patient_profile_server()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile_server.md).

## Value

Shiny UI.

## See also

[`mod_patient_profile()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile.md)
and
[`mod_patient_profile_server()`](https://boehringer-ingelheim.github.io/dv.papo/reference/mod_patient_profile_server.md)
