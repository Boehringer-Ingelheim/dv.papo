# Prepare safety data

Modify safetyData's adsl, adae, and adcm dummy data for easy use within
dv.clinlines.

## Usage

``` r
prep_safety_data(n = 200)
```

## Arguments

- n:

  Number of rows to select from the adsl dataset. The first n rows will
  be taken. Used to reduce runtime during development.

## Value

A list of three data frames.
