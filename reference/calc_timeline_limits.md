# Calculate the timeline limits

Initialized to treatment start and end dates, but takes informed consent
and participation end dates into account if available. If end dates are
missing then set to today's date.

## Usage

``` r
calc_timeline_limits(rfxstdt, rfxendt, rficdt = NULL, rfpendt = NULL)
```

## Arguments

- rfxstdt:

  Treatment start date.

- rfxendt:

  Treatment end date.

- rficdt:

  Informed consent date (`NULL` value allowed).

- rfpendt:

  Participation end date (`NULL` value allowed).

## Value

2-element vector of timeline minimum and maximum limits.
