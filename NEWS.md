# dv.papo 2.0.1-9008

- Improves SAE label positioning.
- Fixes missing palette colours for AE, CM grading values.
- Update to provide early error feedback if a sender_id is not available in list of modules.
- Fixes issue with labels not working fully if a data.frame is passed as input.
- Fixes y-axis getting squashed if blank values present in DECODE variable for AE/CM plots.
- Fixes Serious AE labels mapping when the column is a "Y/N" `character` or `factor` variable instead of `logical`.

# dv.papo 2.0.1

- Fixes failed first interaction when a participant is selected from another module
- Fixes plotly version, previous version numbers failed with an obscure error
- Fixes legend removal when grading was not present in any of the plots

# dv.papo 2.0.0

- Major API redesign with the following breaking changes `mod_patient_profile()`:
  - The name of the subject-level dataset is now explicitly provided through the `subject_level_dataset_name` parameter.
  - `key` has been renamed to `subjid_var`.
  - `pt_info` is now called `summary` and has been simplified. It only allows to specify a _single_ dataframe 
    from which to select columns. Its `row_item` submember has been renamed to `column_count`.
  - `tables` is now called `listings` and is a named list of pairs of `dataset` and `default_vars`. This allows 
    control over which columns are shown by default for each dataset.
  - The legacy `basic_info`, `vlines` and `plots` are all now collected under a single `plots` parameter. They have been
    restructured for clarity and are also accompanied by a `palette` field that makes it possible for the caller to
    specify individual colors. See the main vignette for more information.
  - All parameters referring to Study Days have been reframed to point to absolute days. This ensures consistency of the
    time axis of plots.
  - The module now accepts datetimes as well as dates for easier joint configuration with `dv.clinlines`. The "time"
    part of datetimes is simply discarded.
  - Configuration helper functions have been removed in favor of documentation templates.
  - `jumping_vars` is now called `sender_ids` for consistency across davinci modules.

- The module is now easier to configure because it:
  - Makes all of its main three sections (`summary`, `listings` and `plots`) optional, which facilitates having a base
    working configuration.
  - Provides clear start-up warnings and messages when the module is misconfigured with respect to input datasets. 
    The app creator can safely assume that if no such message appears, the module will not misbehave when interacted 
    with due to misconfiguration. Any remaining error rests solely on the module developer's shoulders or on those of 
    the platform provider.
    
- Bug fixes:
  - Day labels now follow the standard CDISC behavior of skipping day 0 on plots (day 1 refers to the Treatment Start
    Date; day -1 is the day before).
  - Vertical lines now cross all plots.


# dv.papo 1.0.2

- bugfix: With the new version of dv.filter 2.1.0 the patient selector did not work. This is now fixed.

# dv.papo 1.0.1

- bugfix: Updating patient selector after jumping from another module didn't work. This is now fixed.

# dv.papo 1.0.0

- Primary interface: `mod_patient_profile()`

- Launch mock app for demo and testing via `mock_with_mm_app()`

- Enables bookmarking state of Shiny app via URL

# dv.papo 0.1

*  Plot section added.

# dv.papo 0.0.2

*  Shinydashboard element removed.

# dv.papo 0.0.1

* Added a `NEWS.md` file to track changes to the package.


