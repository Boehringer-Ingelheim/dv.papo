# nolint start object_usage_linter # styler: off

# TODO: Generate from mod_patient_profile_API
# This function has been written manually, but mod_patient_profile_API carries
# enough information to derive most of it automatically
check_papo_call <- function(datasets, module_id, subject_level_dataset_name, subjid_var,
                            sender_ids, summary, listings, plots) {
  warn <- character(0)
  err <- character(0)

  assert_warn <- function(cond, msg, do_assert = TRUE) {
    ok <- FALSE
    if (isTRUE(do_assert)) {
      ok <- isTRUE(cond)
      if (!ok) warn <<- c(warn, msg)
    }
    return(ok)
  }

  assert_err <- function(cond, msg, do_assert = TRUE) {
    ok <- FALSE
    if (isTRUE(do_assert)) {
      ok <- isTRUE(cond)
      if (!ok) err <<- c(err, paste0(msg, "."))
    }
    return(ok)
  }

  is_valid_shiny_id <- function(s) grepl("^$|^[a-zA-Z][a-zA-Z0-9_-]*$", s)

  is_date_lower_or_equal <- function(a, b) all(as.POSIXct(a) <= as.POSIXct(b), na.rm = TRUE)

  allowed_classes_logical <- c("logical")
  allowed_classes_character <- c("character")
  allowed_classes_character_factor <- c("character", "factor")
  allowed_classes_numeric <- c("integer", "numeric")
  allowed_classes_date <- c("Date", "POSIXt")

  afmm_datasets <- paste(names(datasets), collapse = ", ")

  used_dataset_names <- list() # name identifies parameter, value stores dataset name

  # module_id
  assert_err(!missing(module_id), "`module_id` missing") &&
    assert_err(checkmate::test_string(module_id), "`module_id` should be a string") &&
    assert_warn(nchar(module_id) > 0, "Consider providing a non-empty `module_id`.") &&
    assert_err(
      is_valid_shiny_id(module_id),
      paste(
        "`module_id` should be a valid identifier, starting with a letter and followed by",
        "alphanumeric characters, hyphens and underscores"
      )
    )

  # subject_level_dataset_name
  sl_dataset_ok <- (
    assert_err(!missing(subject_level_dataset_name), "`subject_level_dataset_name` missing") &&
      assert_err(
        checkmate::test_string(subject_level_dataset_name, min.chars = 1),
        "`subject_level_dataset_name` should be a non-empty string"
      ) &&
      assert_err(
        subject_level_dataset_name %in% names(datasets),
        paste(
          "`subject_level_dataset_name` does not refer to any of the available datasets:",
          afmm_datasets
        )
      )
  )
  if (sl_dataset_ok) {
    used_dataset_names[["subject_level_dataset_name"]] <- subject_level_dataset_name
  }

  # subjid_var
  subjid_var_ok <- (
    assert_err(!missing(subjid_var), "`subjid_var` missing") &&
      assert_err(checkmate::test_string(subjid_var, min.chars = 1), "`subjid_var` should be a non-empty string") &&
      sl_dataset_ok
  )

  if (subjid_var_ok) {
    dataset <- datasets[[subject_level_dataset_name]]
    assert_err(subjid_var %in% names(dataset), "`subjid_var` not a column of `subject_level_dataset_name`")
    assert_err(
      !any(duplicated(dataset[[subjid_var]])),
      sprintf(
        "`subjid_var` (%s) does not uniquely identify rows of `subject_level_dataset_name` (%s)",
        subjid_var, subject_level_dataset_name
      )
    )
  }

  # TODO: sender_ids

  # summary
  if (!missing(summary) && !is.null(summary)) {
    if (assert_err(checkmate::test_list(summary, names = "unique"), "`summary` should be a named list")) {
      if (sl_dataset_ok && assert_err("vars" %in% names(summary), "`summary` is missing element `vars`") &&
        assert_err(
          checkmate::test_character(summary[["vars"]], null.ok = FALSE),
          "`summary$vars` should be a non-empty character vector"
        )) {
        dataset <- datasets[[subject_level_dataset_name]]
        excess_cols <- setdiff(summary[["vars"]], names(dataset))
        assert_err(
          length(excess_cols) == 0,
          sprintf(
            "`summary$vars` refers to unknown dataset variables (%s)",
            paste(excess_cols, collapse = ", ")
          )
        )
      }

      if (assert_err("column_count" %in% names(summary), "`summary` is missing element `column_count`")) {
        assert_err(
          checkmate::test_int(summary[["column_count"]], lower = 1, upper = 12),
          "`summary$column_count` is not a single integer in the [1, 12] range"
        )
      }
    }
  }

  # listings
  if (!missing(listings) && !is.null(listings)) {
    if (assert_err(checkmate::test_list(listings, names = "unique", min.len = 1), "`listings` must be a non-empty named list")) {
      for (i_listing in seq_along(listings)) {
        listing <- listings[[i_listing]]
        listing_name <- names(listings)[[i_listing]]

        code_ref <- sprintf("listings[['%s']]", listing_name)

        if (assert_err(
          checkmate::test_list(listing, names = "unique"),
          sprintf("`%s` is not a named list. ", code_ref)
        )) {
          # listing$dataset
          if (assert_err(
            "dataset" %in% names(listing),
            sprintf("`%s` is missing element `dataset`", code_ref)
          ) &&
            assert_err(
              checkmate::test_string(listing[["dataset"]], min.chars = 1),
              sprintf("`%s$dataset` should be a non-empty string", code_ref)
            ) &&
            assert_err(
              listing[["dataset"]] %in% names(datasets),
              sprintf(
                "`%s$dataset` (%s) does not refer to any of the available datasets (%s)",
                code_ref, listing[["dataset"]], afmm_datasets
              )
            )) {
            dataset_name <- listing[["dataset"]]
            used_dataset_names[[sprintf("listings[[%d]]$dataset_name", i_listing)]] <- listing[["dataset"]]

            # listing$default_vars
            if ("default_vars" %in% names(listing)) {
              default_vars <- listing[["default_vars"]]
              if (assert_err(
                checkmate::test_character(default_vars, null.ok = TRUE),
                sprintf("`%s$default_vars` should be a character vector", code_ref)
              )) {
                dataset <- datasets[[dataset_name]]
                excess_cols <- setdiff(default_vars, names(dataset))
                assert_err(
                  length(excess_cols) == 0,
                  sprintf(
                    "`%s$default_vars` refers to unknown dataset variables (%s)",
                    code_ref, paste(excess_cols, collapse = ", ")
                  )
                )
              }
            }
          }
        }
      }
    }
  }

  if (!missing(plots) && !is.null(plots)) {
    timeline_info <- plots[["timeline_info"]]
    vline_vars <- plots[["vline_vars"]]
    vline_day_numbers <- plots[["vline_day_numbers"]]
    palette <- plots[["palette"]]
    range_plots <- plots[["range_plots"]]
    value_plots <- plots[["value_plots"]]
    x_axis_unit <- plots[["x_axis_unit"]]
    x_axis_breaks <- plots[["x_axis_breaks"]]

    assert_err(
      checkmate::test_subset(x_axis_unit, choices = as.character(CONST$PLOT_X_AXIS_UNITS), empty.ok = FALSE) ||
      is.null(x_axis_unit),
      sprintf("`plots$x_axis_unit` must be `NULL` or one of [%s]", paste('"', CONST$PLOT_X_AXIS_UNITS,'"', collapse = ", "))
    )

    assert_err(
      checkmate::test_numeric(x_axis_breaks, min.len = 1, null.ok = TRUE, any.missing = FALSE),
      "`plots$x_axis_breaks` must NULL or a numeric vector with no NA values"
    )

    if(length(x_axis_breaks) == 1) {
      assert_err(
        checkmate::test_integerish(x_axis_breaks, len = 1, tol = 0, lower = 1, null.ok = TRUE),
        "when a single value is passed`plots$x_axis_breaks` must NULL or an integer larger or equal than 1"
      )
    }

    # timeline_info
    if (assert_err(
      checkmate::test_character(timeline_info, names = "unique"),
      "`plots$timeline_info` must be a named character vector"
    )) {
      timeline_col_names <- names(timeline_info)
      timeline_info_cols_compulsory <- c("trt_start_date", "trt_end_date")
      timeline_info_cols_optional <- c("icf_date", "part_end_date")

      missing_cols <- setdiff(timeline_info_cols_compulsory, timeline_col_names)
      excess_cols <- setdiff(timeline_col_names, c(timeline_info_cols_compulsory, timeline_info_cols_optional))
      if (assert_err(
        length(missing_cols) == 0,
        sprintf("`plots$timeline_info` is missing elements %s", paste(missing_cols, ", "))
      ) &&
        assert_err(
          length(excess_cols) == 0,
          sprintf("`plots$timeline_info` has excess elements %s", paste(excess_cols, ", "))
        )) {
        sl_dataset <- datasets[[subject_level_dataset_name]]

        # NOTE: Repetitions in this section are intentional. In this form they're an easier target for code generation.

        # timeline_info$icf_date
        icf_date_ok <- FALSE
        if ("icf_date" %in% names(timeline_info)) {
          col <- timeline_info[["icf_date"]]
          icf_date_ok <-
            assert_err(
              col %in% names(sl_dataset),
              sprintf(
                "`plots$timeline_info$%s` refers to column %s, which is not part of subject-level dataset %s",
                "icf_date", col, subject_level_dataset_name
              )
            ) &&
              assert_err(
                inherits(sl_dataset[[col]], allowed_classes_date),
                sprintf(
                  "`plots$timeline_info$%s` (%s) is not of allowed types (%s)",
                  "icf_date", col, paste(allowed_classes_date, collapse = ",")
                )
              )
        }
        # timeline_info$trt_start_date
        col <- timeline_info[["trt_start_date"]]
        start_date_ok <-
          assert_err(
            col %in% names(sl_dataset),
            sprintf(
              "`plots$timeline_info$%s` refers to column %s, which is not part of subject-level dataset %s",
              "trt_start_date", col, subject_level_dataset_name
            )
          ) &&
            assert_err(
              inherits(sl_dataset[[col]], allowed_classes_date),
              sprintf(
                "`plots$timeline_info$%s` (%s) is not of allowed types (%s)",
                "trt_start_date", col, paste(allowed_classes_date, collapse = ",")
              )
            ) &&
          assert_err(
            !anyNA(sl_dataset[[col]]),
            sprintf(
              "Dataset: '%s' `plots$timeline_info$%s` (%s) can not contain missing values. <br>
              trt_start_date is used as Day 1 reference date;
              together with trt_end_date, they define the extent of the x-axis",
              subject_level_dataset_name, "trt_start_date", col
            )

          )

        # timeline_info$trt_end_date
        col <- timeline_info[["trt_end_date"]]
        end_date_ok <-
          assert_err(
            col %in% names(sl_dataset),
            sprintf(
              "`plots$timeline_info$%s` refers to column %s, which is not part of subject-level dataset %s",
              "trt_end_date", col, subject_level_dataset_name
            )
          ) &&
            assert_err(
              inherits(sl_dataset[[col]], allowed_classes_date),
              sprintf(
                "`plots$timeline_info$%s` (%s) is not of allowed types (%s)",
                "trt_end_date", col, paste(allowed_classes_date, collapse = ",")
              )
            ) &&
          assert_err(
            !anyNA(sl_dataset[[col]]),
            sprintf(
              "Dataset: '%s' `plots$timeline_info$%s` (%s) can not contain missing values.
              trt_start_date is used as Day 1 reference date;
              together with trt_end_date, they define the extent of the x-axis",
              subject_level_dataset_name, "trt_end_date", col
            )
          )
        # timeline_info$part_end_date
        part_end_date_ok <- FALSE
        if ("part_end_date" %in% names(timeline_info)) {
          col <- timeline_info[["part_end_date"]]
          part_end_date_ok <-
            assert_err(
              col %in% names(sl_dataset),
              sprintf(
                "`plots$timeline_info$%s` refers to column %s, which is not part of subject-level dataset %s",
                "part_end_date", col, subject_level_dataset_name
              )
            ) &&
              assert_err(
                inherits(sl_dataset[[col]], allowed_classes_date),
                sprintf(
                  "`plots$timeline_info$%s` (%s) is not of allowed types (%s)",
                  "part_end_date", col, paste(allowed_classes_date, collapse = ",")
                )
              )
        }

        # Relationship between timeline vars (icf < start < end < part_end)
        if (icf_date_ok && start_date_ok) {
          assert_err(
            is_date_lower_or_equal(sl_dataset[[timeline_info[["icf_date"]]]], sl_dataset[[timeline_info[["trt_start_date"]]]]),
            sprintf(
              "Contents of `plots$timeline_info` column %s (%s) should be lesser than or equal to those of column %s (%s)",
              "icf_date", timeline_info[["icf_date"]], "trt_start_date", timeline_info[["trt_start_date"]]
            )
          )
        }

        if (start_date_ok && end_date_ok) {
          assert_err(
            timeline_info[["trt_start_date"]] != timeline_info[["trt_end_date"]],
            sprintf(
              "`plots$timeline_info$%s` and `plots$timeline_info$%s` refer to the same dataset variable (%s)",
              "trt_start_date", "trt_end_date", timeline_info[["trt_start_date"]]
            )
          ) &&
            assert_err(
              is_date_lower_or_equal(sl_dataset[[timeline_info[["trt_start_date"]]]], sl_dataset[[timeline_info[["trt_end_date"]]]]),
              sprintf(
                "Contents of `plots$timeline_info$%s` (%s) should be lesser than or equal to those of column %s (%s)",
                "trt_start_date", timeline_info[["trt_start_date"]], "trt_end_date", timeline_info[["trt_end_date"]]
              )
            )
        }

        if (end_date_ok && part_end_date_ok) {
          assert_err(
            is_date_lower_or_equal(sl_dataset[[timeline_info[["trt_end_date"]]]], sl_dataset[[timeline_info[["part_end_date"]]]]),
            sprintf(
              "Contents of `plots$timeline_info` column %s (%s) should be lesser than or equal to those of column %s (%s)",
              "trt_end_date", timeline_info[["trt_end_date"]], "part_end_date", timeline_info[["part_end_date"]]
            )
          )
        }
      }
    }

    # range_plots
    if (assert_err(checkmate::test_list(range_plots, names = "unique"), "`plots$range_plots` must be a named list")) {
      # [[i]]
      for (i_plot in seq_along(range_plots)) {
        plot_name <- names(range_plots)[[i_plot]]
        plot <- range_plots[[i_plot]]
        if (assert_err(
          setequal(names(plot), c("dataset", "vars", "tooltip")),
          "`plots$range_plots` needs exactly three children: `dataset`, `vars` and `tooltip`"
        ) &&
          assert_err(
            checkmate::test_string(plot[["dataset"]], min.chars = 1),
            sprintf("Field `dataset` in `plots$range_plots` element (%s) should be a non-empty string", plot_name)
          ) &&
          assert_err(
            plot[["dataset"]] %in% names(datasets),
            sprintf(
              "Field `dataset` in `plots$range_plots` element (%s) does not refer to any of the available datasets: %s",
              plot_name, paste(names(datasets), collapse = ", ")
            )
          )) {
          dataset_name <- plot[["dataset"]]

          used_dataset_names[[sprintf("plots$range_plots[[`%s`]]$dataset", plot_name)]] <- dataset_name

          dataset <- datasets[[dataset_name]]

          code_ref <- sprintf('plots$range_plots[["%s"]]', plot_name)

          # range_plots[[i]]$vars
          vars <- plot[["vars"]]
          if (assert_err(
            checkmate::test_character(vars, names = "unique"),
            sprintf("`%s$vars` must be a named character vector", code_ref)
          )) {
            var_col_names <- names(vars)

            range_plot_cols_compulsory <- c("start_date", "end_date", "decode")
            range_plot_cols_optional <- c("grading", "serious_ae")

            missing_cols <- setdiff(range_plot_cols_compulsory, var_col_names)
            excess_cols <- setdiff(var_col_names, c(range_plot_cols_compulsory, range_plot_cols_optional))
            if (assert_err(
              length(missing_cols) == 0,
              sprintf("`%s$vars` is missing elements: %s", code_ref, paste(missing_cols, collapse = ", "))
            ) &&
              assert_err(
                length(excess_cols) == 0,
                sprintf("`%s$vars` has excess elements: %s", code_ref, paste(excess_cols, collapse = ", "))
              )) {

              # range_plots[[i]]$vars$start_date
              col <- vars[["start_date"]]
              start_date_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$start_date` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_date),
                    sprintf(
                      "`%s$vars$start_date` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_date, collapse = ", ")
                    )
                  )
              # range_plots[[i]]$vars$end_date
              col <- vars[["end_date"]]
              end_date_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$end_date` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_date),
                    sprintf(
                      "`%s$vars$end_date` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_date, collapse = ", ")
                    )
                  )
              # range_plots[[i]]$vars$decode
              col <- vars[["decode"]]
              decode_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$decode` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_character_factor),
                    sprintf(
                      "`%s$vars$decode` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_character_factor, collapse = ", ")
                    )
                  )
              # range_plots[[i]]$vars$grading
              if ("grading" %in% names(vars)) {
                col <- vars[["grading"]]
                grading_ok <-
                  assert_err(
                    col %in% names(dataset),
                    sprintf(
                      "`%s$vars$grading` refers to column %s, which is not part of dataset %s",
                      code_ref, col, dataset_name
                    )
                  ) &&
                    assert_err(
                      inherits(dataset[[col]], allowed_classes_character_factor),
                      sprintf(
                        "`%s$vars$grading` column values (%s) are not of allowed types (%s)",
                        code_ref, col, paste(allowed_classes_character_factor, collapse = ", ")
                      )
                    )
              }
              # range_plots[[i]]$vars$serious_ae
              if ("serious_ae" %in% names(vars)) {
                col <- vars[["serious_ae"]]

                kind <- T_or(T_logical(), T_YN())
                serious_ae_ok <-
                  assert_err(
                    col %in% names(dataset),
                    sprintf(
                      "`%s$vars$serious_ae` refers to column %s, which is not part of dataset %s",
                      code_ref, col, dataset_name
                    )
                  ) &&
                    assert_err(
                      T_is_of_kind(dataset[[col]], kind),
                      sprintf(
                        "`%s$vars$serious_ae` column values (%s) are not of allowed types (%s)",
                        code_ref, col, T_get_type_as_text(kind)
                      )
                    )
              }

              if (start_date_ok && end_date_ok) {
                assert_err(
                  vars[["start_date"]] != vars[["end_date"]],
                  sprintf(
                    "`%s$vars$%s` and `plots$range_plots[['%s']]$vars$%s` refer to the same dataset variable (%s)",
                    code_ref, "start_date", plot_name, "end_date", vars[["start_date"]]
                  )
                ) &&
                  assert_err(
                    is_date_lower_or_equal(dataset[[vars[["start_date"]]]], dataset[[vars[["end_date"]]]]),
                    sprintf(
                      "Contents of `%s$vars$%s` (%s) should be lesser than or equal to those of column %s (%s)",
                      code_ref, "start_date", vars[["start_date"]], "end_date", vars[["end_date"]]
                    )
                  )
              }
            }
          }

          tooltip <- plot[["tooltip"]]
          if (assert_err(
            checkmate::test_character(tooltip, names = "named", null.ok = TRUE),
            sprintf("`%s$tooltip` must be a named character vector", code_ref)
          )) {
            for (i_tooltip in seq_along(tooltip)) {
              col <- tooltip[[i_tooltip]]
              assert_err(
                col %in% names(dataset),
                sprintf(
                  '`%s$tooltip[[%d]]` refers to column "%s", which is not part of dataset %s',
                  code_ref, i_tooltip, col, dataset_name
                )
              )
            }
          }
        }
      }
    }

    if (assert_err(checkmate::test_list(value_plots, names = "unique"), "`plots$value_plots` must be a named list")) {
      # [[i]]
      for (i_plot in seq_along(value_plots)) {
        plot_name <- names(value_plots)[[i_plot]]
        plot <- value_plots[[i_plot]]
        if (assert_err(
          setequal(names(plot), c("dataset", "vars", "tooltip")),
          "`plots$value_plots` needs exactly three children: `dataset`, `vars` and `tooltip`"
        ) &&
          assert_err(
            checkmate::test_string(plot[["dataset"]], min.chars = 1),
            sprintf(sprintf('`plots$value_plots[["%s"]]$dataset` should be a non-empty string', plot_name))
          ) &&
          assert_err(
            plot[["dataset"]] %in% names(datasets),
            sprintf(
              '`plots$value_plots[["%s"]]$dataset` does not refer to any of the available datasets: %s',
              plot_name, paste(names(datasets), collapse = ", ")
            )
          )) {
          dataset_name <- plot[["dataset"]]
          used_dataset_names[[sprintf("`plots$value_plots[[`%s`]]$dataset`", plot_name)]] <- dataset_name
          dataset <- datasets[[dataset_name]]

          code_ref <- sprintf('`plots$value_plots[["%s"]]`', plot_name)

          # value_plots[[i]]$vars
          vars <- plot[["vars"]]
          if (assert_err(
            checkmate::test_character(vars, names = "unique"),
            "Field `vars` in `plots$value_plots` element (%s) must be a named character vector"
          )) {
            var_col_names <- names(vars)

            value_plot_cols_compulsory <- c("analysis_param", "analysis_val", "analysis_date")
            value_plot_cols_optional <- c("range_low_limit", "range_high_limit", "analysis_indicator", "summary_stats")

            missing_cols <- setdiff(value_plot_cols_compulsory, var_col_names)
            excess_cols <- setdiff(var_col_names, c(value_plot_cols_compulsory, value_plot_cols_optional))
            if (assert_err(
              length(missing_cols) == 0,
              sprintf("`%s$vars` is missing elements: %s", code_ref, paste(missing_cols, collapse = ", "))
            ) &&
              assert_err(
                length(excess_cols) == 0,
                sprintf("`%s$vars` has excess elements: %s", code_ref, paste(excess_cols, collapse = ", "))
              )) {

              # value_plots[[i]]$vars$analysis_param
              col <- vars[["analysis_param"]]
              analysis_param_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$analysis_param` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_character_factor),
                    sprintf(
                      "`%s$vars$analysis_param` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_character_factor, collapse = ", ")
                    )
                  )
              # value_plots[[i]]$vars$analysis_val
              col <- vars[["analysis_val"]]
              analysis_val_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$analysis_val` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_numeric),
                    sprintf(
                      "`%s$vars$analysis_val` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_numeric, collapse = ", ")
                    )
                  )
              # value_plots[[i]]$vars$analysis_date
              col <- vars[["analysis_date"]]
              analysis_date_ok <-
                assert_err(
                  col %in% names(dataset),
                  sprintf(
                    "`%s$vars$analysis_date` refers to column %s, which is not part of dataset %s",
                    code_ref, col, dataset_name
                  )
                ) &&
                  assert_err(
                    inherits(dataset[[col]], allowed_classes_date),
                    sprintf(
                      "`%s$vars$analysis_date` column values (%s) are not of allowed types (%s)",
                      code_ref, col, paste(allowed_classes_date, collapse = ", ")
                    )
                  )
              # value_plots[[i]]$vars$analysis_indicator
              if ("analysis_indicator" %in% names(vars)) {
                col <- vars[["analysis_indicator"]]
                analysis_indicator_ok <-
                  assert_err(
                    col %in% names(dataset),
                    sprintf(
                      "`%s$vars$analysis_indicator` refers to column %s, which is not part of dataset %s",
                      code_ref, col, dataset_name
                    )
                  ) &&
                    assert_err(
                      inherits(dataset[[col]], allowed_classes_character_factor),
                      sprintf(
                        "`%s$vars$analysis_indicator` column values (%s) are not of allowed types (%s)",
                        code_ref, col, paste(allowed_classes_character_factor, collapse = ", ")
                      )
                    )
              }
              # value_plots[[i]]$vars$range_low_limit
              range_low_limit_ok <- FALSE
              if ("range_low_limit" %in% names(vars)) {
                col <- vars[["range_low_limit"]]
                range_low_limit_ok <-
                  assert_err(
                    col %in% names(dataset),
                    sprintf(
                      "`%s$vars$range_low_limit refers` to column %s, which is not part of dataset %s",
                      code_ref, col, dataset_name
                    )
                  ) &&
                    assert_err(
                      inherits(dataset[[col]], allowed_classes_numeric),
                      sprintf(
                        "`%s$vars$range_low_limit` column values (%s) are not of allowed types (%s)",
                        code_ref, col, paste(allowed_classes_numeric, collapse = ", ")
                      )
                    )
              }

              # value_plots[[i]]$vars$range_high_limit
              range_high_limit_ok <- FALSE
              if ("range_high_limit" %in% names(vars)) {
                col <- vars[["range_high_limit"]]
                range_high_limit_ok <-
                  assert_err(
                    col %in% names(dataset),
                    sprintf(
                      "`%s$vars$range_high_limit` refers to column %s, which is not part of dataset %s",
                      code_ref, col, dataset_name
                    )
                  ) &&
                    assert_err(
                      inherits(dataset[[col]], allowed_classes_numeric),
                      sprintf(
                        "`%s$vars$range_high_limit` column values (%s) are not of allowed types (%s)",
                        code_ref, col, paste(allowed_classes_numeric, collapse = ", ")
                      )
                    )
              }

              # value_plots[[i]]$vars$summary_stats # TODO

              if (range_low_limit_ok && range_high_limit_ok) {
                assert_err(
                  vars[["range_low_limit"]] != vars[["range_high_limit"]],
                  sprintf(
                    "`%s$vars$%s` and `%s$vars$%s` refer to the same dataset variable (%s)",
                    code_ref, "range_low_limit", code_ref, "range_high_limit", vars[["range_low_limit"]]
                  )
                ) &&
                  assert_err(
                    all(dataset[[vars[["range_low_limit"]]]] <= dataset[[vars[["range_high_limit"]]]], na.rm = TRUE),
                    sprintf(
                      "Contents of `%s$vars$%s` (%s) should be lesser than or equal to those of `%s$vars$%s` (%s)",
                      code_ref, "range_low_limit", vars[["range_low_limit"]], code_ref, "range_high_limit", vars[["range_high_limit"]]
                    )
                  )
              }
            }
          }

          tooltip <- plot[["tooltip"]]
          if (assert_err(
            checkmate::test_character(tooltip, names = "named", null.ok = TRUE),
            sprintf('`plots$value_plots[["%s"]]$tooltip` must be a named character vector', plot_name)
          )) {
            for (i_tooltip in seq_along(tooltip)) {
              col <- tooltip[[i_tooltip]]
              assert_err(
                col %in% names(dataset),
                sprintf(
                  '`plots$value_plots[["%s"]]$tooltip[[%d]]` refers to column "%s", which is not part of dataset %s',
                  plot_name, i_tooltip, col, dataset_name
                )
              )
            }
          }
        }
      }
    }

    # vline_vars
    if (assert_err(
      checkmate::test_character(vline_vars, names = "named", null.ok = TRUE),
      "`plots$vline_vars` must be a named character vector"
    )) {
      for (i_vline in seq_along(vline_vars)) {
        col <- vline_vars[[i_vline]]
        assert_err(
          col %in% names(sl_dataset),
          sprintf(
            'plots$vline_vars[[%d]] refers to column "%s", which is not part of dataset %s',
            i_vline, col, subject_level_dataset_name
          )
        )
      }
    }

    # vline_day_numbers
    assert_err(
      checkmate::test_integerish(vline_day_numbers, names = "named", null.ok = TRUE),
      "`plots$vline_day_numbers` must be a vector of named integers"
    ) &&
      assert_err(
        all(vline_day_numbers != 0),
        "`plots$vline_day_numbers` must be a valid (non-zero) CDISC Study day"
      )

    # palette
    assert_err(
      checkmate::test_character(palette, names = "named", null.ok = TRUE) &&
        all(palette %in% grDevices::colors() | grepl("^#[0-9a-fA-F]{6}$", palette)),
      "`plots$palette` must be a vector of named colors"
    )
  }

  # subjid_var present in all mentioned datasets
  if (subjid_var_ok) {
    for (i_dataset in seq_along(used_dataset_names)) {
      param <- names(used_dataset_names)[[i_dataset]]
      dataset_name <- used_dataset_names[[i_dataset]]

      dataset <- datasets[[dataset_name]]
      assert_err(
        subjid_var %in% names(dataset),
        sprintf(
          "Dataset %s (referenced from %s) is missing `subjid_var` column %s",
          dataset_name, param, subjid_var
        )
      )
    }
  }

  res <- list(warnings = warn, errors = err)

  return(res)
}


# nolint end
