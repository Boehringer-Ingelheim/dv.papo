testthat::test_that("colour palette is completed if it misses colours for grading values", {
  # Let 'n' denote number of grading values
  n <- 20
  sample_data <- data.frame(GRADING = LETTERS[1:n])
  grading_col_pal <- structure(sample(colours(), n - 10), names = LETTERS[1:(n - 10)])
  grading_col_pal_filled <- fill_palette(sample_data[["GRADING"]], grading_col_pal)
  testthat::expect_length(grading_col_pal_filled, n + length(CONST$DEFAULT_PALETTE))
})

testthat::test_that("colour palette is filled even if pre-defined colors used up", {
  sample_data <- data.frame(GRADING = c(LETTERS[1:20], "AA", "BB", "CC"))
  grading_col_pal <- structure(
    c(
      "orange", "green", "cyan", "blue",
      "magenta", "purple", "black", "pink", "khaki",
      "turquoise", "navyblue", "violet", "yellowgreen", "skyblue",
      "indianred", "cornsilk", "chocolate", "darkgoldenrod",
      "coral", "dodgerblue"
    ),
    names = LETTERS[1:20]
  )
  grading_col_pal_filled <- fill_palette(sample_data[["GRADING"]], grading_col_pal)
  testthat::expect_length(
    grading_col_pal_filled,
    nrow(sample_data) + length(CONST$DEFAULT_PALETTE)
  )
})

testthat::test_that("get_grading_vals works if grading parameter was not specified", {
  afmm_data <- list(
    test = list(
      adae = local({
        d <- safetyData::adam_adae
        d[["AESEV"]] <- factor(d[["AESEV"]]) # gradings must be factors
        d
      }),
      cm = safetyData::sdtm_cm
      )
    )

  input_plots <- list(
    "Adverse Events Plot" = list(
      dataset = "adae",
      vars = c(
        start_date = "ASTDT",
        end_date = "AENDT",
        decode = "AEDECOD",
        grading = "AESEV",
        serious_ae = "AESER"
      )
    ),
    "Concomitant Medication Plot" = list(
      dataset = "cm",
      vars = c(
        start_date = "CMSTDT",
        end_date = "CMENDT",
        decode = "CMDECOD",
        grading = NULL
        )
    )
  )

  grading_vals <- testthat::expect_no_error(get_grading_vals(input_plots, afmm_data))
  testthat::expect_setequal(grading_vals, unique(as.character(afmm_data[["test"]][["adae"]][["AESEV"]])))

})

testthat::test_that("get_grading_vals reads grading values from dataset lists provided as lists or as functions", {
  # dv.manager hands modules factors instead of characters, and keeps dataset lists provided as functions as functions
  adae <- safetyData::adam_adae
  adae[["AESEV"]] <- factor(adae[["AESEV"]])
  # `adae` is not the first dataset of the list
  dataset_list <- list(adsl = safetyData::adam_adsl, adae = adae, cm = safetyData::sdtm_cm)

  input_plots <- list(
    "Adverse Events Plot" = list(
      dataset = "adae",
      vars = c(start_date = "ASTDT", end_date = "AENDT", decode = "AEDECOD", grading = "AESEV")
    ),
    # A plot without grading next to a factor grading
    "Concomitant Medication Plot" = list(
      dataset = "cm",
      vars = c(start_date = "CMSTDT", end_date = "CMENDT", decode = "CMDECOD")
    )
  )

  expected <- c("MILD", "MODERATE", "SEVERE")
  testthat::expect_setequal(get_grading_vals(input_plots, list(test = dataset_list)), expected)
  testthat::expect_setequal(get_grading_vals(input_plots, list(test = function() dataset_list)), expected)
})

testthat::test_that("get_grading_vals skips datasets missing from a dataset list", {
  input_plots <- list(
    "Adverse Events Plot" = list(dataset = "adae", vars = c(decode = "AEDECOD", grading = "AESEV"))
  )
  afmm_data <- list(
    with_adae = list(adae = data.frame(AESEV = factor(c("SEVERE", "MILD")))),
    without_adae = list(cm = data.frame(CMDECOD = "A"))
  )

  testthat::expect_setequal(get_grading_vals(input_plots, afmm_data), c("SEVERE", "MILD"))
})

testthat::test_that("get_grading_vals rejects grading columns that are not factors", {
  input_plots <- list(
    "Adverse Events Plot" = list(dataset = "adae", vars = c(decode = "AEDECOD", grading = "AESEV"))
  )

  testthat::expect_error(
    get_grading_vals(input_plots, list(list(adae = data.frame(AESEV = c("SEVERE", "MILD"))))),
    "is.factor"
  )
})
