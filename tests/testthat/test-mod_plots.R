# mod_plots tests

local({

  rfxstdt <- as.Date(rep("2020-06-01", 3))
  rfxendt <- as.Date(c("2020-06-01", "2020-06-02", "2020-09-22"))
  rficdt  <- as.Date(c("2020-05-01", "2020-05-31", "2020-06-01")) # NULL
  rfpendt <- as.Date(rep("2020-09-22", 3)) # NULL

  test_that(vdoc[["add_spec"]](
    "Timeline limits with all dates present",
    c(specs$plots$common$timeline_limits)
  ), {

    t_limits <- list()
    for (i in seq_along(rfxstdt)) {
      t_limits[[i]] <- calc_timeline_limits(
        rfxstdt = rfxstdt[i],
        rfxendt = rfxendt[i],
        rficdt = rficdt[i],
        rfpendt = rfpendt[i]
      )
    }

    expect_equal(t_limits, list(as.Date(c("2020-05-01", "2020-09-22")),
                                as.Date(c("2020-05-31", "2020-09-22")),
                                as.Date(c("2020-06-01", "2020-09-22"))))
  })

  test_that(vdoc[["add_spec"]](
    "Timeline limits with NA dates",
    c(specs$plots$common$timeline_limits)
  ), {

    t_limits <- list()
    for (i in seq_along(rfxstdt)) {
      t_limits[[i]] <- calc_timeline_limits(
        rfxstdt = rfxstdt[i],
        rfxendt = rfxendt[i],
        rficdt = as.Date(NA),
        rfpendt = as.Date(NA)
      )
    }

    expect_equal(t_limits, list(c(as.Date("2020-06-01"), Sys.Date()),
                                c(as.Date("2020-06-01"), Sys.Date()),
                                c(as.Date("2020-06-01"), Sys.Date())))
  })

  test_that(vdoc[["add_spec"]](
    "Timeline limits with NULL dates",
    c(specs$plots$common$timeline_limits)
  ), {

    t_limits <- list()
    for (i in seq_along(rfxstdt)) {
      t_limits[[i]] <- calc_timeline_limits(
        rfxstdt = rfxstdt[i],
        rfxendt = rfxendt[i],
        rficdt = NULL,
        rfpendt = NULL
      )
    }

    expect_equal(t_limits, list(as.Date(c("2020-06-01", "2020-06-01")),
                                as.Date(c("2020-06-01", "2020-06-02")),
                                as.Date(c("2020-06-01", "2020-09-22"))))
  })

  test_that(vdoc[["add_spec"]](
    "Timeline limits with NA and NULL dates",
    c(specs$plots$common$timeline_limits)
  ), {

    t_limits <- list()
    for (i in seq_along(rfxstdt)) {
      t_limits[[i]] <- calc_timeline_limits(
        rfxstdt = rfxstdt[i],
        rfxendt = as.Date(NA),
        rficdt = NULL,
        rfpendt = NULL
      )
    }

    expect_equal(t_limits, list(c(as.Date("2020-06-01"), Sys.Date()),
                                c(as.Date("2020-06-01"), Sys.Date()),
                                c(as.Date("2020-06-01"), Sys.Date())))
  })

})
