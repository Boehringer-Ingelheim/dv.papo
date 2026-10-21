safe_stem <- function(x) {
  x <- gsub("[[:cntrl:]]", "", x)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x) # forbidden on Windows
  x <- trimws(x)
  x <- sub("[. ]+$", "", x) # Windows silently strips trailing dots and spaces
  x[!nzchar(x)] <- "_"
  reserved <- c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9), paste0("LPT", 1:9))
  x[toupper(x) %in% reserved] <- paste0("_", x[toupper(x) %in% reserved])
  substr(x, 1, 200) # leave room under the 255-byte path component limit
}

assert_batch_dependencies <- function(plots, include_provenance, zip) {
  needed <- c(gt = "render the summary and listing tables")
  if (length(plots) > 0) needed <- c(needed, cowplot = "align the patient plots")
  if (isTRUE(include_provenance)) {
    needed <- c(
      needed,
      digest = "compute the dataset hashes",
      devtools = "report the session info"
    )
  }
  if (isTRUE(zip)) needed <- c(needed, zip = "pack the output into a zip file")

  absent <- names(needed)[!vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent) > 0) {
    stop(
      sprintf(
        "`batch_patient_profiles()` needs the following packages:\n%s",
        paste(sprintf("  - `%s`, to %s", absent, needed[absent]), collapse = "\n")
      ),
      call. = FALSE
    )
  }
}

batch_stop <- function(header, items) {
  stop(paste(c(header, paste0("  - ", items)), collapse = "\n"), call. = FALSE)
}

#' Render the patient profile PDF for a set of subjects
#'
#' The output is similar to that of `mod_patient_profile`.
#'
#' @param subjid_vals `[named character(n) | named list(n) of character]`
#'  One PDF per element. Each name is the file name of its PDF and each value contains a character vector with the 
#' subject ids.
#' @param dataset_list `[list|function]` See data requirements vignette.
#' 
#' @param output_dir `[character]`
#'  Directory the results are written to. Created if it does not exist.
#' @param zip `[logical]`
#'  Should results be packed in a single zip file?. Requires the `zip` package.
#' @param zip_name `[character]`
#'  Name of that zip file, within `output_dir`.
#' @param include_provenance `[logical]`
#'  Should data source, session info and warning summary sections be included?
#' @param author `[character]`
#'  Author, as printed on the title page of every PDF.
#' @param verbosity `[numeric]`
#'  0 No output, 1 progress output, 2 all output
#'
#' @inheritParams mod_patient_profile_params
#'
#' @return
#' Invisibly, a list with `manifest`, a data.frame with one row per PDF, and `output` the path to output dir or
#' the zip file.
#'
#' @seealso [mod_patient_profile()], `vignette("a03-batch_patient_profiles", package = "dv.papo")`
#'
#' @examples
#' \dontrun{
#' batch_patient_profiles(
#'   subjid_vals = c("01-701-1015" = "01-701-1015", "01-701-1023" = "01-701-1023"),
#'   dataset_list = dv.papo:::prep_safety_data(5),
#'   subject_level_dataset_name = "adsl",
#'   subjid_var = "USUBJID",
#'   summary = list(vars = c("SITEID", "AGE", "SEX"), column_count = 3),
#'   listings = list("Adverse Event" = list(dataset = "adae")),
#'   output_dir = "profiles"
#' )
#' }
#'
#' @export
batch_patient_profiles <- function(subjid_vals,
                                   dataset_list,
                                   subject_level_dataset_name,
                                   subjid_var,
                                   summary = NULL,
                                   listings = NULL,
                                   plots = NULL,
                                   output_dir,
                                   zip = FALSE,
                                   zip_name = "patient-profiles.zip",
                                   include_provenance = TRUE,
                                   author = "",
                                   clean = TRUE,
                                   verbosity = 1L) {  
  # Names are the output file stems
  
  checkmate::assert(
    checkmate::check_character(subjid_vals, any.missing = FALSE, min.len = 1, names = "named"),
    checkmate::check_list(subjid_vals, any.missing = FALSE, min.len = 1, types = "character", names = "named"),
    .var.name = "subjid_vals"
  )

  if(is.list(subjid_vals)) {
    do.call(      
      checkmate::assert,
      c(
        lapply(subjid_vals, \(x) checkmate::check_character(x, any.missing = FALSE, min.len = 1)),
        list(combine = "and")
      )
    )    
  }

  checkmate::assert(
    checkmate::check_list(dataset_list),
    checkmate::check_function(dataset_list),
    .var.name = "dataset_list"
  )
  checkmate::assert_string(subject_level_dataset_name, min.chars = 1)
  checkmate::assert_string(subjid_var, min.chars = 1)
  checkmate::assert_string(output_dir, min.chars = 1)
  checkmate::assert_string(zip_name, min.chars = 1)
  checkmate::assert_flag(zip)
  checkmate::assert_flag(include_provenance)
  checkmate::assert_integer(as.integer(verbosity), lower = 0, upper = 2, len = 1, any.missing = FALSE)
  if (is.function(dataset_list)) checkmate::assert_function(dataset_list, nargs = 0)

  assert_batch_dependencies(plots, include_provenance, zip)

  dataset_list <- if (is.function(dataset_list)) dataset_list() else dataset_list

  check <- check_batch_params(
    config = list(
      subject_level_dataset_name = subject_level_dataset_name,
      subjid_var = subjid_var,
      summary = summary,
      listings = listings,
      plots = plots,
      author = author
    ),
    dataset_list = dataset_list
  )
  for (message_text in check[["warnings"]]) warning(message_text, call. = FALSE)
  if (length(check[["errors"]]) > 0) {
    batch_stop("`batch_patient_profiles()` was called with an invalid configuration:", check[["errors"]])
  }
  
  duplicated_names <- unique(names(subjid_vals[duplicated(names(subjid_vals))]))
  if (length(duplicated_names) > 0) {
    batch_stop("`subjid_vals` contains duplicated names:", duplicated_names)
  }

  stems <- safe_stem(names(subjid_vals))
  collisions <- split(names(subjid_vals), stems)
  collisions <- collisions[lengths(collisions) > 1]
  if (length(collisions) > 0) {
    batch_stop(
      "Some `subjid_vals` map to the same output file name:",
      sprintf('%s -> "%s"', vapply(collisions, toString, character(1)), names(collisions))
    )
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  pdf_names <- paste0(stems, ".pdf")
  # The error reports count as targets even though they are only written on failure
  txt_names <- paste0(stems, ".txt")
  collected_names <- c(pdf_names, txt_names, "manifest.csv")
  targets <- file.path(output_dir, if (zip) zip_name else collected_names)

  existing_targets <- targets[file.exists(targets)]
  if (length(existing_targets) > 0) {
    batch_stop(
      "Refusing to overwrite. The following output files already exist:",
      existing_targets
    )
  }

  # Render each subject into a private staging directory.
  staging <- tempfile("dv_papo_batch_")
  dir.create(staging)

  if(clean) {
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  }

  template_dir <- system.file("batcher", package = "dv.papo", mustWork = TRUE)

  # Compute once identical for every subject
  provenance <- if (include_provenance) {
    list(
      mtimes = lapply(dataset_list, function(d) format(attr(d, "meta")[["mtime"]])),
      hashes = vapply(dataset_list, digest::digest, character(1)),
      session_lines = local({
        old_width <- options(width = 80)
        on.exit(options(old_width))
        utils::capture.output(devtools::session_info())
      })
    )
  }
  file.copy(file.path(template_dir, c("template.Rmd", "header.tex", "participant_chunk.Rmd")), staging)
  template <- file.path(staging, "template.Rmd")

  # messages are hard cut to console width defined by option width. Use a longer one to capture and cut later.
  old_options <- options(width = 200)
  on.exit(options(old_options), add = TRUE)

  n <- length(subjid_vals)  
  failures <- vector("list", n)  
  progress_message_fmt <- local({
    sprintf("[%%0%dd/%%d] %%s", ceiling(log10(n)))
  })

  for (i in seq_len(n)) {
    start_time <- Sys.time()
    subjid <- subjid_vals[[i]]
    entry_name <- names(subjid_vals)[[i]]
    pdf_path <- file.path(staging, pdf_names[[i]])

    if (verbosity > 0) {
      message(sprintf(progress_message_fmt, i, n, entry_name))
    }

    not_present_subjids <- subjid[!subjid %in% dataset_list[[subject_level_dataset_name]][[subjid_var]]]

    failure <- NULL
    if (length(not_present_subjids) != 0) {
      failure <- sprintf(
        "`subjids` (%s) are not values of column `%s` in dataset `%s`",
        paste(not_present_subjids, collapse = ", "),
        subjid_var,
        subject_level_dataset_name
      )
    }

    if (length(failure) == 0) {
      # Do not capture the error exclusively but also the text surrounding the error. Usually render sends
      # some extra relevant info in stderr before throwing the error.
      captured <- utils::capture.output(
        type = "message",
        outcome <- tryCatch(
          rmarkdown::render(
            input = template,
            output_file = pdf_names[[i]],
            output_dir = staging,
            params = list(
              author = author,
              include_provenance = include_provenance,
              subjid_var = subjid_var,
              subjid_vals = subjid,
              subject_level_dataset_name = subject_level_dataset_name,
              summary = summary,
              listings = listings,
              plots = plots,
              dataset_list = dataset_list,
              provenance = provenance
            ),
            # Chunk variables leak into the rendering environment. New environment for each render.
            # Knit hooks and chunk options are saved and restored by `render()` itself.
            envir = new.env(parent = globalenv()),
            quiet = verbosity < 2
          ),
          error = function(e) e
        )
      )

      if (inherits(outcome, "condition")) {
        failure <- c(conditionMessage(outcome), captured)
      } else if (verbosity > 1 && length(captured) > 0) {
        message(captured)        
      }
    }

    if (length(failure) == 0) {
      failures[[i]] <- character(0)
    } else {
      failures[[i]] <- failure
      unlink(pdf_path)

      txt_path <- file.path(staging, txt_names[[i]])
      writeLines(c(sprintf("Entry: %s", entry_name), "", failure), txt_path)

      report_location <- if (zip) {
        sprintf(
          "%s, inside %s",
          txt_names[[i]],
          file.path(output_dir, zip_name)
        )
      } else {
        file.path(output_dir, txt_names[[i]])
      }
      # Always report an error
      message(paste(
        c(
          sprintf("[%d/%d] %s: FAILED", i, n, entry_name),
          failure,
          sprintf("Error report: %s", report_location)
        ),
        collapse = "\n"
      ))
    }

    if (verbosity > 0) {
      message(paste(
        "Process time:",
        format(round(Sys.time() - start_time, digits = 2))
      ))
    }
  }

  # Collect results
  manifest <- data.frame(
    entry = names(subjid_vals),
    file = ifelse(
      vapply(failures, \(x) identical(x, character(0)), logical(1)),
      pdf_names,
      txt_names
    ),
    ok = vapply(failures, \(x) identical(x, character(0)), logical(1)),    
    error = vapply( # Remove new lines from errors
      failures,
      function(x) gsub("\n", " | ", paste(x, collapse = "\n")),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(staging, "manifest.csv"), row.names = FALSE)

  # Only copy the relevant files, pdf and txt
  produced <- collected_names[file.exists(file.path(staging, collected_names))]

  if (zip) {
    output <- file.path(output_dir, zip_name)    
    zip::zip(zipfile = file.path(normalizePath(output_dir), zip_name), files = produced, root = staging)
  } else {
    output <- output_dir
    file.copy(file.path(staging, produced), output_dir)
  }

  ok_count <- sum(manifest[["ok"]])
  message(sprintf("%d/%d rendered, %d failed", ok_count, n, n - ok_count))
  if(!clean){
    message(sprintf("Stage directory not cleaned: %s", staging))
  }

  res <- list(
    manifest = manifest,
    output = output
  )
  
  return(invisible(res))
}

# Runs module check against the batcher dataset.
# It checks the whole dataset_list, so it will fail even for non selected subjects
check_batch_params <- function(config, dataset_list) {  
  data_check <- checkmate::assert_list(
    dataset_list,
    types = "data.frame",
    any.missing = FALSE,
    min.len = 1,
    names = "unique"
  )

 checkmate::assert_string(config[["author"]])
  
  res <- check_papo_call(
    datasets = dataset_list,
    module_args = list(        
      module_id = "batcher", # dummy identifier
      subject_level_dataset_name = config[["subject_level_dataset_name"]],
      subjid_var = config[["subjid_var"]],
      # `summary$column_count` is accepted and ignored downstream
      summary = config[["summary"]],
      listings = config[["listings"]],
      plots = config[["plots"]]
    ),
    afmm_module_names = list()
  )
  errors <- res[["errors"]]
  warnings <- res[["warnings"]]

  return(list(errors = errors, warnings = warnings))
}

batcher_pt_summary_to_latex <- function(summary_res, section_str) {
  fmt <- r"--(
\beginwidepage{%d}  
\begingroup
\setlength\LTleft{0pt}\setlength\LTright{\fill}
\wptabfont
%s
\subsection{Patient Summary}
%s
\endgroup
\stopwidepage
)--"
  
  present <- NULL
  if (summary_res[["error_list"]][["any"]]()) {
    summary_res[["error_list"]][["issue_warnings"]]()
    table_latex <- "\\alertwarning{Error calculating summary}"
    latex_str <- sprintf(
      fmt,
      utils_batcher$USE_TABLE_ESTIMATION_WIDTH,
      section_str,
      table_latex
    )
    present <- TRUE
  } else {
    if (ncol(summary_res[["result"]]) == 0) {
      latex_str <- character(0)
      present <- FALSE
    } else {
      table_latex <- utils_batcher$gt_latex_table(summary_res[["result"]])
      knitr::knit_meta_add(attr(table_latex, "knit_meta"))
      latex_str <- sprintf(
        fmt,
        utils_batcher$USE_TABLE_ESTIMATION_WIDTH,
        section_str,
        table_latex
      )
      present <- TRUE
    }
  }

  res <- list(
    content = latex_str,
    present = present
  )

  return(res)  
}

# `id_prefix` must be unique in the document, it prefixes the link targets of the long cells
batcher_pt_listings_to_latex <- function(current_subjid, subjid_var, dataset_list, listings_conf, section_str, id_prefix) {
  if (length(listings_conf) == 0) {
    return(
      list(
        content = character(0),
        present = FALSE
      )
    )
  }

  listings_latex <- character(length(listings_conf))

  for (idx in seq_along(listings_conf)) {
    listing_label <- names(listings_conf)[[idx]]
    listing_conf <- listings_conf[[listing_label]]

    listing_table <- local({
      dataset <- dataset_list[[listing_conf[["dataset"]]]]

      columns <- listing_conf[["default_vars"]]
      if (is.null(columns)) {
        columns <- names(dataset)
      }

      dataset[
        dataset[[subjid_var]] == current_subjid,
        columns,
        drop = FALSE
      ]
    })

    curr_listing_latex <- utils_batcher$gt_latex_table(
      listing_table,
      long_cell_id = sprintf("%s-%d", id_prefix, idx)
    )
    knitr::knit_meta_add(attr(curr_listing_latex, "knit_meta"))

    listings_latex[[idx]] <- sprintf(
      "\\subsubsection{%s listing}\n\n%s\n\n",
      listing_label,
      curr_listing_latex
    )
  }

  fmt <- r"--(
\beginwidepage{%d}  
\begingroup
\setlength\LTleft{0pt}\setlength\LTright{\fill}
\wptabfont
%s
\subsection{Listings}
%s
\endgroup
\stopwidepage
)--"

  latex_str <- sprintf(
    fmt,
    utils_batcher$USE_TABLE_ESTIMATION_WIDTH,
    section_str,
    paste(listings_latex, collapse = "\n\n")
  )

  res <- list(
    content = latex_str,
    present = TRUE    
  )

  return(res)
}

batcher_pt_plots_to_plot_and_messages <- function(
  current_subjid,
  subjid_var,
  subject_level_dataset_name,
  dataset_list,
  plots_conf
) {
  if (length(plots_conf) == 0) {
    return(
      list(
        plot_list = list(),
        messages = list()
      )
    )
  }

  vars_as_list <- function(plot) {
    if ("vars" %in% names(plot)) {
      plot[["vars"]] <- as.list(plot[["vars"]])
    }
    plot
  }

  range_plots <- lapply(plots_conf[["range_plots"]], vars_as_list)
  value_plots <- lapply(plots_conf[["value_plots"]], vars_as_list)

  x_axis_unit <- plots_conf[["x_axis_unit"]]
  if (is.null(x_axis_unit)) {
    x_axis_unit <- CONST$PLOT_X_AXIS_UNITS$DAYS
  }

  x_axis_breaks <- plots_conf[["x_axis_breaks"]]
  if (is.null(x_axis_breaks)) {
    x_axis_breaks <- CONST$PLOT_X_AXIS_DEFAULT_NUMBER_OF_BREAKS
  }

  grading_vals <- get_grading_vals(range_plots, list(dataset_list))
  palette <- fill_palette(grading_vals, plots_conf[["palette"]])

  subject_level_dataset <- local({
    sl <- dataset_list[[subject_level_dataset_name]]
    sl[sl[[subjid_var]] == current_subjid, , drop = FALSE]
  })

  extra_datasets <- lapply(dataset_list, function(df) {
    res <- df[df[[subjid_var]] == current_subjid, , drop = FALSE]
    # rescue attributes dropped during subsetting
    for (i in seq_along(res)) {
      attributes(res[[i]]) <- attributes(df[[i]])
    }
    res
  })

  # No parameter selectors outside the app, so value plots show their default parameters
  vs_lb_selected <- stats::setNames(
    lapply(value_plots, function(plot) plot[["default_analysis_params"]]),
    sanitize_id(names(value_plots))
  )

  plots_and_messages <- compute_plots_and_messages(
    subject_level_dataset = subject_level_dataset,
    extra_datasets = extra_datasets,
    vs_lb_selected = vs_lb_selected,
    timeline_info = plots_conf[["timeline_info"]],
    range_plots = range_plots,
    value_plots = value_plots,
    vline_vars = plots_conf[["vline_vars"]],
    vline_day_numbers = plots_conf[["vline_day_numbers"]],
    x_axis_unit = x_axis_unit,
    x_axis_breaks = x_axis_breaks,
    palette = palette
  )

  return(plots_and_messages)
  
}

batcher_plots_to_grobs_and_heights <- function(plots_and_messages){

  if (length(plots_and_messages[["plot_list"]]) > 0) {
    # knitr autocrops margins of the images, we include an OFFWHITE border to avoid it
    plots <- lapply(plots_and_messages[["plot_list"]], function(p) {
      p +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(
            color = PCONSTANT$OFFWHITE
          )
        )
    })

    # Each plot is built once and the gtables are aligned. "hv" + "tblr" equalize all four margins and
    # greedy = FALSE does it column by column, which keeps the legends in place.
    grobs <- cowplot::align_plots(
      plotlist = lapply(plots, ggplot2::ggplotGrob),
      align = "hv",
      axis = "tblr",
      greedy = FALSE
    )

    # Force plots are never shorter than their legend, otherwise it gets clipped
    heights <- vapply(
      seq_along(grobs),
      function(i) {
        legend_height <- max(
          0,
          vapply(
            grobs[[i]]$grobs[grepl("guide-box", grobs[[i]]$layout$name)],
            function(legend) {
              grid::convertHeight(
                grid::grobHeight(legend),
                "in",
                valueOnly = TRUE
              )
            },
            numeric(1)
          )
        )
        max(
          1 + 2 * plots_and_messages[["plot_height_ratios"]][[i]],
          legend_height + 0.2
        )
      },
      numeric(1)
    )

    res <- list(
      grobs = grobs,
      heights = heights
    )

  } else {
    res <- list(
      grobs = list(),
      heights = list()
    )
  }

  return(res)
}

batcher_grobs_and_heights_to_chunks <- function(grobs_and_heights, chunk_label_prefix, section_str) {

  grobs <- grobs_and_heights[["grobs"]]
  heights <- grobs_and_heights[["heights"]]

  if (length(grobs) == 0) {
    return(
      list(
        content = character(0),
        present = FALSE
      )
    )
  }

    # Draw each plot with its corresponding height in a separate chunk. We use grid.draw as we have already
    # transformed them into a Grob for legend measurement. Otherwise, we double the warnings and messages as
    # we replot them.
    # Chunks are evaluated in this environment, where `grobs` lives. Their labels name the figure files, so
    # `chunk_label_prefix` must be unique in the document.
  children <- sprintf(
    "```{r %1$s-%2$d, fig.height=%3$f, fig.align='center'}\ngrid::grid.draw(grobs[[%2$d]])\n```",
    chunk_label_prefix,
    seq_along(grobs),
    heights
  )

  section_title <- sprintf("%s\n\n\\subsection{Patient Plots}\n\n", section_str)

  plots_chunks <- paste0(
    section_title,
    knitr::knit_child(
      text = paste(children, collapse = "\n\n"),
      envir = list2env(list(grobs = grobs)),
      quiet = TRUE
    )
  )

  res <- list(
    content = plots_chunks,
    present = TRUE
  )

  return(res)
}

batcher_plot_messages_to_latex <- function(messages, section_str) {
  if (length(messages) == 0) {
    return(
      list(
        content = character(0),
        present = FALSE
      )
    )
  }

  # Verbatim because messages may contain latex special characters
  fmt <- r"--(
%s
\subsection{Patient Plot messages}

\begin{Verbatim}[breaklines, breakanywhere, breaksymbolleft={}, breakindentnchars=2]
%s
\end{Verbatim}
)--"

  res <- list(
    content = sprintf(fmt, section_str, paste(messages, collapse = "\n")),
    present = TRUE
  )

  return(res)
}
