# YT#VH7d6bc2c24e816dc54e05e8110dec8313#VHef1af52f38c3fd9e7535ab6f81170fa3#
DR <- local({ # _D_ressing _R_oom
  # 2026-08-28: [cleanup] Inherit `message_well` from CM.R
  #             [feature] Call the module `check_mod_fn` instead of relying on it being wrapped by a CM$module call

  inline_shiny_input <- function(elem, label = NULL, name_selector = NULL, label_elem = NULL) {
    if (is.character(label) && length(label) == 1 && nchar(label) > 0) {
      label_elem <- shiny::tags$label(`for` = NULL, label)
    }

    res <- shiny::tags[["div"]](
      style = "display: flex; align-items: baseline; place-content: space-between; column-gap:1rem",
      label_elem, name_selector, elem
    )
    return(res)
  }

  enable_nicer_unnamed_multicolumn_selection <- TRUE
  enable_nicer_multichoice_selection <- TRUE

  color_picker_input <- function(inputId, value = NULL) {
    # https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color +
    # https://shiny.posit.co/r/articles/build/building-inputs/ +
    # https://forum.posit.co/t/bookmarking-custom-inputs/162483

    restored_value <- shiny::restoreInput(id = inputId, default = NULL)
    if (!is.null(restored_value)) value <- restored_value

    list(
      shiny::singleton(shiny::tags$head(
        shiny::tags$script("
          var color_picker_binding = new Shiny.InputBinding();
          $.extend(color_picker_binding, {
            find: function(scope) { return scope.querySelectorAll('.color-picker'); },
            getValue: function(el) { return el.value; },
            subscribe: function(el, callback) {
              $(el).on('change.color_picker_binding', function(e) { callback(); });
            },
            unsubscribe: function(el) { $(el).off('.color_picker_binding'); }
          });
          Shiny.inputBindings.register(color_picker_binding);
        ")
      )),
      shiny::tags$input(
        id = inputId, class = "color-picker", type = "color", style = "border:none", value = value
      )
    )
  }

  # NOTE: selectInput does not preserve the order of columns on bookmark restoration
  # (see https://github.com/rstudio/shiny/issues/1490, which predates trump's first presidency)
  #       Bonus: returns character(0) on empty selection to allow to distinguish it from NULL, which is
  #              the value returned when the input is not present.
  T_select_input <- function(inputId, label, choices, selected = NULL, multiple = FALSE) {
    res <- NULL
    if (multiple) {
      caller_respects_limitations <- (
        (is.null(choices) ||
          (is.character(choices) && all(nchar(choices)) > 0)) &&
          (is.character(selected) || is.null(selected))
      )
      if (!caller_respects_limitations) browser()
      stopifnot(caller_respects_limitations)

      shiny::registerInputHandler(
        type = "dressing_room.select_input", fun = function(msg, session, input_name) as.character(msg), force = TRUE
      )

      internal_id <- paste0(inputId, "_internal")
      selected <- shiny::restoreInput(id = internal_id, default = selected)

      initial_value <- paste("[", paste(sprintf("'%s'", selected), collapse = ", "), "]")

      res <- shiny::selectizeInput(
        inputId = internal_id, label = label, choices = choices, multiple = TRUE, selected = selected,
        options = list( # https://selectize.dev/docs/events
          onInitialize =
            I(sprintf(
              "function() { this.setValue(%s); Shiny.setInputValue('%s:dressing_room.select_input', %s); }",
              initial_value, inputId, initial_value
            )),
          onChange = I(sprintf("v => Shiny.setInputValue('%s:dressing_room.select_input', v)", inputId)),
          plugins = list("drag_drop", "remove_button")
        )
      )
    } else {
      res <- shiny::selectInput(inputId, label, choices, selected, selectize = FALSE)
    }
    return(res)
  }

  # Module explorer app ----
  explorer_ui <- function() {
    panel <- function(...) {
      shiny::div(
        class = "card",
        style = "background-color:#eff7ff;",
        shiny::div(class = "card-body", style = "padding-top:0.5rem; padding-left:0.5rem; padding-right:0.5rem;", ...)
      )
    }

    fix_dependencies_for_shiny_widgets_pickerInput <- local({
      # https://github.com/dreamRs/shinyWidgets/issues/147#issuecomment-459004725

      # get bootstrap dependency
      bsDep <- (shiny::bootstrapLib())()
      bsDep$name <- "bootstrap2"
      # get pickerInput dependency
      pkDep <- htmltools::findDependencies(shinyWidgets:::attachShinyWidgetsDep(htmltools::tags$div(), widget = "picker"))
      pkDep[[2]]$name <- "picker2"

      res <- list(
        htmltools::suppressDependencies("selectPicker"), htmltools::suppressDependencies("bootstrap"), # remove
        bsDep, pkDep # inject in correct order
      )
      return(res)
    })

    ui <- shiny::fluidPage(
      fix_dependencies_for_shiny_widgets_pickerInput,
      fontawesome::fa_html_dependency(),
      shiny::tags$head(shiny::HTML("<title>Davinci's Module Dressing Room</title>")),

      # shamelessly stolen from https://github.com/insightsengineering/teal.modules.bsafe/blob/main/inst/www/bsafe.css
      # and https://github.com/insightsengineering/teal.modules.bsafe/blob/main/R/mod_bsafe.R
      shiny::tags$head(shiny::tags$style(shiny::HTML("
      .toggle-label .chevron:before {
        font-size: 2rem;
        display: inline-block;
        margin-left: 1rem;
        content:'\u203A';
      }

      .toggle-checkbox:checked + .toggle-label .chevron:before {
        font-size: 2rem;
        display: inline-block;
        content:'\u203A';
        transform: rotate(90deg);
      }

      .margin_bottom_0px { margin-bottom:0px; } /* Gen-X CSS */

      .inc_dec_button {background-color:#ffffffaa; border-radius:4px; border: 1px solid #000000aa; margin-bottom:14px;}
      .inc_dec_button:hover {background-color:#eeeeeeaa; border: 1px solid #000000ff}
      "))),
      shiny::div(style = "height:1.5rem"),
      shiny::sidebarLayout(
        shiny::div(
          class = "col-sm-3",
          panel(
            shiny::h4("DaVinci's Module Dressing Room",
              align = "center",
              style = "margin-top:0; margin-bottom:2rem; font-weight:bold"
            ),
            inline_shiny_input(
              shiny::selectInput(
                inputId = "spec", label = NULL,
                choices = names(module_specifications), selected = NULL,
                selectize = FALSE
              ),
              label_elem = shiny::tags$label(`for` = "spec", "Module", title = "Module to configure")
            ),
            shiny::uiOutput("data_input")
          ),
          panel(
            shiny::uiOutput("selectors")
          ),
        ),
        shiny::mainPanel(
          width = 9, style = "padding-left:0.5rem;",
          panel(
            shiny::conditionalPanel(
              condition = "input.edit_code == false",
              shiny::uiOutput("code")
            ),
            shiny::conditionalPanel(
              condition = "input.edit_code == true",
              shiny::tags$textarea(
                id = "manual_code",
                style = "width:100%; resize:vertical; height: 300px; font-family: 'Courier New', Courier, monospace;",
                class = "form-control"
              )
            ),
            shiny::checkboxInput("edit_code", "Edit code", value = FALSE)
          ),
          shiny::uiOutput("module")
        )
      )
    )

    return(ui)
  }

  # Solves https://github.com/rstudio/shiny/issues/825#issuecomment-496679761
  observer_dedup <- local({
    # Evaluates `expr` under a reactive domain identified by `id` while keeping track of all observers created by it.
    # On repeated calls to this function, the old tracked observers are destroyed prior to evaluating `expr`.

    states <- list() # One state per `id`. Each state is an environment for mutation purposes.

    observer_dedup_func <- function(id, expr, session = shiny::getDefaultReactiveDomain(), verbose = FALSE) {
      # New state if unknown `id`
      if (!(id %in% names(states))) {
        states[[id]] <<- list2env(
          list(
            subdomain = list(end = function() NULL),
            captured_callbacks = list()
          ),
          parent = emptyenv()
        )
      }

      state <- states[[id]] # The only state that concerns us

      # Glorified append
      capture_callbacks <- function(callback) {
        return(state[["captured_callbacks"]][[length(state[["captured_callbacks"]]) + 1]] <<- callback)
      }

      make_scope_that_captures_callbacks <- function(namespace) {
        parent <- get("parent", envir = state[["subdomain"]])
        ns <- shiny::NS(namespace)
        scope <- parent$makeScope(namespace)
        overrides <- get("overrides", scope)
        overrides[["onEnded"]] <- capture_callbacks
        overrides[["makeScope"]] <- function(namespace) make_scope_that_captures_callbacks(ns(namespace))
        scope[["overrides"]] <- overrides
        return(scope)
      }

      invoke_and_remove_callbacks <- function() {
        for (cb in state[["captured_callbacks"]]) {
          if (verbose) {
            owner <- environment(cb)
            if (inherits(owner, "Observer")) {
              message(sprintf("Destroying observer %s %s", owner$.reactId, owner$.label))
            } else {
              browser()
            }
          }
          cb()
        }
        state[["captured_callbacks"]] <<- list()
      }

      state[["subdomain"]]$end() # Destroy tracked observers from the previous observer_dedup invocation
      state[["subdomain"]] <- shiny:::createSessionProxy( # Session that tracks observers even inside nested shiny modules
        session,
        makeScope = make_scope_that_captures_callbacks,
        onEnded = capture_callbacks,
        end = invoke_and_remove_callbacks
      )

      expr <- substitute(expr)
      env <- parent.frame()
      result <- shiny::withReactiveDomain(state[["subdomain"]], eval(expr, env))
      return(result)
    }
    return(observer_dedup_func)
  })

  explorer_server_with_datasets <- function(caller_datasets = NULL) {
    explorer_server <- function(input, output, session) {
      shiny::observe({
        input <- shiny::reactiveValuesToList(input)
        if ("module_id" %in% names(input)) { # FIXME: Should not depend on specific input
          session$doBookmark()
        }
      })
      shiny::onBookmarked(shiny::updateQueryString)

      datasets <- shiny::reactive({
        res <- caller_datasets
        if (is.null(res)) {
          dataset_info <- input[["datasets"]]
          shiny::req(dataset_info)
          res <- list()
          for (i_row in seq_len(nrow(dataset_info))) {
            row <- dataset_info[i_row, ]
            res[[row[["name"]]]] <- readRDS(row[["datapath"]])
          }
        }
        return(res)
      })

      well <- function(title_ui, contents, expanded, checkbox_id = NULL) {
        style <- "
          min-height: 20px;
          padding: 19px;
          padding-top: 14px;
          padding-bottom: 0px;
          padding-right: 10px;
          margin-bottom: 20px;
          background-color: #1155a00c;
          border: 1px solid #00000020;
          border-radius: 4px;
        "

        checked <- if (isFALSE(expanded)) NULL else NA # awkward mapping, but HTML is what it is

        res <- list(
          shiny::div(
            title_ui,
            shiny::tags$input(
              id = checkbox_id, type = "checkbox", class = "toggle-checkbox",
              checked = checked, style = "display: none;"
            ),
            shiny::tags$label(
              `for` = checkbox_id, class = "toggle-label",
              shiny::tags$span(class = "chevron")
            ),
            style = "display:flex; align-items:baseline;"
          )
        )

        if (length(contents)) {
          res <- append(
            res,
            list(shiny::div(
              shiny::conditionalPanel(
                condition = paste0("input['", checkbox_id, "']"),
                shiny::tags[["div"]](contents, style = style)
              )
            ))
          )
        }

        return(res)
      }

      choices_from_dataset_and_columns <- function(datasets, dataset_slot, columns) {
        res <- NULL
        if (!is.null(dataset_slot) && length(columns)) {
          # Derive choices from dataset and columns
          dataset <- datasets[[dataset_slot]]
          if (length(columns) == 1) {
            col_data <- dataset[[columns]]
            if (is.factor(col_data)) {
              res <- levels(col_data)
            } else if (is.character(col_data)) {
              res <- sort(unique(col_data))
            } else if (is.numeric(col_data)) {
              res <- sort(unique(col_data))
            } else {
              browser()
            }
          } else {
            stopifnot(length(columns) > 1)
            browser() # TODO: Implement this: only present on mod_lineplot default_visit_val at the moment
          }
        }
        return(res)
      }

      column_selector <- function(elem, datasets, visible_datasets, visible_col_selectors, inputs, id, multiple) {
        dataset_name <- elem[["dataset_name"]]
        dataset_slot <- mget(dataset_name, envir = visible_datasets, ifnotfound = list(NULL), inherits = TRUE)[[1]]

        choices <- c()
        if (!is.null(dataset_slot)) {
          dataset <- datasets[[dataset_slot]]
          for (col_name in names(dataset)) {
            if (TC$is_of_kind(dataset[[col_name]], elem[["sub_kind"]])) {
              label <- attr(dataset[[col_name]], "label")
              if (is.null(label)) {
                choices <- c(choices, col_name)
              } else {
                choices[[paste0(col_name, " [", label, "]")]] <- col_name
              }
            }
          }
        }

        if (length(choices) == 0) choices <- inputs[[id]]

        selected <- as.character(inputs[[id]])
        ui <- T_select_input(
          inputId = id, label = NULL, choices = unlist(choices), selected = selected, multiple = multiple
        )

        visible_col_selectors[[id]] <- list(dataset_slot = dataset_slot, columns = selected)

        return(ui)
      }

      compute_ui_info_inner <- function(visible_datasets, visible_col_selectors, label, name, elem, inputs, datasets, counts) {
        if (isTRUE(attr(elem, "ignore"))) {
          # NOTE: This element should have been filtered out at this point
          return(list(
            ui = shiny::p(paste("**FIXME: should have been ignored**", name)),
            input_ids = NULL, dependencies = list()
          ))
        }

        ui <- list()
        input_ids <- NULL
        deps <- list()

        if (elem[["kind"]] == "mod") {
          ui <- shiny::textInput(
            inputId = name, label = NULL, value = inputs[[name]],
            placeholder = "<shiny module ID>"
          )
          input_ids <- name
        } else if (elem[["kind"]] == "dataset_name") {
          ui <- shiny::selectInput(
            inputId = name, label = NULL,
            choices = names(datasets), selected = inputs[[name]],
            selectize = FALSE
          )

          if (!is.null(inputs[[name]]) && !is.null(label)) {
            visible_datasets[[label]] <- inputs[[name]]
          }
          input_ids <- name
          deps[[length(deps) + 1]] <- name # columns depend on datasets, so we ask for another pass
        } else if (elem[["kind"]] == "col") {
          ui <- column_selector(elem, datasets, visible_datasets, visible_col_selectors, inputs, name, multiple = FALSE)
          input_ids <- name
        } else if (elem[["kind"]] == "logical") {
          ui <- shiny::checkboxInput(inputId = name, label = NULL, value = FALSE, width = NULL)
          # FIXME: Doesn't get hackier that this alignment right here:
          ui[["children"]][[1]][["children"]][[1]][["children"]][[1]][["attribs"]][["style"]] <- "position:relative"
          input_ids <- name
        } else if (elem[["kind"]] == "integer" || elem[["kind"]] == "numeric" ||
          elem[["kind"]] == "cdisc_study_day") {
          possible_values <- c(inputs[[name]], elem[["min"]], elem[["max"]], 0)
          value <- possible_values[which(is.finite(possible_values))[1]]

          ui <- shiny::numericInput(
            inputId = name, label = NULL, value = value,
            min = elem[["min"]], max = elem[["max"]], width = "5em"
          )
          input_ids <- name
        } else if (elem[["kind"]] == "color") {
          ui <- color_picker_input(inputId = name, value = inputs[[name]])
          input_ids <- name
        } else if (elem[["kind"]] == "group") {
          child_visible_datasets <- new.env(parent = visible_datasets)
          child_visible_col_selectors <- new.env(parent = visible_col_selectors)

          input_ids <- list()
          for (child_param in seq_along(elem$elements)) {
            child_name <- names(elem$elements)[[child_param]]
            child_elem <- elem[["elements"]][[child_param]]

            label <- child_name
            if (is.null(label)) label <- sprintf("[[%d]]", child_param)

            child_info <- compute_ui_info_outer(
              child_visible_datasets,
              child_visible_col_selectors,
              label = label,
              name = paste(c(name, child_name), collapse = "-"),
              child_elem, inputs, datasets, counts
            )

            ui[[length(ui) + 1]] <- child_info[["ui"]]
            if (is.null(child_name)) {
              input_ids[[child_param]] <- child_info[["input_ids"]]
            } else {
              input_ids[[child_name]] <- child_info[["input_ids"]]
            }
            deps <- append(deps, child_info[["dependencies"]])
          }
        } else if (elem[["kind"]] == "choice_from_col_contents") {
          param <- elem$param

          choices <- character(0)
          info <- visible_col_selectors[[param]]
          if (length(info[["columns"]])) {
            choices <- choices_from_dataset_and_columns(
              datasets, info[["dataset_slot"]], info[["columns"]] # TODO(miguel): Try datasets[[dataset_slot]] instead
            )
          }

          ui <- T_select_input(
            inputId = name, label = NULL, choices = as.character(choices), selected = as.character(inputs[[name]]),
            multiple = FALSE
          )
          input_ids <- name
          deps <- c(deps, param)
        } else if (elem[["kind"]] == "choice") {
          param <- elem$param
          choices <- visible_col_selectors[[param]][["columns"]]
          ui <- T_select_input(
            inputId = name, label = NULL, choices = as.character(choices), selected = as.character(inputs[[name]]),
            multiple = FALSE
          )
          input_ids <- name
          deps <- c(deps, param)
        } else {
          ui <- shiny::p(paste("TODO: ", elem[["kind"]]))
          input_ids <- list()
        }

        attr(input_ids, "as_array") <- attr(elem, "as_array")

        return(list(ui = ui, input_ids = input_ids, dependencies = deps))
      }

      # We use -/+ buttons instead of a shiny::numericInput because that clicking that selector twice in
      # quick succession triggers a race condition in the context of renderUIs that redraw the selector itself
      # Thus, we avoid representing the absolute number of elements and instead only capture the purpose of
      # incrementing or decrementing whatever value is stored on the server
      button_delta_id <- "button_delta"
      inc_dec_buttons <- function(id) {
        list(
          shiny::tags[["button"]](
            shiny::icon("minus"), class = "inc_dec_button",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {id:'%s', delta:-1}, {priority:'event'});", button_delta_id, id
            )
          ),
          shiny::tags[["button"]](
            shiny::icon("plus"), class = "inc_dec_button",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {id:'%s', delta:+1}, {priority:'event'});", button_delta_id, id
            )
          )
        )
      }

      # Bookmark shenanigans ----
      counts_rv <- shiny::reactiveValues()
      candidate_state <- NULL

      shiny::onBookmark(function(state) {
        state$values$counts <- shiny::reactiveValuesToList(counts_rv)
      })

      shiny::onRestored(function(state) {
        candidate_state <<- state$input
        counts <- state$values$counts
        for (name in names(counts)) counts_rv[[name]] <- counts[[name]]

        if (isTRUE(state$input[["edit_code"]])) {
          shiny::updateTextAreaInput(
            session = session, "manual_code",
            value = state$input[["manual_code"]]
          )
        }
      })

      retrigger_rv <- shiny::reactiveVal(0)
      shiny::observe({
        msg <- input[[button_delta_id]]
        if (!is.null(msg)) {
          id <- msg[["id"]]
          v <- shiny::isolate(counts_rv[[id]])
          new_v <- max(0, v + msg[["delta"]])
          counts_rv[[id]] <- new_v
          if (!identical(v, new_v)) retrigger_rv(shiny::isolate(retrigger_rv() + 1))
        }
      })

      hacky_select_input_align <- function(optional_ui, label, hover_info, ui) {
        label_elem <- list(shiny::tags$span(optional_ui,
          shiny::tags$label(`for` = NULL, label, title = hover_info),
          style = "display:inline-flex; align-items:baseline; padding-top:0.7rem;"
        ))

        ui <- shiny::tags[["div"]](
          style = "display: flex; align-items: flex-start; place-content: space-between; column-gap:1rem",
          label_elem, ui
        )
        return(ui)
      }

      compute_ui_info_outer <- function(visible_datasets, visible_col_selectors, label, name, elem, inputs, datasets, counts) {
        if (isTRUE(attr(elem, "ignore"))) {
          return(list(ui = NULL, input_ids = NULL, dependencies = NULL))
        } # NOTE: early out

        res <- NULL

        docs <- attr(elem, "docs")
        doc_pieces <- paste0("[", docs[["type"]], "]")
        if (nchar(docs[["manual_desc"]]) > 0) doc_pieces <- c(doc_pieces, docs[["manual_desc"]])
        if (length(docs[["auto_desc"]])) doc_pieces <- c(doc_pieces, paste(docs[["auto_desc"]], collapse = "\n"))

        hover_info <- paste(doc_pieces, collapse = "\n")

        dependencies <- list()

        optional_ui <- NULL
        if (isTRUE(attr(elem, "optional"))) {
          checkbox_id <- paste(c(name, "enabled"), collapse = "-")
          expanded <- isTRUE(inputs[[checkbox_id]])
          checked <- if (isFALSE(expanded)) NULL else NA # awkward mapping, but HTML is what it is
          checkbox <- list(shiny::tags$input(
            id = checkbox_id, type = "checkbox", class = "optional-checkbox",
            checked = checked, style = "margin-right:0.5rem;"
          ))

          dependencies <- c(dependencies, checkbox_id)
          if (expanded) {
            optional_ui <- checkbox
          } else {
            ui <- list(shiny::div(checkbox,
              shiny::tags$label(
                `for` = checkbox_id, label,
                style = "font-weight:normal;font-style:italic;margin-bottom:0px",
                title = hover_info
              ),
              style = "display:flex;align-items:baseline;padding-bottom:1rem;"
            ))

            return(list(ui = ui, input_ids = NULL, dependencies = dependencies)) # NOTE: early out
          }
        }

        named <- isTRUE(attr(elem, "named"))
        zero_or_more <- isTRUE(attr(elem, "zero_or_more"))
        one_or_more <- isTRUE(attr(elem, "one_or_more"))
        zero_or_one_or_more <- zero_or_more || one_or_more

        if (enable_nicer_unnamed_multicolumn_selection && zero_or_one_or_more && !named && elem$kind == "col") {
          # NOTE: special-case with a multiselector for a more streamlined interface
          ui <- column_selector(elem, datasets, visible_datasets, visible_col_selectors, inputs, name, multiple = TRUE)
          input_ids <- name

          ui <- hacky_select_input_align(optional_ui, label, hover_info, ui)

          res <- list(ui = ui, input_ids = input_ids, dependencies = dependencies)
        } else if (enable_nicer_multichoice_selection && zero_or_one_or_more && elem$kind == "choice_from_col_contents") {
          param <- elem$param

          choices <- character(0)
          info <- visible_col_selectors[[param]]
          if (length(info[["columns"]])) {
            choices <- choices_from_dataset_and_columns(
              datasets, info[["dataset_slot"]], info[["columns"]] # TODO(miguel): Try datasets[[dataset_slot]] instead
            )
          }

          ui <- T_select_input(
            inputId = name, label = NULL, choices = as.character(choices), selected = as.character(inputs[[name]]),
            multiple = TRUE
          )
          input_ids <- name

          ui <- hacky_select_input_align(optional_ui, label, hover_info, ui)

          dependencies <- c(dependencies, param)

          res <- list(ui = ui, input_ids = input_ids, dependencies = dependencies)
        } else if (zero_or_one_or_more) {
          attr(elem, "zero_or_more") <- FALSE
          attr(elem, "one_or_more") <- FALSE

          input_ids <- list()

          possible_values <- c(counts[[name]], 0)
          count <- possible_values[which(is.finite(possible_values))[1]]

          counts_rv[[name]] <- ifelse(name %in% names(counts), counts[[name]], 0)

          sub_ui <- list()

          name_input_ids <- character(0)
          for (i_rep in seq_len(count)) {
            child_name <- paste0(name, "-", i_rep)

            name_selector <- NULL

            if (named) {
              name_sel_id <- paste0(child_name, "-name")
              name_input_ids <- c(name_input_ids, name_sel_id)
              name_selector <- shiny::textInput(
                inputId = name_sel_id, label = NULL,
                value = inputs[[name_sel_id]], placeholder = "<name>"
              )
            }

            child_info <- compute_ui_info_inner(
              visible_datasets, visible_col_selectors,
              label = NULL, child_name, elem,
              inputs, datasets, counts
            )
            child_ui <- child_info[["ui"]]

            if (elem$kind == "group") { # repeats #eenahw partially; not ready to compress it yet
              child_ui <- list(well(
                title_ui = name_selector, child_ui,
                expanded = inputs[[child_name]],
                checkbox_id = child_name
              ))
            } else {
              child_ui <- list(inline_shiny_input(child_ui, label = NULL, name_selector = name_selector))
            }

            sub_ui <- append(sub_ui, child_ui)
            input_ids <- append(input_ids, list(child_info[["input_ids"]]))
            dependencies <- append(dependencies, child_info[["dependencies"]])
          }
          sub_ui <- append(sub_ui, inc_dec_buttons(name))

          title_ui <- list(optional_ui, shiny::tags$label(`for` = name, class = "chevron", label, title = hover_info))
          ui <- list(well(title_ui, sub_ui, expanded = inputs[[name]], name))

          if (named) {
            attr(input_ids, "name_input_ids") <- name_input_ids
          }

          attr(input_ids, "as_array") <- attr(elem, "as_array")

          res <- list(ui = ui, input_ids = input_ids, dependencies = dependencies)
        } else {
          res <- compute_ui_info_inner(visible_datasets, visible_col_selectors, label, name, elem, inputs, datasets, counts)
          res[["dependencies"]] <- c(dependencies, res[["dependencies"]])

          if (elem$kind == "group") { # repeats #eenahw partially; not ready to compress it yet
            title_ui <- list(optional_ui, shiny::tags$label(`for` = name, class = "chevron", label, title = hover_info))
            res[["ui"]] <- list(well(title_ui, res[["ui"]],
              expanded = inputs[[name]],
              checkbox_id = name
            ))
          } else {
            if (!is.null(optional_ui)) {
              res[["ui"]] <- list(inline_shiny_input(
                res[["ui"]],
                label_elem = list(shiny::tags$span(optional_ui,
                  shiny::tags$label(`for` = NULL, label, title = hover_info),
                  style = "display:inline-flex; align-items:baseline;"
                ))
              ))
            } else {
              res[["ui"]] <- list(
                shiny::tags[["div"]](style = "display: flex; align-items: baseline; place-content: space-between; column-gap:1rem",
                  shiny::div(shiny::tags$label(`for` = NULL, label), style = "display:inline-flex;", title = hover_info), res[["ui"]]
                )
              )
            }
          }
        }

        return(res)
      }

      ui_and_ids <- shiny::reactive({
        ui <- list()
        input_ids <- list()
        deps <- list()

        selected_spec <- input[["spec"]]
        shiny::req(selected_spec)
        datasets <- datasets()

        inputs <- shiny::isolate(shiny::reactiveValuesToList(input))
        if (!is.null(candidate_state)) {
          inputs <- candidate_state
          candidate_state <<- NULL
        }

        visible_datasets <- new.env(parent = emptyenv())
        visible_col_selectors <- new.env(parent = emptyenv())
        spec_elements <- module_specifications[[selected_spec]]

        counts <- shiny::isolate(shiny::reactiveValuesToList(counts_rv))
        info <- shiny::maskReactiveContext(
          compute_ui_info_inner(visible_datasets, visible_col_selectors, "placeholder_label", name = NULL, spec_elements, inputs, datasets, counts)
        )
        ui <- info[["ui"]]
        input_ids <- info[["input_ids"]]
        deps <- info[["dependencies"]]

        for (dep in deps) input[[dep]] # makes this reactive depend only on certain inputs
        retrigger_rv()

        return(list(ui = ui, input_ids = input_ids))
      })

      output[["selectors"]] <- shiny::renderUI(ui_and_ids()[["ui"]])

      output[["data_input"]] <- shiny::renderUI({
        ui <- NULL
        if (is.null(caller_datasets)) {
          # no caller datasets: ask the user for some
          ui <- shiny::tagList(
            # Hacky alignment of fileInput
            shiny::tags[["div"]](
              style = "display: flex; align-items: flex-start; place-content: space-between; column-gap:1rem",
              shiny::tags$label(
                `for` = "datasets", "Datasets", style = "padding-top: 0.7rem;",
                title = "Input datasets for selected module"
              ),
              shiny::fileInput(
                inputId = "datasets", label = NULL, multiple = TRUE, accept = ".rds",
                capture = NULL
              )
            )
          )
        } else {
          ui <- shiny::HTML(sprintf("<b>Preloaded datasets:</b> %s", paste(names(caller_datasets), collapse = ", ")))
        }

        return(ui)
      })

      indent <- function(code, level) {
        indent_string <- strrep("  ", max(0, level))
        return(paste0(indent_string, code))
      }

      resolve_rhs <- function(id, parent_indent_level) {
        res <- ""
        if (is.character(id) && length(id) == 1) {
          param_value <- input[[id]]
          res <- deparse1(param_value)
        } else if (is.list(id)) {
          subres <- c()

          elem_names <- local({
            res <- names(id)
            name_input_ids <- attr(id, "name_input_ids")
            if (length(name_input_ids) > 0) {
              res <- Map(function(id) {
                s <- input[[id]]
                if (identical(s, "")) {
                  s <- NULL
                } else {
                  s <- deparse(s)
                }
              }, name_input_ids)
            }
            return(res)
          })

          for (i_elem in seq_along(id)) {
            name <- NULL
            if (i_elem <= length(elem_names)) name <- elem_names[[i_elem]]
            if (identical(name, "")) name <- NULL
            subres <- c(
              subres,
              paste(c(name, resolve_rhs(id[[i_elem]], parent_indent_level + 1)), collapse = " = ")
            )
          }

          as_array <- isTRUE(attr(id, "as_array"))
          container <- ifelse(as_array, "c", "list")

          res <- ""
          if (length(subres)) {
            res <- paste0(
              container,
              "(\n",
              paste(subres |> indent(parent_indent_level + 1), collapse = ",\n"),
              "\n",
              ")" |> indent(parent_indent_level)
            )
          } else {
            res <- paste0(res, container, "()")
          }
        } else {
          res <- "TODO"
        }
        return(res)
      }

      code <- shiny::reactive({
        selected_spec <- input[["spec"]]
        t <- paste0(selected_spec, "(\n")

        params <- local({
          res <- c()
          ids <- ui_and_ids()[["input_ids"]]

          flatten_id_tree <- function(tree) {
            res <- character(0)
            for (i in seq_along(tree)) {
              elem <- tree[[i]]
              if (is.character(elem)) {
                res <- c(res, elem)
              } else if (is.list(elem)) {
                res <- c(res, flatten_id_tree(elem))
              } else {
                browser()
              }

              name_input_ids <- attr(elem, "name_input_ids")
              if (!is.null(name_input_ids)) res <- c(res, name_input_ids)
            }

            return(res)
          }

          missing_ids <- setdiff(flatten_id_tree(ids), shiny::isolate(names(input)))
          for (id in missing_ids) {
            if (length(id) != 1) browser()
            input[[id]] # depend on missing expected inputs
          }
          shiny::req(length(missing_ids) == 0) # but block execution until all inputs exist

          for (i_val in seq_along(ids)) {
            param_name <- names(ids)[[i_val]]
            id <- ids[[i_val]]
            res <- c(
              res,
              paste(c(param_name, resolve_rhs(id, parent_indent_level = 1)), collapse = " = ") |> indent(1)
            )
          }
          return(res)
        })

        t <- paste0(t, paste(params, collapse = ",\n"))
        t <- paste0(t, "\n)")

        # NOTE(miguel): If you want to plug your own styler, here would be a good place to do so. Here's an (ugly) example:
        # t <- sapply(parse(text = t), function(s) deparse1(s, collapse = '\n', width.cutoff = 80L)) # nolint

        return(t)
      })

      code_highlight_differences <- function(a, b) {
        # FIXME(miguel): Not the correct place to do this operation. We could have the parse tree prior to rendering it as
        #                text, so a tree comparison would be less heuristic. That would involve building a nested list
        #                inside code()+resolve_rhs, which we would also use here. Producing the indented code from that
        #                nested list would also be cleaner than the ad-hoc formatting I do in the `code()` reactive, but...
        # NOTE(miguel):  we're in a hurry, so look ma, no hands:

        drop_trailing_comma <- function(strings) {
          return(substr(strings, 1, nchar(strings) - endsWith(strings, ",")))
        }

        as <- strsplit(a, "\n", fixed = TRUE)[[1]] |> drop_trailing_comma()
        bs <- strsplit(b, "\n", fixed = TRUE)[[1]]
        res <- character(0)
        for (e in bs) {
          indices <- which(drop_trailing_comma(e) == as)
          if (length(indices)) {
            as <- as[-indices[[1]]]
          } else {
            e <- paste0("<strong>", e, "</strong>")
          }
          res <- c(res, e)
        }
        return(paste(res, collapse = "\n"))
      }

      message_well <- function(title, contents, color = "f5f5f5") {
        style <- sprintf(
          paste0(
            "padding: 0.5rem;",
            "padding-left: 1rem;",
            "margin-bottom: 20px;",
            "background-color: %s;",
            "border: 1px solid #e3e3e3;",
            "border-radius: 4px;",
            "-webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);",
            "box-shadow: inset 0 1px 1px rgba(0,0,0,.05);"
          ),
          color
        )

        res <- list(shiny::h3(title))
        if (length(contents)) res <- append(res, list(shiny::tags[["div"]](contents, style = style)))
        return(res)
      }

      prev_code_update_delay_s <- 3 # TODO: constant
      prev_code <- prev_diffed_code <- ""
      prev_code_t <- Sys.time()

      output[["code"]] <- shiny::renderUI({ # TODO: Add 'copy' icon, like the ones on code snippets on vignettes
        code <- code()

        code <- gsub("&", "&amp;", code, fixed = TRUE, useBytes = TRUE)
        code <- gsub("<", "&lt;", code, fixed = TRUE, useBytes = TRUE)
        code <- gsub(">", "&gt;", code, fixed = TRUE, useBytes = TRUE)

        t_current <- Sys.time()
        t_diff <- as.numeric(difftime(t_current, prev_code_t, units = "secs"))
        if (prev_code_update_delay_s <= t_diff) prev_diffed_code <<- prev_code
        prev_code <<- code
        prev_code_t <<- t_current

        code_to_print <- code
        if (!identical(prev_diffed_code, code)) code_to_print <- code_highlight_differences(prev_diffed_code, code)

        res <- shiny::HTML(paste0(
          "<pre><code style='white-space: pre;'>",
          code_to_print,
          "</code></pre>"
        ))

        return(res)
      })

      shiny::observeEvent(input[["edit_code"]], {
        if (isTRUE(input[["edit_code"]])) {
          shiny::updateTextAreaInput(session = session, "manual_code", value = code())
        }
      })


      error_and_ui_rv <- shiny::reactiveValues(ui = list(), error = NULL)

      shiny::observe({
        code_to_eval <- NULL
        if (isTRUE(input[["edit_code"]])) {
          code_to_eval <- input[["manual_code"]]
          shiny::req(is.character(code_to_eval))
        } else {
          code_to_eval <- trimws(code())
        }

        get_package_maintainer_name <- function() {
          package_name <- strsplit(input[["spec"]], split = "::", fixed = TRUE)[[1]][[1]]
          desc <- utils::packageDescription(package_name)[["Maintainer"]]
          if (is.character(desc) && length(desc) == 1 && nchar(desc) > 0) {
            desc <- paste0("`", desc, "`")
          } else {
            # NOTE: Available after installing from source, but not under devtools
            desc <- "the package maintainer"
          }
          return(desc)
        }

        error_and_ui <- local({
          ui <- NULL

          spec <- input[["spec"]]

          build_error <- function(title, condition, preface, ui = NULL) {
            return(list(error = list(title = title, condition = condition, preface = preface), ui = ui))
          }

          if (!is.character(spec) || nchar(input[["spec"]]) == 0) {
            return(build_error(
              title = "Module selection error",
              condition = base::simpleError("No DaVINCI module selected on the `Module` drop-down."),
              preface = "Module selection error"
            )) # FIXME: repeats message
          }

          if (!startsWith(code_to_eval, spec)) {
            return(build_error(
              title = "Module Configuration Error.",
              condition = base::simpleError(paste("Expected call to", spec)),
              preface = "Please refer to the diagnostic messages below."
            ))
          }

          # FIXME(miguel): We should parse and evaluate arguments separately outside of a reactive environment
          #                to see if any of them is badly constructed, but here I take a shortcut and evaluate
          #                them all inside a list()
          list_of_args <- paste0("list", substr(code_to_eval, nchar(input[["spec"]]) + 1, nchar(code_to_eval)))
          parsed_code <- try(parse(text = list_of_args), silent = TRUE)
          eval_result <- try(eval(parsed_code), silent = TRUE)
          if (inherits(eval_result, "try-error")) {
            attr(eval_result, "condition")[["call"]][[1]] <- parse(text = spec)[[1]] # undo the spec->`list` substitution
            return(build_error(
              title = "Syntax error",
              condition = attr(eval_result, "condition"),
              preface = "Cannot parse the code provided."
            ))
          }

          # NOTE(miguel): With that out of the way, this shouldn't fail but I keep it just in case
          parsed_code <- try(parse(text = code_to_eval), silent = TRUE)
          if (inherits(parsed_code, "try-error")) {
            return(build_error(
              title = "Syntax error",
              condition = attr(parsed_code, "condition"),
              preface = "Cannot parse the code provided."
            ))
          }

          ui_server_id <- try(eval(parsed_code), silent = TRUE)
          shiny::req(!is.null(ui_server_id))
          if (inherits(ui_server_id, "try-error")) {
            return(build_error(
              title = "Module Development Error",
              condition = attr(ui_server_id, "condition"),
              preface = paste0("Please report the following error to ", get_package_maintainer_name(), ".")
            ))
          }

          if (length(setdiff(c("ui", "server", "module_id"), names(ui_server_id))) > 0) {
            return(build_error(
              title = "Module Configuration Error",
              condition = base::simpleError("The provided code does not return a {ui, server, module_id} triplet."),
              preface = paste0("Are you actually calling ", input[["spec"]], "?")
            ))
          }

          id <- ui_server_id[["module_id"]]
          if (is.function(ui_server_id[["ui"]])) ui <- ui_server_id[["ui"]](id)

          afmm <- list(
            data = list(DS = datasets()),
            dataset_metadata = list(name = shiny::reactive("DS")),
            unfiltered_dataset_list = datasets,
            filtered_dataset_list = datasets,
            module_output = function() list()
          )

          check_mod_fn <- ui_server_id[["meta"]][["check_mod_fn"]]
          early_error_messages <- check_mod_fn(afmm, datasets())
          
          if (length(early_error_messages) == 0) {
            server_return_val <- try(
              # Executes server on a separate reactive domain and destroys its observers when reinvoked
              observer_dedup(
                id = "unique_dedup_id",
                ui_server_id[["server"]](afmm),
                session = session
              ),
              silent = TRUE
            )
            
            if (inherits(server_return_val, "try-error")) {
              return(build_error(
                title = "Module Development Error",
                condition = attr(server_return_val, "condition"),
                preface = paste0("Please report the following error to ", get_package_maintainer_name(), "."),
              ))
            }
          } else {
            message_text <- paste(
              paste("\u2022", early_error_messages), 
              collapse = "\n"
            )
            return(build_error(
              title = "Module Configuration Error",
              condition = base::simpleError(message_text),
              preface = paste0("Please refer to the diagnostic messages below."),
              ui = ui
            ))
          }

          return(list(ui = ui))
        })

        error_and_ui_rv[["ui"]] <- error_and_ui[["ui"]]
        error_and_ui_rv[["error"]] <- error_and_ui[["error"]]
      })

      output[["module"]] <- shiny::renderUI({
        ui <- error_and_ui_rv[["ui"]]
        error <- error_and_ui_rv[["error"]]

        if (!is.null(error)) {
          error_message <- error$condition[["message"]]
          error_context <- paste(deparse(error$condition[["call"]]), collapse = "\n")

          ui <- list(
            message_well(error$title, error$preface, color = "#f4d7d7"),
            shiny::p("Messages are:"),
            shiny::pre(error_message),
            shiny::p("And happened in the vicinity of:"),
            shiny::pre(error_context),
            shiny::div(ui, style = "visibility:hidden;") # does not remove ui from layout
          )
        }

        return(ui)
      })

      NULL
    }

    return(explorer_server)
  }

  list(
    explorer_ui = explorer_ui,
    explorer_server_with_datasets = explorer_server_with_datasets
  )
})

# TODO: Dehardcode name of module in comment to allow snippet sharing across modules

#' Interactive demo/configuration tool for [mod_patient_profile()]
#'
#' Launch an interactive configuration app for [mod_patient_profile()]. This application guides the user through the
#' configuration of the module, offering an experimental point-and-click interface to the module API. Help is accessible
#' by hovering over any of the provided parameters. \cr
#' To try it using demo data, run `dv.papo::explorer_app(dv.papo:::prep_safety_data())` in your R prompt.
#'
#' @param datasets `[list(data.frame(n))]` (optional) Datasets available to the module. One of them should be a
#' demographic subject-level dataset and the rest should be visit-dependent datasets. If not provided, the UI offers a
#' file input selector that is functionally equivalent.
#'
#' @export
explorer_app <- function(datasets = NULL) {
  shiny::shinyApp(
    ui = DR$explorer_ui,
    server = DR$explorer_server_with_datasets(caller_datasets = datasets),
    enableBookmarking = "url"
  )
}
