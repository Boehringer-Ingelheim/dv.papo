# THIS FILE IS NAMED aaa_preface.R SO IT IS LOADED BEFORE ALL OTHER FILES
# DO NOT CHANGE ITS NAME. IT MUST BE THE FIRST ONE ALPHABETICALLY.

#' @noRd
#' @keywords internal

safe_list_ns <- local({
  # Defines a safe-list that:
  # - Fails when trying to access a non-existant element by name, either by $, [] and [[]]
  # - Disables partial matching when using $
  # - Supports numerical indexing as usual
  #
  # R does not fail when accesing non-existant elements by name, it returns NULL.
  # If we want to be defensive and forbiding access to non-existant elements, we should do that on an element by
  # element basis (!is.null, name %in names(), ...)
  # With this type we avoid including these checks and also forgetting to add a check when accessing a
  # new element.
  #
  # The intention is to use it in specific scopes where this conditions hold true.
  #
  # It exports elements individually and includes a `define_safe_list` that exports all individual elements
  # with the correct names for the operators to work

  #' @keywords internal
  safe_list <- function(...) {
    result <- list(...)
    class(result) <- c("safe_list", class(result))
    return(result)
  }

  #' @keywords internal
  `[[.safe_list` <- function(x, name) {
    if (is.character(name) && !name %in% names(x)) {
      stop(sprintf("Element '%s' not found in safe_list.", name), call. = FALSE)
    }
    NextMethod("[[")
  }

  #' @keywords internal
  `$.safe_list` <- `[[.safe_list`

  #' @keywords internal
  `[.safe_list` <- function(x, i) {
    missing_elements <- setdiff(i, names(x))
    if (is.character(i) && length(missing_elements) > 0) {
      stop(sprintf("Elements '%s' not found in safe_list", paste(missing_elements, collapse = ", ")), call. = FALSE)
    }
    x <- NextMethod("[")
    as_safe_list(x)
  }

  as_safe_list <- function(x) {
    if (!is.list(x)) {
      stop("x must be a list")
    }
    class(x) <- c("safe_list", class(x))
    x
  }

  is_safe_list <- function(x) isTRUE(is.list(x) && inherits(x, "safe_list"))

  test <- function() {
    assert <- function(expr, msg) if (!isTRUE(expr)) stop(msg)
    x <- safe_list(aa = 0, bb = 1)

    assert(is.list(x) && inherits(x, "safe_list"), "Classes are correctly set")

    assert(x[["aa"]] == 0, "Present element 'aa' can be accessed via [[]]")
    assert(x[["bb"]] == 1, "Present element 'bb' can be accessed via [[]]")

    err <- try(x[["c"]], silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via [[]]")
    assert(
      attr(err, "condition")[["message"]] == "Element 'c' not found in safe_list.",
      "Not present element 'c' cannot be accessed via [[]] and throws the correct error"
    )

    assert(x$aa == 0, "Present element 'aa' can be accessed via $")
    assert(x$bb == 1, "Present element 'bb' can be accessed via $")

    err <- try(x$c, silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via $")
    assert(
      attr(err, "condition")[["message"]] == "Element 'c' not found in safe_list.",
      "Not present element 'c' cannot be accessed via $ and throws the correct error"
    )

    err <- try(x$b, silent = TRUE)
    assert(inherits(err, "try-error"), "Partial matching is not possible via $")

    assert(isTRUE(is_safe_list(x)), "safe_list return TRUE when passed a safe_list")
    assert(isFALSE(is_safe_list(list())), "safe_list return FALSE when passed a regular list")

    err <- try(x["c"], silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via $")
    assert(
      attr(err, "condition")[["message"]] == "Elements 'c' not found in safe_list",
      "Not present element 'c' cannot be accessed via $ and throws the correct error"
    )

    assert(identical(x[c("aa")], safe_list(aa = 0)), "[] returns a subset safe_list")

    assert(x[[1]] == 0, "[[]] allows numerical indexing")
    assert(identical(x[1], safe_list(aa = 0)), "[] allows numerical indexing")

    assert(is_safe_list(as_safe_list(list(aa = 0))), "as_safe_list returns a safe_list")

    TRUE
  }

  individual_list <- list(
    safe_list = safe_list,
    "$.safe_list" = `$.safe_list`,
    "[[.safe_list" = `[[.safe_list`,
    "[.safe_list" = `[.safe_list`,
    as_safe_list = as_safe_list,
    is_safe_list = is_safe_list
  )

  c(
    individual_list,
    define_safe_list = function(env = parent.frame()) {
      list2env(individual_list, env)
      invisible(NULL)
    },
    test = test
  )
})

#' @keywords internal
safe_list <- safe_list_ns[["safe_list"]]
#' @keywords internal
`$.safe_list` <- safe_list_ns[["$.safe_list"]]
#' @keywords internal
`[[.safe_list` <- safe_list_ns[["[[.safe_list"]]
#' @keywords internal
`[.safe_list` <- safe_list_ns[["[.safe_list"]]
#' @keywords internal
as_safe_list <- safe_list_ns[["as_safe_list"]]
#' @keywords internal
is_safe_list <- safe_list_ns[["is_safe_list"]]

#' Build a collection of named constants
#'
#' @param ... Named parameters to be collected as constants
#' @details
#' Shiny uses strings as IDs to link UI and server elements. E.g:
#'   foo_UI(id = ns("foo")) ...
#'   foo_server(id = "foo")
#'
#' This pattern makes it easy for programmers to fall on the trap of modifying one instance of the string literal "foo"
#' without modifying the rest and be unaware of the problem until a bug is hit. It's also easy to mistakes uses of "foo"
#' as an identifier from other uses (text labels, ...) when, as it's often the case, the parameter is not explicitly
#' named.
#' One easy fix consists in using global variables instead of plain string literals. In the case of the previous
#' example, that would mean:
#'   ID_FOO <- "foo"
#'   foo_UI(ns(ID_FOO)) ...
#'   foo_server(ID_FOO)
#'
#' That simple addition makes the purpose of ID_FOO clear and also fails gracefully when not all ID_FOO instances are
#' updated synchronously along a codebase. It has the drawback of polluting the global namespace with identifier
#' variables. That's easily solved by creating a container of constants, which is the purpose of this pack_of_constants
#' alias.
#'  ID <- pack_of_constants(FOO = "foo", BAR = "bar")
#'  ID$FOO
#'  "foo"
#'  ID$BA
#'  Error in `$.pack_of_constants`(ID, BA) :
#'  Pack of constants "ID" does not contain "BA"
#'
#' The pack of constants is a plain named list that enforces that all elements have unique, non-null names.
#' It is tagged as an S3 object to override its extraction operators.
#'
#' The use of checkmate is unnecessary, but it's a Good Library(TM) and your module should rely on it anyways
#' @keywords internal
pack_of_constants <- function(...) {
  result <- list(...)
  checkmate::assert_list(result, any.missing = FALSE, names = "unique")
  class(result) <- c("pack_of_constants", class(result))
  result
}

#' Extract constant from pack
#'
#' @param pack pack_of_constants
#' @param name target constant
#'
#' This function differs from the base list extraction method in that it avoids partial matching of keys and throws
#' an error if the looked-for constant is not contained within the pack.
#' @keywords internal
`$.pack_of_constants` <- function(pack, name) {
  checkmate::assert_true(name %in% names(pack), .var.name = paste0(deparse(substitute(pack)), "$", name))
  NextMethod()
}

#' @keywords internal
`[[.pack_of_constants` <- `$.pack_of_constants`

#' @keywords internal
`[.pack_of_constants` <- function(pack, name) {
  stop("Invalid pack_of_constants method")
}

poc <- pack_of_constants

error_list_ns <- local({
  # Defines an error list that accumulates errors so they can be reported together instead of stopping at the
  # first one.
  #
  # Errors can be pushed as a single string, as a list of arguments for `errorCondition` or as a condition object.

  new_error_list <- function() {
    errors <- list()
    push_error <- function(x) {
      if ("condition" %in% class(x)) {
        errors[[length(errors) + 1]] <<- x
      } else {
        if (!is.list(x)) {
          x <- list(message = x)
        }

        stopifnot(is.character(x[["message"]]) && length(x[["message"]]) == 1)

        errors[[length(errors) + 1]] <<- do.call(errorCondition, x)
      }
    }

    get_errors <- function() {
      return(errors)
    }

    get_messages <- function() {
      messages <- character(0)
      for (idx in seq_along(errors)) {
        messages[[idx]] <- errors[[idx]][["message"]]
      }
      return(messages)
    }

    any_error <- function() {
      return(length(errors) > 0)
    }

    any_error_has_class <- function(class_name) {
      for (error in errors) {
        if (any(class_name %in% class(error))) {
          return(TRUE)
        }
      }
      return(FALSE)
    }

    issue_warnings <- function(prefix = "") {
      for (msg in get_messages()) {
        warning(paste0(prefix, msg))
      }
    }

    merge_errors <- function(x) {
      c(errors, x$get_errors())
    }

    return(
      as_safe_list(
        list(
          push = push_error,
          get_errors = get_errors,
          get_messages = get_messages,
          any = any_error,
          any_has_class = any_error_has_class,
          merge = merge_errors,
          issue_warnings = issue_warnings
        )
      )
    )
  }

  test <- function() {
    assert <- function(expr, msg) if (!isTRUE(expr)) stop(msg)
    collect_warnings <- function(expr) {
      warns <- character(0)
      withCallingHandlers(expr, warning = function(w) {
        warns[[length(warns) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      })
      warns
    }

    x <- new_error_list()

    assert(is_safe_list(x), "Error list is a safe_list")
    assert(isFALSE(x$any()), "New error list has no errors")
    assert(identical(x$get_errors(), list()), "New error list returns an empty list of errors")
    assert(identical(x$get_messages(), character(0)), "New error list returns no messages")

    x$push("aa")
    assert(isTRUE(x$any()), "Error list has errors after a push")
    assert(identical(x$get_messages(), "aa"), "Pushed string is stored as message")
    assert(inherits(x$get_errors()[[1]], "error"), "Pushed string is stored as an error condition")

    x$push(list(message = "bb", class = "bb_class"))
    assert(identical(x$get_messages(), c("aa", "bb")), "Pushed list is stored as message, keeping push order")
    assert(
      all(c("bb_class", "error") %in% class(x$get_errors()[[2]])),
      "Pushed list is passed to errorCondition"
    )

    cond <- warningCondition("cc", class = "cc_class")
    x$push(cond)
    assert(identical(x$get_messages(), c("aa", "bb", "cc")), "Pushed condition is stored as message")
    assert(identical(x$get_errors()[[3]], cond), "Pushed condition is stored as is")

    err <- try(x$push(1), silent = TRUE)
    assert(inherits(err, "try-error"), "Pushing a non-character message fails")
    err <- try(x$push(c("dd", "ee")), silent = TRUE)
    assert(inherits(err, "try-error"), "Pushing more than one message fails")
    err <- try(x$push(list(msg = "dd")), silent = TRUE)
    assert(inherits(err, "try-error"), "Pushing a list without message fails")
    assert(length(x$get_errors()) == 3, "Failed pushes do not modify the error list")

    assert(isTRUE(x$any_has_class("bb_class")), "any_has_class finds a class present in one error")
    assert(isTRUE(x$any_has_class(c("not_a_class", "cc_class"))), "any_has_class matches any of several classes")
    assert(isFALSE(x$any_has_class("not_a_class")), "any_has_class returns FALSE when no error has the class")
    assert(isFALSE(new_error_list()$any_has_class("error")), "any_has_class returns FALSE for an empty error list")

    y <- new_error_list()
    y$push("dd")
    merged <- x$merge(y)
    assert(
      identical(vapply(merged, conditionMessage, character(1)), c("aa", "bb", "cc", "dd")),
      "merge returns the errors of both lists, in order"
    )

    assert(
      identical(collect_warnings(x$issue_warnings("prefix:")), c("prefix:aa", "prefix:bb", "prefix:cc")),
      "issue_warnings issues one prefixed warning per error"
    )
    assert(
      identical(collect_warnings(new_error_list()$issue_warnings()), character(0)),
      "issue_warnings issues no warnings for an empty error list"
    )

    TRUE
  }

  list(
    new_error_list = new_error_list,
    test = test
  )
})

new_error_list <- error_list_ns[["new_error_list"]]
