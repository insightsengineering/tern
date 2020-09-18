# Prototype functions to work with rtables.
# Note: These are just prototypes. These are not final and will likely later be part of {rtables}.

#' Convert a table into a matrix of strings.
#'
#' Helper function to use mostly within tests.
#'
#' @param x the table
#'
#' @return matrix of strings
#'
#' @export
to_string_matrix <- function(x) {
  matrix_form(x)$string
}

#' Flatten a list by one level.
#'
#' Internal function used by the automatically created formatting functions.
#' Elements of the original input list which have been a list themselves are replaced
#' by the corresponding elements of the lower level list.
#'
#' @param x possibly nested list
#'
#' @return list with one list level flattened out
flatten_list <- function(x) {
  assert_that(is.list(x))
  x_mod <- Map(
    function(x, n) {
      if (is.list(x)) x else setNames(list(x), n)
    },
    x = x,
    n = names(x)
  )
  result <- do.call(c, c(x_mod, use.names = FALSE))
  return(result)
}

#' Get the length of a list and return 1 for a vector
#'
#' Internal helper function to deal with possibly nested lists.
#' We might call this result "list length" since only lists count here.
#'
#' @param x list or vector
#'
#' @return the length of the list, or 1 in case of a vector
list_length <- function(x) {
  result <- if (!is.list(x)) {
    1L
  } else {
    length(x)
  }
  return(result)
}

#' Get the list lengths of all list elements.
#'
#' Internal helper function to deal with possibly nested lists.
#'
#' @param x a list
#'
#' @return the list lengths of all elements
#' @seealso \code{\link{list_length}}
list_lengths_in_list <- function(x) {
  assert_that(is.list(x))
  vapply(x, list_length, 1L)
}

#' Get the labels or names if there are no labels from a list.
#'
#' Internal helper function for working with nested statistic function results which typically
#' don't have labels but names that we can use.
#'
#' @param x a list
#'
#' @return a character vector with the labels or names for the list elements
#'
#' @importFrom rlang names2
labels_or_names <- function(x) {
  assert_that(is.list(x))
  labs <- sapply(x, label)
  nams <- rlang::names2(x)
  label_is_null <- sapply(labs, is.null)
  result <- unlist(ifelse(label_is_null, nams, labs))
  return(result)
}

#' Compare two objects without considering attributes.
#'
#' Helper function used in format wrapper functions.
#'
#' @details This function only works on the top level of the objects,
#'   i.e. if a list is compared with another list, then we still consider
#'   attributes of the individual list elements when doing the comparison.
#'
#' @note Note that there is a difference of this function compared to the
#'   behavior of [all.equal]. For example, the latter will not return `TRUE`
#'   if `x` and `y` just differ in their class attribute, even when using it
#'   with argument `check.attributes = FALSE`.
#'
#' @param x first object
#' @param y second object
#'
#' @return A single logical value, `TRUE` or `FALSE`, never `NA` and never
#'   anything other than a single value.
identical_without_attr <- function(x, y) {
  attributes(x) <- NULL
  attributes(y) <- NULL
  identical(x, y)
}

#' Construct Formatted Analysis functions with additional formatting arguments for use with `analyze`.
#'
#' The returned function has first argument `df` or `x` and uses any additional arguments for the
#' original `sfun`. It has additional four formatting arguments:
#' \describe{
#'   \item{.stats}{character vector to select statistics from the `sfun` result}
#'   \item{.indent_mods}{named vector with custom (nonnegative) indent modifications}
#'   \item{.formats}{named vector with custom formats}
#'   \item{.labels}{named vector with custom labels}
#' }
#' Note that `.indent_mods`, `.formats` and `.labels` don't need to contain an element for each
#' statistic, as they have defaults initialized either from the arguments below (for indents and formats)
#' or from the `sfun` results (for labels).
#'
#' @param sfun the original statistics function returning a named list of results
#' @param indent_mods named vector with default (nonnegative) indent modifications
#' @param formats named vector with default formats
#'
#' @return the constructed analysis function
#'
#' @details For comparison functions it is common to return "empty" results for the
#' comparison column. This can be specified by returning empty strings `""` in the
#' corresponding statistics from the statistics function, along with the usual labels.
#' The Formatted Analysis function which is returned from these wrapper constructors
#' will then replace the old formats with the default format `"xx"` that works with these
#' empty strings. The table cells will then stay empty.
#'
#' @name format_wrap
NULL

#' @describeIn format_wrap the wrapper for `sfun` having `df` as first argument.
#' @export
#'
#' @examples
#' sfun <- function(df) {
#'   list(
#'     nrows = nrow(df),
#'     ncols = length(df)
#'   )
#' }
#' afun <- format_wrap_df(
#'   sfun,
#'   indent_mods = c(nrows = 0L, ncols = 2L),
#'   formats = c(nrows = "xx.", ncols = "xx.xx")
#' )
#' df <- data.frame(
#'   a = c(1, 2),
#'   b = c(3, 4)
#' )
#' afun(df)
#' afun(
#'   df,
#'   .indent_mods = c(nrows = 3L),
#'   .stats = "nrows",
#'   .formats = c(nrows = "xx.xx"),
#'   .labels = c(nrows = "Number of rows")
#' )
format_wrap_df <- function(sfun,
                           indent_mods,
                           formats) {
  assert_that(
    is.function(sfun),
    identical(names(formals(sfun)[1]), "df"),
    all(sapply(indent_mods, is_nonnegative_count)),
    all(sapply(formats, is_rcell_format)),
    !is.null(names(indent_mods)),
    identical(names(indent_mods), names(formats))
  )

  # Find out which rtables arguments are requested by the statistics function.
  rtables_arg_names <- c(".N_col", ".N_total", ".var", ".ref_group", ".ref_full", ".in_ref_col")
  selected_arg_names <- intersect(
    names(formals(sfun)),
    rtables_arg_names
  )

  afun <- function(df,
                   ...,
                   .stats, .indent_mods, .formats, .labels) {

    # Call statistics function with arguments df, ..., and requested rtables arguments.
    # (Note that these will be available in this function signature, see below.)
    rtables_args <- mget(selected_arg_names)
    all_args <- c(
      list(df = df),
      list(...),
      rtables_args
    )
    vals <- as.list(do.call(sfun, all_args))

    # Overwrite defaults with user choices.
    if (!missing(.formats)) {
      formats[names(.formats)] <- .formats
    }
    if (!missing(.indent_mods)) {
      indent_mods[names(.indent_mods)] <- .indent_mods
    }
    stats <- names(vals)
    if (!missing(.stats)) {
      stats <- .stats
    }

    # Subset values before formatting so we operate on top list level.
    vals <- vals[stats]

    # Replicate formats and indents to accommodate nested lists.
    rep_index <- rep(names(vals), list_lengths_in_list(vals))
    formats <- formats[rep_index]
    indent_mods <- indent_mods[rep_index]

    vals_flat <- flatten_list(vals)

    # Now we can construct labels.
    labels <- labels_or_names(vals_flat)
    if (!missing(.labels)) {
      labels[names(.labels)] <- .labels
    }

    # Handle the case of empty strings in general.
    vals_is_empty_string <- vapply(
      vals_flat,
      FUN = identical_without_attr,
      y = "",
      FUN.VALUE = TRUE
    )
    formats[vals_is_empty_string] <- "xx"

    # Do the formatting.
    vals_formatted <- mapply(
      rcell,
      x = vals_flat,
      format = formats
    )
    indented_labels <- mapply(
      function(indent, label) {
        indent_space <- paste(rep(" ", as.integer(indent)), collapse = "")
        paste0(indent_space, label)
      },
      indent = indent_mods,
      label = labels
    )

    # Put formatted values in list with labels.
    rows <- rtables::in_rows(
      .list = vals_formatted,
      .labels = indented_labels
    )
    return(rows)
  }

  # Finally add the requested rtables arguments to the formals of afun.
  add_formals <- as.pairlist(sapply(selected_arg_names, function(x) substitute()))
  new_formals <- c(formals(afun), add_formals)
  formals(afun) <- new_formals
  return(afun)
}

#' @describeIn format_wrap the wrapper for `sfun` having `x` as first argument.
#' @export
#'
#' @examples
#' sfun <- function(x) {
#'   list(
#'     n = length(x),
#'     mean = mean(x),
#'     median = median(x)
#'   )
#' }
#' afun <- format_wrap_x(
#'   sfun,
#'   indent_mods = c(n = 0L, mean = 2L, median = 1L),
#'   formats = c(n = "xx.", mean = "xx.xx", median = "xx")
#' )
#' x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
#' afun(x)
#' afun(
#'   x,
#'   .indent_mods = c(median = 3L),
#'   .stats = c("n", "median"),
#'   .formats = c(median = "xx.xx"),
#'   .labels = c(n = "Number of numbers")
#' )
format_wrap_x <- function(sfun,
                          indent_mods,
                          formats) {
  assert_that(
    is.function(sfun),
    identical(names(formals(sfun)[1]), "x"),
    all(sapply(indent_mods, is_nonnegative_count)),
    all(sapply(formats, is_rcell_format)),
    !is.null(names(indent_mods)),
    identical(names(indent_mods), names(formats))
  )

  # Find out which rtables arguments are requested by the statistics function.
  rtables_arg_names <- c(".N_col", ".N_total", ".var", ".ref_group", ".ref_full", ".in_ref_col")
  selected_arg_names <- intersect(
    names(formals(sfun)),
    rtables_arg_names
  )

  afun <- function(x,
                   ...,
                   .stats, .indent_mods, .formats, .labels) {

    # Call statistics function with arguments x, ..., and requested rtables arguments.
    # (Note that these will be available in this function signature, see below.)
    rtables_args <- mget(selected_arg_names)
    all_args <- c(
      list(x = x),
      list(...),
      rtables_args
    )
    vals <- as.list(do.call(sfun, all_args))

    # Overwrite defaults with user choices.
    if (!missing(.formats)) {
      formats[names(.formats)] <- .formats
    }
    if (!missing(.indent_mods)) {
      indent_mods[names(.indent_mods)] <- .indent_mods
    }
    stats <- names(vals)
    if (!missing(.stats)) {
      stats <- .stats
    }

    # Subset values before formatting so we operate on top list level.
    vals <- vals[stats]

    # Replicate formats and indents to accommodate nested lists.
    rep_index <- rep(names(vals), list_lengths_in_list(vals))
    formats <- formats[rep_index]
    indent_mods <- indent_mods[rep_index]

    vals_flat <- flatten_list(vals)

    # Now we can construct labels.
    labels <- labels_or_names(vals_flat)  # default labels are extracted from flattened result list.
    if (!missing(.labels)) {
      labels[names(.labels)] <- .labels
    }

    # Handle the case of empty strings in general.
    vals_is_empty_string <- vapply(
      vals_flat,
      FUN = identical_without_attr,
      y = "",
      FUN.VALUE = TRUE
    )
    formats[vals_is_empty_string] <- "xx"

    # Do the formatting.
    vals_formatted <- mapply(
      rcell,
      x = vals_flat,
      format = formats
    )
    indented_labels <- mapply(
      function(indent, label) {
        indent_space <- paste(rep(" ", as.integer(indent)), collapse = "")
        paste0(indent_space, label)
      },
      indent = indent_mods,
      label = labels
    )

    # Put formatted values in list with labels.
    rows <- rtables::in_rows(
      .list = vals_formatted,
      .labels = indented_labels
    )
    return(rows)
  }

  # Finally add the requested rtables arguments to the formals of afun.
  add_formals <- as.pairlist(sapply(selected_arg_names, function(x) substitute()))
  new_formals <- c(formals(afun), add_formals)
  formals(afun) <- new_formals
  return(afun)
}
