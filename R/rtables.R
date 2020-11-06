#' Prototype Functions to Work with `rtables`
#'
#' This summarizes the prototype functions that we currently use to work with
#' rtables. Note: These are really just prototypes. These are not final and
#' will likely later be part of rtables.
#'
#' @seealso [to_string_matrix()], [flatten_list()], [list_length()],
#'   [list_lengths_in_list()], [labels_or_names()], [identical_without_attr()],
#'   [format_wrap_df()], [format_wrap_x()]
#' @name rtables_prototypes
NULL

#' Convert Table into Matrix of Strings
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

#' Flatten a List by One Level
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

#' List Length Definition
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

#' List Lengths for a List
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

#' Labels or Names of List Elements
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
  labs <- sapply(x, obj_label)
  nams <- rlang::names2(x)
  label_is_null <- sapply(labs, is.null)
  result <- unlist(ifelse(label_is_null, nams, labs))
  return(result)
}

#' Compare Without Considering Attributes
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

#' Construct Formatted Analysis Functions
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
#'
format_wrap_df <- function(sfun, #nousage #nolint
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
  rtables_arg_names <- c(".N_row", ".N_col", ".N_total", ".var", ".df_row", ".ref_group", ".ref_full", ".in_ref_col")
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
    # Note that statistics could be duplicate in `vals` therefore the more careful
    # subsetting here.
    vals <- vals[which(names(vals) %in% stats)]

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

    indented_labels <- Map(
      function(indent, label) {
        indent_space <- paste(rep(" ", as.integer(indent)), collapse = "")
        paste0(indent_space, label)
      },
      indent = indent_mods,
      label = labels
    )

    # Do the formatting.
    vals_formatted <- Map(
      CellValue,
      val = vals_flat,
      format = formats,
      label = indented_labels
    )

    # Put formatted values in list.
    rows <- rtables::in_rows(
      .list = vals_formatted
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
#'
format_wrap_x <- function(sfun,  #nousage #nolint
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
  rtables_arg_names <- c(".N_row", ".N_col", ".N_total", ".var", ".df_row", ".ref_group", ".ref_full", ".in_ref_col")
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
    # Note that statistics could be duplicate in `vals` therefore the more careful
    # subsetting here.
    vals <- vals[which(names(vals) %in% stats)]

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

    indented_labels <- Map(
      function(indent, label) {
        indent_space <- paste(rep(" ", as.integer(indent)), collapse = "")
        paste0(indent_space, label)
      },
      indent = indent_mods,
      label = labels
    )

    # Do the formatting.
    vals_formatted <- Map(
      CellValue,
      val = vals_flat,
      format = formats,
      label = indented_labels
    )

    # Put formatted values in list.
    rows <- rtables::in_rows(
      .list = vals_formatted
    )
    return(rows)
  }

  # Finally add the requested rtables arguments to the formals of afun.
  add_formals <- as.pairlist(sapply(selected_arg_names, function(x) substitute()))
  new_formals <- c(formals(afun), add_formals)
  formals(afun) <- new_formals
  return(afun)
}

#' Content Row Function to Add Row Total to Labels
#'
#' This takes the label of the latest row split level and adds the row total in parentheses.
#'
#' @inheritParams argument_convention
#'
#' @return `CellValue` that just has the right label.
#'
#' @note Important is here to not use `df` but `.N_row` in the implementation, because the former
#'   is already split by columns and will refer to the first column of the data only.
#'
c_label_n <- function(df,
                      labelstr,
                      .N_row  #nolint
) {
  label <- paste0(labelstr, " (N=", .N_row, ")")
  CellValue(
    val = NULL,
    label = label
  )
}

#' Layout Creating Function to Add Row Total Counts
#'
#' This works analogously to [rtables::add_colcounts()] but on the rows.
#'
#' @inheritParams argument_convention
#'
#' @return The modified layout where the latest row split labels now have the row-wise
#'   total counts (i.e. without column based subsetting) attached in parentheses.
#'
#' @export
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   split_rows_by("RACE", split_fun = drop_split_levels) %>%
#'   add_rowcounts() %>%
#'   analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx") %>%
#'   build_table(DM)
#'
add_rowcounts <- function(lyt) {
  summarize_row_groups(
    lyt,
    cfun = c_label_n
  )
}

#' Obtain Column Indices
#'
#' Helper function to extract column indices from a `VTableTree` for a given
#' vector of column names.
#'
#' @param table_tree (`VTableTree`)\cr table to extract the indices from.
#' @param col_names (`character`)\cr vector of column names.
#'
#' @return the vector of column indices.
#'
h_col_indices <- function(table_tree, col_names) {
  assert_that(has_tabletree_colnames(table_tree, col_names))
  match(col_names, names(table_tree))
}

#' Convert List of Groups to Data Frame
#'
#' This converts a list of group levels into a data frame format which is expected by
#' [rtables::add_combo_levels()].
#'
#' @param groups_list (named `list` of `character`)\cr specifies the new group levels via the names and the
#'   levels that belong to it in the character vectors that are elements of the list.
#'
#' @return [tibble::tibble()] in the required format.
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' grade_groups <- list(
#'   "Any Grade (%)" = c("1", "2", "3", "4", "5"),
#'   "Grade 3-4 (%)" = c("3", "4"),
#'   "Grade 5 (%)" = "5"
#' )
#' groups_list_to_df(grade_groups)
#'
groups_list_to_df <- function(groups_list) {
  assert_that(
    is_fully_named_list(groups_list),
    all(sapply(groups_list, is.character))
  )
  tibble::tibble(
    valname = make_names(names(groups_list)),
    label = names(groups_list),
    levelcombo = unname(groups_list),
    exargs = replicate(length(groups_list), list())
  )
}

#' Split Columns by Groups of Levels
#'
#' @inheritParams argument_convention
#' @inheritParams groups_list_to_df
#' @param ... additional arguments for [rtables::split_cols_by()].
#'
#' @return the modified layout.
#' @export
#'
#' @examples
#' groups <- list(
#'   "Arms A+B" = c("A: Drug X", "B: Placebo"),
#'   "Arms A+C" = c("A: Drug X", "C: Combination")
#' )
#' basic_table() %>%
#'   split_cols_by_groups("ARM", groups) %>%
#'   add_colcounts() %>%
#'   analyze("AGE") %>%
#'   build_table(DM)
#'
split_cols_by_groups <- function(lyt, var, groups_list, ...) {
  groups_df <- groups_list_to_df(groups_list)
  split_cols_by(
    lyt = lyt,
    var = var,
    split_fun = add_combo_levels(groups_df, keep_levels = groups_df$valname),
    ...
  )
}

#' Convert to `rtable`
#'
#' This is a new generic function to convert objects to `rtable` tables.
#'
#' @param x the object which should be converted to an `rtable`.
#' @param ... additional arguments for methods.
#'
#' @return The `rtable` object. Note that the concrete class will depend on the method
#'   which is used.
#' @export
#'
as.rtable <- function(x, ...) {  #nolint
  UseMethod("as.rtable", x)
}

#' @describeIn as.rtable method for converting `data.frame` that contain numeric columns to `rtable`.
#' @param format the format which should be used for the columns.
#' @method as.rtable data.frame
#' @export
#' @examples
#' x <- data.frame(
#'   a = 1:10,
#'   b = rnorm(10)
#' )
#' as.rtable(x)
#'
as.rtable.data.frame <- function(x, format = "xx.xx", ...) { # nousage # nolint
  assert_that(all(sapply(x, is.numeric)), msg = "only works with numeric data frame columns")
  do.call(
    rtable,
    c(
      list(
        header = labels_or_names(x),
        format = format
      ),
      Map(
        function(row, row_name) {
          do.call(rrow,
                  c(as.list(unname(row)),
                    row.name = row_name))
        },
        row = as.data.frame(t(x)),
        row_name = rownames(x)
      )
    )
  )
}

#' Split parameters
#'
#' It divides the data in the vector `param` into the groups defined by `f`
#' based on specified `values`.
#' It is relevant in rtables layers so as to distribute parameters
#' `.stats` or' `.formats` into lists with items corresponding to
#' specific analysis function.
#'
#' @param param (`vector`)\cr the parameter to be split.
#' @param value (`vector`)\cr the value used to split.
#' @param f (`list` of `vectors`)\cr the reference to make the split
#'
#'
#' @export
#' @examples
#' f <- list(
#'   surv = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci"),
#'   surv_diff = c("rate_diff", "rate_diff_ci", "ztest_pval")
#' )
#'
#' .stats <- c("pt_at_risk", "rate_diff")
#' h_split_param(.stats, .stats, f = f)
#'
#' # $surv
#' # [1] "pt_at_risk"
#' #
#' # $surv_diff
#' # [1] "rate_diff"
#'
#' .formats <- c("pt_at_risk" = "xx", "event_free_rate" = "xxx")
#' h_split_param(.formats, names(.formats), f =  f)
#'
#' # $surv
#' # pt_at_risk event_free_rate
#' # "xx"           "xxx"
#' #
#' # $surv_diff
#' # NULL
#'
h_split_param <- function(param,
                          value,
                          f) {
  y <- lapply(f, function(x) param[value %in% x])
  lapply(y, function(x) if (length(x) == 0) NULL else x)
}

#' Get Selected Statistics Names
#'
#' Helper function to be used for creating `afun`.
#'
#' @param .stats (`vector` or `NULL`)\cr input to the layout creating function. Note that `NULL` means
#'   in this context that all default statistics should be used.
#' @param all_stats (`character`)\cr all statistics which can be selected here potentially.
#'
#' @return Character vector with the selected statistics.
#'
afun_selected_stats <- function(.stats, all_stats) {
  assert_that(
    is.null(.stats) || is.character(.stats),
    is.character(all_stats)
  )
  if (is.null(.stats)) {
    all_stats
  } else {
    intersect(.stats, all_stats)
  }
}
