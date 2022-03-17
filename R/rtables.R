#' Convert Table into Matrix of Strings
#'
#' Helper function to use mostly within tests.
#'
#' @param x the table
#'
#' @return matrix of strings
#'
#' @export
#'
to_string_matrix <- function(x) {
  matrix_form(x)$string
}

#' Blank for Missing Input
#'
#' Helper function to use in tabulating model results.
#'
#' @param x (`vector`)\cr input for a cell.
#' @keywords internal
#'
#' @return Either empty character vector if all entries in `x` are missing (`NA`), or otherwise
#'   the unlisted version of `x`
#'
unlist_and_blank_na <- function(x) {
  unl <- unlist(x)
  if (all(is.na(unl))) {
    character()
  } else {
    unl
  }
}

#' Constructor for Content Functions given Data Frame with Flag Input
#'
#' This can be useful for tabulating model results.
#'
#' @param analysis_var (`string`)\cr variable name for the column containing values to be
#'   returned by the content function.
#' @param flag_var (`string`)\cr variable name for the logical column identifying which
#'   row should be returned.
#' @param format (`string`)\cr rtables format to use.
#'
#' @return Content function which just gives `df$analysis_var` at the row identified by
#'   `.df_row$flag` in the given format.
#' @keywords internal
#'
cfun_by_flag <- function(analysis_var,
                         flag_var,
                         format = "xx") {
  assertthat::assert_that(
    assertthat::is.string(analysis_var),
    assertthat::is.string(flag_var)
  )
  function(df, labelstr) {
    row_index <- which(df[[flag_var]])
    x <- unlist_and_blank_na(df[[analysis_var]][row_index])
    formatable::with_label(
      rcell(x, format = format),
      labelstr
    )
  }
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
#' @keywords internal
#'
c_label_n <- function(df,
                      labelstr,
                      .N_row # nolint
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
#' @keywords internal
#'
h_col_indices <- function(table_tree, col_names) {
  assertthat::assert_that(has_tabletree_colnames(table_tree, col_names))
  match(col_names, names(table_tree))
}

#' Labels or Names of List Elements
#'
#' Internal helper function for working with nested statistic function results which typically
#' don't have labels but names that we can use.
#'
#' @param x a list
#'
#' @return a character vector with the labels or names for the list elements
#' @keywords internal
#'
labels_or_names <- function(x) {
  assertthat::assert_that(is.list(x))
  labs <- sapply(x, obj_label)
  nams <- rlang::names2(x)
  label_is_null <- sapply(labs, is.null)
  result <- unlist(ifelse(label_is_null, nams, labs))
  return(result)
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
as.rtable <- function(x, ...) { # nolint
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
as.rtable.data.frame <- function(x, format = "xx.xx", ...) { # nolint
  assertthat::assert_that(all(sapply(x, is.numeric)), msg = "only works with numeric data frame columns")
  do.call(
    rtable,
    c(
      list(
        header = labels_or_names(x),
        format = format
      ),
      Map(
        function(row, row_name) {
          do.call(
            rrow,
            c(as.list(unname(row)),
              row.name = row_name
            )
          )
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
#' h_split_param(.formats, names(.formats), f = f)
#'
#' # $surv
#' # pt_at_risk event_free_rate
#' # "xx"           "xxx"
#' #
#' # $surv_diff
#' # NULL
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
#' @keywords internal
#'
afun_selected_stats <- function(.stats, all_stats) {
  assertthat::assert_that(
    is.null(.stats) || is.character(.stats),
    is.character(all_stats)
  )
  if (is.null(.stats)) {
    all_stats
  } else {
    intersect(.stats, all_stats)
  }
}


#' Add Variable Labels to Top Left Corner in Table
#'
#' Helper layout creating function to just append the variable labels of a given variables vector
#' from a given dataset in the top left corner. If a variable label is not found then the
#' variable name itself is used instead. Multiple variable labels are concatenated with slashes.
#'
#' @note This is not an optimal implementation of course, since we are using here the data set
#'   itself during the layout creation. When we have a more mature rtables implementation then
#'   this will also be improved or not necessary anymore.
#'
#' @inheritParams argument_convention
#' @param vars (`character`)\cr variable names of which the labels are to be looked up in `df`.
#' @param indent (`integer`)\cr non-negative number of nested indent space, default to 0L which means no indent.
#' 1L means two spaces indent, 2L means four spaces indent and so on.
#'
#' @return The modified layout.
#'
#' @export
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   split_rows_by("SEX") %>%
#'   append_varlabels(DM, "SEX") %>%
#'   analyze("AGE", afun = mean) %>%
#'   append_varlabels(DM, "AGE", indent = 1)
#' build_table(lyt, DM)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX") %>%
#'   analyze("AGE", afun = mean) %>%
#'   append_varlabels(DM, c("SEX", "AGE"))
#' build_table(lyt, DM)
append_varlabels <- function(lyt, df, vars, indent = 0L) {
  if (assertthat::is.flag(indent)) {
    warning("indent argument is now accepting integers. Boolean indent will be converted to integers.")
    indent <- as.integer(indent)
  }

  assertthat::assert_that(
    is.data.frame(df),
    is.character(vars),
    is_nonnegative_count(indent)
  )

  lab <- var_labels(df[vars], fill = TRUE)
  lab <- paste(lab, collapse = " / ")
  space <- paste(rep(" ", indent * 2), collapse = "")
  lab <- paste0(space, lab)

  append_topleft(lyt, lab)
}

# Temporary fix of rtables::rbind to be removed once rtables has been fixed,
# see https://github.com/insightsengineering/tern/issues/217
rbind_fix <- function(...) {
  tabs <- list(...)
  if (length(tabs) == 1L) {
    tabs[[1L]]
  } else {
    rbind(...)
  }
}
