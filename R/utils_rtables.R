# Utility functions to cooperate with {rtables} package

#' Convert Table into Matrix of Strings
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to use mostly within tests. `with_spaces`parameter allows
#' to test not only for content but also indentation and table structure.
#' `print_txt_to_copy` instead facilitate the testing development by returning a well
#' formatted text that needs only to be copied and pasted in the expected output.
#'
#' @inheritParams formatters::toString
#' @param x `rtables` table.
#' @param with_spaces (`logical`)\cr should the tested table keep the indentation and other relevant spaces?
#' @param print_txt_to_copy  (`logical`)\cr utility to have a way to copy the input table directly
#'   into the expected variable instead of copying it too manually.
#'
#' @return A `matrix` of `string`s. If `print_txt_to_copy = TRUE` the well formatted printout of the
#'   table will be printed to console, ready to be copied as a expected value.
#'
#' @examples
#' tbl <- basic_table() %>%
#'   split_rows_by("SEX") %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE") %>%
#'   build_table(tern_ex_adsl)
#'
#' to_string_matrix(tbl, widths = ceiling(propose_column_widths(tbl) / 2))
#'
#' @export
to_string_matrix <- function(x, widths = NULL, max_width = NULL,
                             hsep = formatters::default_hsep(),
                             with_spaces = TRUE, print_txt_to_copy = FALSE) {
  checkmate::assert_flag(with_spaces)
  checkmate::assert_flag(print_txt_to_copy)
  checkmate::assert_int(max_width, null.ok = TRUE)

  if (inherits(x, "MatrixPrintForm")) {
    tx <- x
  } else {
    tx <- matrix_form(x, TRUE)
  }

  tf_wrap <- FALSE
  if (!is.null(max_width)) {
    tf_wrap <- TRUE
  }

  # Producing the matrix to test
  if (with_spaces) {
    out <- strsplit(toString(tx, widths = widths, tf_wrap = tf_wrap, max_width = max_width, hsep = hsep), "\\n")[[1]]
  } else {
    out <- tx$string
  }

  # Printing to console formatted output that needs to be copied in "expected"
  if (print_txt_to_copy) {
    out_tmp <- out
    if (!with_spaces) {
      out_tmp <- apply(out, 1, paste0, collapse = '", "')
    }
    cat(paste0('c(\n  "', paste0(out_tmp, collapse = '",\n  "'), '"\n)'))
  }

  # Return values
  return(out)
}

#' Blank for Missing Input
#'
#' Helper function to use in tabulating model results.
#'
#' @param x (`vector`)\cr input for a cell.
#'
#' @return An empty `character` vector if all entries in `x` are missing (`NA`), otherwise
#'   the unlisted version of `x`.
#'
#' @keywords internal
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
#' @param analysis_var (`string`)\cr variable name for the column containing values to be returned by the
#'   content function.
#' @param flag_var (`string`)\cr variable name for the logical column identifying which row should be returned.
#' @param format (`string`)\cr `rtables` format to use.
#'
#' @return A content function which gives `df$analysis_var` at the row identified by
#'   `.df_row$flag` in the given format.
#'
#' @keywords internal
cfun_by_flag <- function(analysis_var,
                         flag_var,
                         format = "xx",
                         .indent_mods = NULL) {
  checkmate::assert_string(analysis_var)
  checkmate::assert_string(flag_var)
  function(df, labelstr) {
    row_index <- which(df[[flag_var]])
    x <- unlist_and_blank_na(df[[analysis_var]][row_index])
    formatters::with_label(
      rcell(x, format = format, indent_mod = .indent_mods),
      labelstr
    )
  }
}

#' Content Row Function to Add Row Total to Labels
#'
#' This takes the label of the latest row split level and adds the row total from `df` in parentheses.
#' This function differs from [c_label_n_alt()] by taking row counts from `df` rather than
#' `alt_counts_df`, and is used by [add_rowcounts()] when `alt_counts` is set to `FALSE`.
#'
#' @inheritParams argument_convention
#'
#' @return A list with formatted [rtables::CellValue()] with the row count value and the correct label.
#'
#' @note It is important here to not use `df` but rather `.N_row` in the implementation, because
#'   the former is already split by columns and will refer to the first column of the data only.
#'
#' @seealso [c_label_n_alt()] which performs the same function but retrieves row counts from
#'   `alt_counts_df` instead of `df`.
#'
#' @keywords internal
c_label_n <- function(df,
                      labelstr,
                      .N_row) { # nolint
  label <- paste0(labelstr, " (N=", .N_row, ")")
  in_rows(
    .list = list(row_count = formatters::with_label(c(.N_row, .N_row), label)),
    .formats = c(row_count = function(x, ...) "")
  )
}

#' Content Row Function to Add `alt_counts_df` Row Total to Labels
#'
#' This takes the label of the latest row split level and adds the row total from `alt_counts_df`
#' in parentheses. This function differs from [c_label_n()] by taking row counts from `alt_counts_df`
#' rather than `df`, and is used by [add_rowcounts()] when `alt_counts` is set to `TRUE`.
#'
#' @inheritParams argument_convention
#'
#' @return A list with formatted [rtables::CellValue()] with the row count value and the correct label.
#'
#' @seealso [c_label_n()] which performs the same function but retrieves row counts from `df` instead
#'   of `alt_counts_df`.
#'
#' @keywords internal
c_label_n_alt <- function(df,
                          labelstr,
                          .alt_df_row) {
  N_row_alt <- nrow(.alt_df_row) # nolint
  label <- paste0(labelstr, " (N=", N_row_alt, ")")
  in_rows(
    .list = list(row_count = formatters::with_label(c(N_row_alt, N_row_alt), label)),
    .formats = c(row_count = function(x, ...) "")
  )
}

#' Layout Creating Function to Add Row Total Counts
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This works analogously to [rtables::add_colcounts()] but on the rows. This function
#'  is a wrapper for [rtables::summarize_row_groups()].
#'
#' @inheritParams argument_convention
#' @param alt_counts (`flag`)\cr whether row counts should be taken from `alt_counts_df` (`TRUE`)
#'   or from `df` (`FALSE`). Defaults to `FALSE`.
#'
#' @return A modified layout where the latest row split labels now have the row-wise
#'   total counts (i.e. without column-based subsetting) attached in parentheses.
#'
#' @note Row count values are contained in these row count rows but are not displayed
#'   so that they are not considered zero rows by default when pruning.
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
#' @export
add_rowcounts <- function(lyt, alt_counts = FALSE) {
  summarize_row_groups(
    lyt,
    cfun = if (alt_counts) c_label_n_alt else c_label_n
  )
}

#' Obtain Column Indices
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to extract column indices from a `VTableTree` for a given
#' vector of column names.
#'
#' @param table_tree (`VTableTree`)\cr table to extract the indices from.
#' @param col_names (`character`)\cr vector of column names.
#'
#' @return A vector of column indices.
#'
#' @export
h_col_indices <- function(table_tree, col_names) {
  checkmate::assert_class(table_tree, "VTableNodeInfo")
  checkmate::assert_subset(col_names, names(attr(col_info(table_tree), "cextra_args")), empty.ok = FALSE)
  match(col_names, names(attr(col_info(table_tree), "cextra_args")))
}

#' Labels or Names of List Elements
#'
#' Internal helper function for working with nested statistic function results which typically
#' don't have labels but names that we can use.
#'
#' @param x a list.
#'
#' @return A `character` vector with the labels or names for the list elements.
#'
#' @keywords internal
labels_or_names <- function(x) {
  checkmate::assert_multi_class(x, c("data.frame", "list"))
  labs <- sapply(x, obj_label)
  nams <- rlang::names2(x)
  label_is_null <- sapply(labs, is.null)
  result <- unlist(ifelse(label_is_null, nams, labs))
  return(result)
}

#' Convert to `rtable`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a new generic function to convert objects to `rtable` tables.
#'
#' @param x the object which should be converted to an `rtable`.
#' @param ... additional arguments for methods.
#'
#' @return An `rtables` table object. Note that the concrete class will depend on the method used.
#'
#' @export
as.rtable <- function(x, ...) { # nolint
  UseMethod("as.rtable", x)
}

#' @describeIn as.rtable method for converting `data.frame` that contain numeric columns to `rtable`.
#'
#' @param format the format which should be used for the columns.
#'
#' @method as.rtable data.frame
#'
#' @examples
#' x <- data.frame(
#'   a = 1:10,
#'   b = rnorm(10)
#' )
#' as.rtable(x)
#'
#' @export
as.rtable.data.frame <- function(x, format = "xx.xx", ...) {
  checkmate::assert_numeric(unlist(x))
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
#' @description `r lifecycle::badge("stable")`
#'
#' It divides the data in the vector `param` into the groups defined by `f` based on specified `values`. It is relevant
#' in `rtables` layers so as to distribute parameters `.stats` or' `.formats` into lists with items corresponding to
#' specific analysis function.
#'
#' @param param (`vector`)\cr the parameter to be split.
#' @param value (`vector`)\cr the value used to split.
#' @param f (`list` of `vectors`)\cr the reference to make the split
#'
#' @return A named `list` with the same element names as `f`, each containing the elements specified in `.stats`.
#'
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
#'
#' @export
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
#' @return A `character` vector with the selected statistics.
#'
#' @keywords internal
afun_selected_stats <- function(.stats, all_stats) {
  checkmate::assert_character(.stats, null.ok = TRUE)
  checkmate::assert_character(all_stats)
  if (is.null(.stats)) {
    all_stats
  } else {
    intersect(.stats, all_stats)
  }
}

#' Add Variable Labels to Top Left Corner in Table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper layout creating function to just append the variable labels of a given variables vector
#' from a given dataset in the top left corner. If a variable label is not found then the
#' variable name itself is used instead. Multiple variable labels are concatenated with slashes.
#'
#' @inheritParams argument_convention
#' @param vars (`character`)\cr variable names of which the labels are to be looked up in `df`.
#' @param indent (`integer`)\cr non-negative number of nested indent space, default to 0L which means no indent.
#'   1L means two spaces indent, 2L means four spaces indent and so on.
#'
#' @return A modified layout with the new variable label(s) added to the top-left material.
#'
#' @note This is not an optimal implementation of course, since we are using here the data set
#'   itself during the layout creation. When we have a more mature `rtables` implementation then
#'   this will also be improved or not necessary anymore.
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
#'
#' @export
append_varlabels <- function(lyt, df, vars, indent = 0L) {
  if (checkmate::test_flag(indent)) {
    warning("indent argument is now accepting integers. Boolean indent will be converted to integers.")
    indent <- as.integer(indent)
  }

  checkmate::assert_data_frame(df)
  checkmate::assert_character(vars)
  checkmate::assert_count(indent)

  lab <- formatters::var_labels(df[vars], fill = TRUE)
  lab <- paste(lab, collapse = " / ")
  space <- paste(rep(" ", indent * 2), collapse = "")
  lab <- paste0(space, lab)

  append_topleft(lyt, lab)
}

#' Default string replacement for `NA` values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The default string used to represent `NA` values. This value is used as the default
#' value for the `na_str` argument throughout the `tern` package, and printed in place
#' of `NA` values in output tables. If not specified for each `tern` function by the user
#' via the `na_str` argument, or in the R environment options via [set_default_na_str()],
#' then `NA` is used.
#'
#' @param na_str (`string`)\cr Single string value to set in the R environment options as
#'   the default value to replace `NA`s. Use `getOption("tern_default_na_str")` to check the
#'   current value set in the R environment (defaults to `NULL` if not set).
#'
#' @name default_na_str
NULL

#' @describeIn default_na_str Getter for default `NA` value replacement string.
#'
#' @return
#' * `default_na_str` returns the current value if an R environment option has been set
#'   for `"tern_default_na_str"`, or `NA_character_` otherwise.
#'
#' @examples
#' # Default settings
#' default_na_str()
#' getOption("tern_default_na_str")
#'
#' # Set custom value
#' set_default_na_str("<Missing>")
#'
#' # Settings after value has been set
#' default_na_str()
#' getOption("tern_default_na_str")
#'
#' @export
default_na_str <- function() {
  getOption("tern_default_na_str", default = NA_character_)
}

#' @describeIn default_na_str Setter for default `NA` value replacement string. Sets the
#'   option `"tern_default_na_str"` within the R environment.
#'
#' @return
#' * `set_default_na_str` has no return value.
#'
#' @export
set_default_na_str <- function(na_str) {
  checkmate::assert_character(na_str, len = 1, null.ok = TRUE)
  options("tern_default_na_str" = na_str)
}
