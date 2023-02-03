#' Number of patients
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Count the number of unique and non-unique patients in a column (variable).
#'
#' @inheritParams argument_convention
#' @param x (`character` or `factor`) \cr vector of patient IDs.
#' @param count_by (`character` or `factor`) \cr optional vector to be combined with `x` when counting
#' `nonunique` records.
#' @param unique_count_suffix (`logical`) \cr should `"(n)"` suffix be added to `unique_count` labels.
#' Defaults to `TRUE`.
#'
#' @name summarize_num_patients
NULL

#' @describeIn summarize_num_patients Statistics function which counts the number of
#' unique patients, the corresponding percentage taken with respect to the
#' total number of patients, and the number of non-unique patients.
#'
#' @return A list with:
#'   - `unique`: vector of count and percentage.
#'   - `nonunique` : vector of count.
#'   - `unique_count`: count.
#'
#' @export
#'
#' @examples
#' # Use the statistics function to count number of unique and nonunique patients.
#' s_num_patients(x = as.character(c(1, 1, 1, 2, 4, NA)), labelstr = "", .N_col = 6L)
#' s_num_patients(
#'   x = as.character(c(1, 1, 1, 2, 4, NA)),
#'   labelstr = "",
#'   .N_col = 6L,
#'   count_by = as.character(c(1, 1, 2, 1, 1, 1))
#' )
s_num_patients <- function(x, labelstr, .N_col, count_by = NULL, unique_count_suffix = TRUE) { # nolint

  checkmate::assert_string(labelstr)
  checkmate::assert_count(.N_col)
  checkmate::assert_multi_class(x, classes = c("factor", "character"))
  checkmate::assert_flag(unique_count_suffix)

  count1 <- n_available(unique(x))
  count2 <- n_available(x)

  if (!is.null(count_by)) {
    checkmate::assert_vector(count_by, len = length(x))
    checkmate::assert_multi_class(count_by, classes = c("factor", "character"))
    count2 <- n_available(unique(interaction(x, count_by)))
  }

  out <- list(
    unique = formatters::with_label(c(count1, ifelse(count1 == 0 && .N_col == 0, 0, count1 / .N_col)), labelstr),
    nonunique = formatters::with_label(count2, labelstr),
    unique_count = formatters::with_label(count1, ifelse(unique_count_suffix, paste(labelstr, "(n)"), labelstr))
  )

  out
}

#' @describeIn summarize_num_patients Counts the number of unique patients in a column
#' (variable), the corresponding percentage taken with respect to the total
#' number of patients, and the number of non-unique patients in the column.
#' Function serves as a wrapper that carries over both expected arguments `df`
#' and `labelstr` in `cfun` of [summarize_row_groups()].
#'
#' @param required (`character` or `NULL`) optional name of a variable that is required to be non-missing.
#' @export
#'
#' @examples
#' # Count number of unique and non-unique patients.
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = as.character(c(10, 15, 10, 17, 8))
#' )
#' s_num_patients_content(df, .N_col = 5, .var = "USUBJID")
#'
#' df_by_event <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = as.character(c(10, 15, 10, 17, 8))
#' )
#' s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID")
#' s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID", count_by = "EVENT")
s_num_patients_content <- function(df, labelstr = "", .N_col, .var, required = NULL, count_by = NULL, unique_count_suffix = TRUE) { # nolint

  checkmate::assert_string(.var)
  checkmate::assert_data_frame(df)
  if (is.null(count_by)) {
    assert_df_with_variables(df, list(id = .var))
  } else {
    assert_df_with_variables(df, list(id = .var, count_by = count_by))
  }
  if (!is.null(required)) {
    checkmate::assert_string(required)
    assert_df_with_variables(df, list(required = required))
    df <- df[!is.na(df[[required]]), , drop = FALSE]
  }

  x <- df[[.var]]
  y <- switch(as.numeric(!is.null(count_by)) + 1,
    NULL,
    df[[count_by]]
  )

  s_num_patients(
    x = x,
    labelstr = labelstr,
    .N_col = .N_col,
    count_by = y,
    unique_count_suffix = unique_count_suffix
  )
}

c_num_patients <- make_afun(
  s_num_patients_content,
  .stats = c("unique", "nonunique", "unique_count"),
  .formats = c(unique = format_count_fraction_fixed_dp, nonunique = "xx", unique_count = "xx")
)

#' @describeIn summarize_num_patients Layout creating function which adds content rows using the statistics
#' function [s_num_patients_content()] and desired format.
#'
#' @export
summarize_num_patients <- function(lyt,
                                   var,
                                   .stats = NULL,
                                   .formats = NULL,
                                   .labels = c(
                                     unique = "Number of patients with at least one event",
                                     nonunique = "Number of events"
                                   ),
                                   risk_diff = NULL,
                                   ...) {
  if (is.null(.stats)) .stats <- c("unique", "nonunique", "unique_count")
  if (length(.labels) > length(.stats)) .labels <- .labels[names(.labels) %in% .stats]

  cfun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  cfun_risk_diff <- function(df, labelstr, .var, .N_col, .N_row, .spl_context) {
    n_spl <- length(.spl_context$split)
    if (.spl_context$cur_col_n[n_spl] == sum(.spl_context[[risk_diff$arm_x]][[n_spl]], .spl_context[[risk_diff$arm_y]][[n_spl]])) {
      N_col_x <- nrow(unique(df[df$ARM == risk_diff$arm_x, .var]))
      N_col_y <- nrow(unique(df[df$ARM == risk_diff$arm_y, .var]))
      s_x <- s_num_patients_content(df = df[df$ARM == risk_diff$arm_x, ], .var = .var, .N_col = N_col_x, ...)
      s_y <- s_num_patients_content(df = df[df$ARM == risk_diff$arm_y, ], .var = .var, .N_col = N_col_y, ...)
      rd_ci <- stat_risk_diff_ci(frac_x = list(s_x[c("unique")]$unique[2]), frac_y = list(s_y[c("unique")]$unique[2]),
                                 N_x = N_col_x, N_y = N_col_y, row_names = .var)
      in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)")
    } else {
      cfun(df = df, .var = .var, .N_col = .N_col, labelstr = .spl_context$value[n_spl])
    }
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = ifelse(is.null(risk_diff), cfun, cfun_risk_diff),
    extra_args = list(...)
  )
}

#' @describeIn summarize_num_patients Identically to [summarize_num_patients()],
#'   This function creates a layout which adds content rows using the statistics
#'   function [s_num_patients_content()] and desired format. Differently from its
#'   counterpart, this function does not impose the produced rows to be repeated.
#'
#' @details In general, functions that starts with `analyze*` are expected to
#'   work like [rtables::analyze()], while functions that starts with `summarize*`
#'   are based upon [rtables::summarize_row_groups()]. The latter provides a
#'   value for each dividing split in the row and column space, but, being it
#'   bound to the fundamental splits, it is repeated by design in every page
#'   when pagination is involved.
#'
#' @examples
#' df_tmp <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
#'   ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
#'   AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
#' )
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   analyze_num_patients("USUBJID", .stats = c("unique")) %>%
#'   build_table(df_tmp)
#' tbl
#'
#' @export
analyze_num_patients <- function(lyt,
                                 vars,
                                 .stats = NULL,
                                 .formats = NULL,
                                 .labels = c(
                                   unique = "Number of patients with at least one event",
                                   nonunique = "Number of events"
                                 ),
                                 show_labels = c("default", "visible", "hidden"),
                                 indent_mod = 0L,
                                 risk_diff = NULL,
                                 ...) {
  if (is.null(.stats)) .stats <- c("unique", "nonunique", "unique_count")
  if (length(.labels) > length(.stats)) .labels <- .labels[names(.labels) %in% .stats]

  afun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  afun_risk_diff <- function(df, .var, .N_col, .spl_context) {
    n_spl <- length(.spl_context$split)
    if (.spl_context$cur_col_n[n_spl] == sum(.spl_context[[risk_diff$arm_x]][[n_spl]], .spl_context[[risk_diff$arm_y]][[n_spl]])) {
      N_col_x <- nrow(unique(df[df$ARM == risk_diff$arm_x, .var]))
      N_col_y <- nrow(unique(df[df$ARM == risk_diff$arm_y, .var]))
      s_x <- s_num_patients_content(df = df[df$ARM == risk_diff$arm_x, ], .var = .var, .N_col = N_col_x, ...)
      s_y <- s_num_patients_content(df = df[df$ARM == risk_diff$arm_y, ], .var = .var, .N_col = N_col_y, ...)
      rd_ci <- stat_risk_diff_ci(frac_x = list(s_x[c("unique")]$unique[2]), frac_y = list(s_y[c("unique")]$unique[2]),
                                 N_x = N_col_x, N_y = N_col_y, row_names = vars)
      in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
    } else {
      afun(df = df, .var = .var, .N_col = .N_col)
    }
  }

  analyze(
    afun = ifelse(is.null(risk_diff), afun, afun_risk_diff),
    lyt = lyt,
    vars = vars,
    extra_args = list(...),
    show_labels = show_labels,
    indent_mod = indent_mod
  )
}
