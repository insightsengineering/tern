#' Summary of PK datasets
#'
#' These functions can be used to produce summary tables for PK datasets.
#'
#' @name summarize_pk
#'
NULL

#' @describeIn summarize_pk Statistics function that produces a named list of statistics
#'   to include as columns in the PK table.
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty then this will
#'   be used as the row label.
#'
#' @return A named list of 9 statistics:
#'   - `n`: count of complete sample size for the group.
#'   - `mean`: group data mean.
#'   - `sd`: standard deviation of the group data.
#'   - `cv`: coefficient of variation of the group.
#'   - `min`: minimum value among the group.
#'   - `max`: maximum value among the group.
#'   - `median`: median of the group data.
#'   - `geom_mean`: geometric mean of the group.
#'   - `geom_cv`: geometric coefficient of variation of the group.
#'
#' @export
#' @examples
#'
#' library(scda)
#' ADSL <- scda::synthetic_cdisc_data("latest")$adsl
#' s_summary_pk(ADSL$AGE, custom_label = "stats")
#'
s_summary_pk <- function(x,
                         labelstr = "",
                         custom_label = NULL,
                         ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }
  pk_stats <- c("n", "mean", "sd", "cv", "min", "max", "median", "geom_mean", "geom_cv")

  # Calling s_summary.numeric
  results <- s_summary.numeric(x)

  lapply(results[pk_stats], with_label, row_label)
}

#' @describeIn summarize_pk Layout creating function which can be used for creating
#'   summary tables for PK data sets.
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'
#' @export
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM") %>%
#'   split_rows_by(var = "SEX") %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = TRUE)
#'   result <- build_table(lyt, df = ADSL)
#'   result
#'
#' lyt <- basic_table() %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = TRUE)
#'   result <- build_table(lyt, df = ADSL)
#'   result
#'
summarize_pk_in_cols <- function(lyt,
                                 var,
                                 ...,
                                 .stats = c("n", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
                                 .labels = c(
                                 n = "n",
                                 mean = "mean",
                                 sd = "sd",
                                 cv = "CV % Mean",
                                 geom_mean = "Geometric Mean",
                                 geom_cv = "CV % Geometric Mean",
                                 median = "Median",
                                 min = "Minimum",
                                 max = "Maximum"
                                 ),
                                 .indent_mods = NULL,
                                 col_split = TRUE) {

  afun_list <- Map(function(stat) {
    make_afun(
      s_summary_pk,
      .stats = stat,
      .formats = summary_formats()[names(summary_formats()) == stat])
    },
    stat = .stats)

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(var, length(.stats)),
      varlabels = .labels[.stats])
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun_list,
    extra_args = list(...))
}
