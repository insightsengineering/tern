#' Summary numeric variables in columns
#'
#' These functions can be used to produce summary tables for PK datasets.
#'
#' @name summarize_numeric_in_columns
#'
NULL

#' @describeIn summarize_numeric_in_columns Statistics function that produces a named list of statistics
#'   to include as columns.
#'
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
#' ADPC <- scda::synthetic_cdisc_data("latest")$adpc
#' summary_numeric_in_cols(ADPC$AGE, custom_label = "stats")
#'
summary_numeric_in_cols <- function(x,
                         labelstr = "",
                         custom_label = NULL,
                         #.stats =  c("n", "mean", "sd", "se", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
                         ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }

  # Calling s_summary.numeric
  results <- s_summary.numeric(x)

  lapply(results, with_label, row_label)
}

#' @describeIn summarize_numeric_in_columns Layout creating function which can be used for creating
#'   summary tables in columns, primarily used for PK data sets.
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'
#' @seealso summarize_vars.
#'
#' @export
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM") %>%
#'   split_rows_by(var = "SEX") %>%
#'   summarize_vars_numeric_in_cols(var = "AGE", col_split = TRUE)
#' result <- build_table(lyt = lyt, df = ADPC)
#' result
#'
#' lyt <- basic_table() %>%
#'   summarize_vars_numeric_in_cols(var = "AGE", col_split = TRUE)
#'   result <- build_table(lyt, df = ADPC)
#' result
#'
#' # PKPT03
#' ADPC$PARAMUNIT <- paste0(ADPC$PARAM, "(", ADPC$AVALU, ")")
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "PARAMUNIT") %>%
#'   summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)
#'   result <- build_table(lyt, df = ADPC)
#' result
#'
#' # PKCT01FDS
#' ADPC$NRELTM1 <- as.factor(ADPC$NRELTM1)
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "VISIT") %>%
#'   split_rows_by(var = "NRELTM1") %>%
#'   summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)
#'   result <- build_table(lyt, df = ADPC)
#' result
#'
#' # PKCT01MRD
#' ADPC$NRELTM1 <- as.factor(ADPC$NRELTM1)
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "VISIT") %>%
#'   split_rows_by(var = "NRELTM2") %>%
#'   summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)
#'   result <- build_table(lyt, df = ADPC)
#' result
#'
summarize_vars_numeric_in_cols <- function(lyt,
                                 var,
                                 ...,
                                 .stats = c("n", "mean", "sd", "se", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
                                 .labels = c(
                                 n = "n",
                                 mean = "Mean",
                                 sd = "SD",
                                 se = "SE",
                                 cv = "CV % Mean",
                                 geom_mean = "Geometric Mean",
                                 geom_cv = "CV % Geometric Mean",
                                 median = "Median",
                                 min = "Minimum",
                                 max = "Maximum"
                                 ),
                                 .indent_mods = NULL,
                                 col_split = TRUE) {


  afun_list <- Map(
    function(stat) {
      make_afun(
       summary_numeric_in_cols,
        .stats = stat,
        .formats = summary_formats()[names(summary_formats()) == stat])
    },
    stat = .stats
  )

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
