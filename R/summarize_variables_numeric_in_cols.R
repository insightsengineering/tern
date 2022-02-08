#' Summary numeric variables in columns
#'
#' These functions can be used to produce summary tables for PK datasets.
#'
#' @name summarize_numeric_in_columns
#'
NULL

#' @describeIn summarize_numeric_in_columns a wrapper of [s_summary.numeric()]
#'  function that produces a named list of statistics to include as columns.
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.numeric()].
#' See [s_summary.numeric()] to be aware of all available statistics.
#'
#' @export
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' ADPC <- scda::synthetic_cdisc_data("latest")$adpc
#' summary_numeric_in_cols(ADPC$AGE, custom_label = "stats")
summary_numeric_in_cols <- function(x,
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

  # Calling s_summary.numeric
  results <- s_summary.numeric(x)

  lapply(results, with_label, row_label)
}

#' @describeIn summarize_numeric_in_columns Layout creating function which can be used for creating
#'   summary tables in columns, primarily used for PK data sets.
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'
#' @seealso [summarize_vars].
#'
#' @export
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_numeric_in_cols(var = "AGE", col_split = TRUE)
#' result <- build_table(lyt = lyt, df = ADPC)
#' result
#'
#' # By selecting just some statistics and ad-hoc labels
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_numeric_in_cols(
#'     var = "AGE",
#'     .stats = c("n", "cv", "geom_mean", "mean_ci"),
#'     .labels = c(n = "myN", cv = "myCV", geom_mean = "myGeomMean", mean_ci = "Mean (95%CI)"),
#'     col_split = TRUE
#'   )
#' result <- build_table(lyt = lyt, df = ADPC)
#' result
#'
#' lyt <- basic_table() %>%
#'   summarize_vars_numeric_in_cols(
#'     var = "AGE",
#'     col_split = TRUE,
#'     custom_label = "some custom label"
#'   )
#' result <- build_table(lyt, df = ADPC)
#' result
#'
#' # PKPT03
#' ADPC <- mutate(ADPC, PARAMUNIT = paste0(.data$PARAM, "(", .data$AVALU, ")"))
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "PARAMUNIT", split_label = "PK Parameter", label_pos = "topleft") %>%
#'   summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)
#' result <- build_table(lyt, df = ADPC)
#' result
#'
#' # PKCT01FDS
#' ADPC <- mutate(ADPC, as.factor(.data$NRELTM1))
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "VISIT", split_label = "Visit", label_pos = "topleft") %>%
#'   split_rows_by(
#'     var = "NRELTM1",
#'     split_label = "Norminal time from first dose",
#'     label_pos = "topleft"
#'   ) %>%
#'   summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)
#' result <- build_table(lyt, df = ADPC)
#' result
summarize_vars_numeric_in_cols <- function(lyt,
                                           var,
                                           ...,
                                           .stats = c(
                                             "n",
                                             "mean",
                                             "sd",
                                             "se",
                                             "cv",
                                             "geom_cv"
                                           ),
                                           .labels = c(
                                             n = "n",
                                             mean = "Mean",
                                             sd = "SD",
                                             se = "SE",
                                             cv = "CV (%)",
                                             geom_cv = "CV % Geometric Mean"
                                           ),
                                           .indent_mods = NULL,
                                           col_split = TRUE) {
  afun_list <- Map(
    function(stat) {
      make_afun(
        summary_numeric_in_cols,
        .stats = stat,
        .formats = summary_formats()[names(summary_formats()) == stat]
      )
    },
    stat = .stats
  )

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(var, length(.stats)),
      varlabels = .labels[.stats]
    )
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun_list,
    extra_args = list(...)
  )
}
