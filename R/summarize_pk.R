#' Title
#'
#' @param labelstr
#' @param ...
#'
#' @return
#' @export
#'
s_summary_pk <- function(x,
                         labelstr = "",
                         custom_label = "Statistics",
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

  results$n <-  with_label(results$n, row_label)
  results$mean <- with_label(results$mean, row_label)
  results$sd <-  with_label(results$sd, row_label)
  results$cv <- with_label(results$cv, row_label)
  results$min <- with_label(results$min, row_label)
  results$max <- with_label(results$max, row_label)
  results$median <- with_label(results$median, row_label)
  results$geom_mean <- with_label(results$geom_mean, row_label)
  results$geom_cv <- with_label(results$geom_cv, row_label)

  results

}

#' Title
#'
#' @param lyt
#' @param var
#' @param ...
#' @param .stats
#' @param .labels
#' @param .indent_mods
#' @param col_split
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(scda)
#' adsl <- scda::synthetic_cdisc_data("latest")$adsl
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM") %>%
#'   split_rows_by(var = "SEX") %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = TRUE)
#'   result <- build_table(lyt, df = adsl)
#'   result
#'
#' lyt <- basic_table() %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = TRUE)
#'   result <- build_table(lyt, df = adsl)
#'   result

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
  lyt <- split_cols_by_multivar(lyt = lyt,
                                vars = rep(var, length(.stats)),
                                varlabels = .labels[.stats])

  }
  summarize_row_groups(lyt = lyt,
                       var = var,
                       cfun = afun_list,
                       extra_args = list(...))


}


