
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
#' library(random.cdisc.data)
#' adsl <- radsl(100)
#'
#' lyt <- basic_table() %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = TRUE) %>%
#'   split_rows_by(var = "SEX") %>%
#'   summarize_pk_in_cols(var = "AGE", col_split = FALSE)
#'   result <- build_table(lyt, df = adsl)
#'   result

summarize_pk_in_cols <- function(lyt,
                         var,
                         ...,
                         .stats = c("n", "mean_sd", "cv"),
                         .labels = c(n = "n", mean_sd = "mean (sd)", cv = "CV %"),
                         .indent_mods = NULL,
                         col_split = TRUE) {


  .a_summary_numeric_formats <- summary_formats()
  afun_list <- Map(function(stat) {
    make_afun(
      s_summary.numeric,
      .stats = stat,
      .formats = .a_summary_numeric_formats[names(.a_summary_numeric_formats) == stat])
  },
  stat = .stats)
  # analyze(
  #   lyt = lyt,
  #   vars = vars,
  #   var_labels = var_labels,
  #   afun = afun,
  #   nested = nested,
  #   extra_args = list(...),
  #   inclNAs = TRUE,
  #   show_labels = show_labels,
  #   table_names = table_names
  # )

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


