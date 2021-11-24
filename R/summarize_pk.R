



summarize_pk <- function(lyt,
                           vars,
                           var_labels = vars,
                           nested = TRUE,
                           ...,
                           show_labels = "default",
                           table_names = vars,
                           .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {

  afun_list <- Map(function(stat) {
    make_afun(
      s_count_patients_sum_exposure,
      .stats = stat,
      .formats = ifelse(stat == "n_patients", "xx (xx.x%)", "xx")
    )
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

  lyt <- split_cols_by_multivar(lyt = lyt,
                                vars = .stats,
                                varlabels = .labels[.stats])


  summarize_row_groups(lyt = lyt,
                       var = vars,
                       cfun = afun,
                       extra_args = list(...))


}
