#' Split Function to Configure Risk Difference Column
#'
#' @export
add_risk_diff <- function(arm_x,
                          arm_y,
                          col_label = "Risk Difference (%) (95% CI)") {
  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    paste("riskdiff", arm_x, arm_y, sep = "_"), col_label, c(arm_x, arm_y), list()
  )
  add_combo_levels(combodf)
}

#' Analysis Function to Calculate Risk Difference Column Values
#'
#' @export
afun_riskdiff <- function(df,
                          .var,
                          .N_col,
                          .N_row,
                          .spl_context,
                          .all_col_counts,
                          .stats,
                          .indent_mods,
                          afun,
                          s_args = list(...)) {
  afun_args <- list(.var = .var, .N_row = .N_row, denom = "N_col")
  afun_args <- afun_args[intersect(names(afun_args), names(as.list(args(afun[[1]]))))]
  if (grepl("^riskdiff", .spl_context$cur_col_split_val)) {
    arm_x <- strsplit(.spl_context$cur_col_split_val[[1]], "_")[[1]][2]
    arm_y <- strsplit(.spl_context$cur_col_split_val[[1]], "_")[[1]][3]
    N_col_x <- .all_col_counts[[arm_x]]
    N_col_y <- .all_col_counts[[arm_y]]
    cur_var <- .spl_context$cur_col_split[[1]]
    s_x <- do.call(
      gsub("afun", "s", names(afun)),
      args = c(list(df = df[df[[cur_var]] == arm_x, ], .N_col = N_col_x), afun_args, s_args)
    )
    s_y <- do.call(
      gsub("afun", "s", names(afun)),
      args = c(list(df = df[df[[cur_var]] == arm_y, ], .N_col = N_col_y), afun_args, s_args)
    )
    stat <- ifelse("count_fraction" %in% names(s_x), "count_fraction", "unique")
    var_nms <- if ("flag_variables" %in% names(s_args)) s_args$flag_variables else names(s_x[[stat]])
    rd_ci <- rep(stat_propdiff_ci(
      lapply(s_x[[stat]], `[`, 2), lapply(s_y[[stat]], `[`, 2),
      N_col_x, N_col_y, list_names = var_nms
    ), max(1, length(.stats)))
    in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
  } else {
    do.call(afun[[1]], args = c(list(df = df, .N_col = .N_col), afun_args, s_args))
  }
}
