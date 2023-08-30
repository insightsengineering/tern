#' Apply 1/3 or 1/2 Imputation Rule to Data
#'
#' @export
imputation_rule <- function(df, x_stats, stat, imp, post = FALSE) {
  n_blq <- sum(df$AVALCAT1 %in% c("BLQ", "LTR", "<PCLLOQ"))
  ltr_blq_ratio <- n_blq / max(1, nrow(df))

  # defaults
  val <- x_stats[[stat]]
  na_level = "NE"

  if (imp == "1/3") {
    if (!post && stat == "geom_mean") val <- NA # 1/3_pre_LT, 1/3_pre_GT
    if (ltr_blq_ratio > 1 / 3) {
      if (stat != "geom_mean") na_level <- "ND" # 1/3_pre_GT, 1/3_post_GT
      if (!post && !stat %in% c("median", "max")) val <- NA # 1/3_pre_GT
      if (post && !stat %in% c("median", "max", "geom_mean")) val <- NA # 1/3_post_GT
    }
  } else if (imp == "1/2") {
    if (ltr_blq_ratio > 1 / 2 && !stat == "max") {
      val <- NA # 1/2_GT
      na_level <- "ND" # 1/2_GT
    }
  }

  list(val = val, na_level = na_level)
}
