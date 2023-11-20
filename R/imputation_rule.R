#' Apply 1/3 or 1/2 Imputation Rule to Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#' @param x_stats (`named list`)\cr a named list of statistics, typically the results of [s_summary()].
#' @param stat (`character`)\cr statistic to return the value/NA level of according to the imputation
#'   rule applied.
#' @param imp_rule (`character`)\cr imputation rule setting. Set to `"1/3"` to implement 1/3 imputation
#'   rule or `"1/2"` to implement 1/2 imputation rule.
#' @param post (`flag`)\cr whether the data corresponds to a post-dose time-point (defaults to `FALSE`).
#'   This parameter is only used when `imp_rule` is set to `"1/3"`.
#' @param avalcat_var (`character`)\cr name of variable that indicates whether a row in `df` corresponds
#'   to an analysis value in category `"BLQ"`, `"LTR"`, `"<PCLLOQ"`, or none of the above
#'   (defaults to `"AVALCAT1"`). Variable `avalcat_var` must be present in `df`.
#'
#' @return A `list` containing statistic value (`val`) and NA level (`na_str`) that should be displayed
#'   according to the specified imputation rule.
#'
#' @seealso [analyze_vars_in_cols()] where this function can be implemented by setting the `imp_rule`
#'   argument.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   AVAL = runif(50, 0, 1),
#'   AVALCAT1 = sample(c(1, "BLQ"), 50, replace = TRUE)
#' )
#' x_stats <- s_summary(df$AVAL)
#' imputation_rule(df, x_stats, "max", "1/3")
#' imputation_rule(df, x_stats, "geom_mean", "1/3")
#' imputation_rule(df, x_stats, "mean", "1/2")
#'
#' @export
imputation_rule <- function(df, x_stats, stat, imp_rule, post = FALSE, avalcat_var = "AVALCAT1") {
  checkmate::assert_choice(avalcat_var, names(df))
  checkmate::assert_choice(imp_rule, c("1/3", "1/2"))
  n_blq <- sum(grepl("BLQ|LTR|<[1-9]|<PCLLOQ", df[[avalcat_var]]))
  ltr_blq_ratio <- n_blq / max(1, nrow(df))

  # defaults
  val <- x_stats[[stat]]
  na_str <- "NE"

  if (imp_rule == "1/3") {
    if (!post && stat == "geom_mean") val <- NA # 1/3_pre_LT, 1/3_pre_GT
    if (ltr_blq_ratio > 1 / 3) {
      if (stat != "geom_mean") na_str <- "ND" # 1/3_pre_GT, 1/3_post_GT
      if (!post && !stat %in% c("median", "max")) val <- NA # 1/3_pre_GT
      if (post && !stat %in% c("median", "max", "geom_mean")) val <- NA # 1/3_post_GT
    }
  } else if (imp_rule == "1/2") {
    if (ltr_blq_ratio > 1 / 2 && !stat == "max") {
      val <- NA # 1/2_GT
      na_str <- "ND" # 1/2_GT
    }
  }

  list(val = val, na_str = na_str)
}
