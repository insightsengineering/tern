#' Odds Ratio Estimation
#'
#' Compares bivariate responses between two groups in terms of odds ratios
#' along with a confidence interval.
#'
#' @details This function uses either logistic regression for unstratified
#'   analyses, or conditional logistic regression for stratified analyses.
#'   The Wald confidence interval with the specified confidence level is
#'   calculated. Note that, for stratified analyses, there is currently no
#'   implementation for conditional likelihood confidence intervals,
#'   therefore the likelihood confidence interval as an option is not yet
#'   available. Besides, when `rsp` contains only responders or non-responders,
#'   then the result values will be `NA`, because no odds ratio estimation is
#'   possible.
#'
#' @name odds_ratio
#' @md
#'
NULL


#' @describeIn odds_ratio estimates the odds ratio based on [stats::glm()].
#'
#' @inheritParams argument_convention
#' @param data (`data frame`)\cr
#'   expected with three variables `rsp`, `grp`.
#' @importFrom stats binomial as.formula glm coef confint setNames
#' @export
#' @examples
#'
#' # Data
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2)]
#' )
#'
#' # Odds ratio based on glm.
#' or_glm(data, conf_level = 0.95)
#'
or_glm <- function(data, conf_level) {

  assert_that(
    is_df_with_variables(data, list(rsp = "rsp", grp = "grp")),
    is.logical(data$rsp),
    is_character_or_factor(data$grp),
    is_proportion(conf_level)
  )

  data$grp <- as_factor_keep_attributes(data$grp)
  formula <- stats::as.formula("rsp ~ grp")
  model_fit <- stats::glm(
    formula = formula, data = data,
    family = stats::binomial(link = "logit")
  )

  # Note that here we need to discard the intercept.
  or <- exp(stats::coef(model_fit)[-1])
  or_ci <- exp(
    stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE]
  )

  values <- stats::setNames(c(or, or_ci), c("est", "lcl", "ucl"))

  list(or_ci = values)

}


#' @describeIn odds_ratio Statistics function which estimates the odds ratio
#'   between a treatment and a control.
#' @export
#' @examples
#'
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50))
#' )
#'
#' s_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE
#' )
#'
s_odds_ratio <- function(df,
                         .var,
                         .ref_group,
                         .in_ref_col,
                         conf_level = 0.95) {
  y <- list(or_ci = "")

  if (!.in_ref_col) {
    assert_that(
      is_df_with_variables(df, list(rsp = .var)),
      is_df_with_variables(.ref_group, list(rsp = .var)),
      is_proportion(conf_level)
    )

    data <- data.frame(
      rsp = c(.ref_group[[.var]], df[[.var]]),
      grp = factor(
        rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))),
        levels = c("ref", "Not-ref")
      )
    )

    y <- or_glm(data, conf_level = conf_level)
  }

  y$or_ci <- with_label(
    x = y$or_ci,
    label = paste0("Odds Ratio (", 100 * conf_level, "% CI)")
  )
  y
}

#' @describeIn odds_ratio Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE
#' )
#'
a_odds_ratio <- make_afun(
  s_odds_ratio,
  .formats = c(or_ci = "xx.xx (xx.xx - xx.xx)"),
  .indent_mods = c(or_ci = 1L)
)

#' @describeIn odds_ratio Layout creating function which can be used for creating
#'   tables, which can take statistics function arguments and additional format
#'   arguments (see below).
#'
#' @inheritParams argument_convention
#' @param ... arguments passed to `s_odds_ratio()`.
#' @export
#' @examples
#'
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50))
#' )
#'
#' l <- split_cols_by(lyt = NULL, var = "grp", ref_group = "B") %>%
#'   estimate_odds_ratio(vars = "rsp")
#'
#' build_table(l, df = dta)
#'
estimate_odds_ratio <- function(lyt,
                                vars,
                                ...,
                                show_labels = "hidden",
                                .stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  afun <- make_afun(
    a_odds_ratio,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...),
    show_labels = show_labels
  )
}
