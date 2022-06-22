#' Odds Ratio Estimation
#'
#' @description `r lifecycle::badge("stable")`
#' Compares bivariate responses between two groups in terms of odds ratios
#' along with a confidence interval.
#'
#' @param data (`data frame`)\cr
#'   with at least the variables `rsp`, `grp`, and in addition `strata` for [or_clogit()].
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

#' @describeIn odds_ratio estimates the odds ratio based on [stats::glm()]. Note that there must be
#'   exactly 2 groups in `data` as specified by the `grp` variable.
#'
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' # Data with 2 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
#'   strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on glm.
#' or_glm(data, conf_level = 0.95)
or_glm <- function(data, conf_level) {
  assertthat::assert_that(
    is_df_with_variables(data, list(rsp = "rsp", grp = "grp")),
    is.logical(data$rsp),
    is_proportion(conf_level)
  )
  assert_character_or_factor(data$grp)

  data$grp <- as_factor_keep_attributes(data$grp)
  assertthat::assert_that(
    is_df_with_nlevels_factor(data, variable = "grp", n_levels = 2)
  )
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
  n_tot <- stats::setNames(nrow(model_fit$model), "n_tot")

  list(or_ci = values, n_tot = n_tot)
}

#' @describeIn odds_ratio estimates the odds ratio based on [survival::clogit()]. This is done for
#'   the whole data set including all groups, since the results are not the same as when doing
#'   pairwise comparisons between the groups.
#'
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' # Data with 3 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
#'   strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on stratified estimation by conditional logistic regression.
#' or_clogit(data, conf_level = 0.95)
or_clogit <- function(data, conf_level) {
  assertthat::assert_that(
    is_df_with_variables(data, list(rsp = "rsp", grp = "grp", strata = "strata")),
    is.logical(data$rsp),
    is_proportion(conf_level)
  )
  assert_character_or_factor(data$grp)
  assert_character_or_factor(data$strata)

  data$grp <- as_factor_keep_attributes(data$grp)
  data$strata <- as_factor_keep_attributes(data$strata)

  # Deviation from convention: `survival::strata` must be simply `strata`.
  formula <- stats::as.formula("rsp ~ grp + strata(strata)")
  model_fit <- clogit_with_tryCatch(formula = formula, data = data)

  # Create a list with one set of OR estimates and CI per coefficient, i.e.
  # comparison of one group vs. the reference group.
  coef_est <- stats::coef(model_fit)
  ci_est <- stats::confint(model_fit, level = conf_level)
  or_ci <- list()
  for (coef_name in names(coef_est)) {
    grp_name <- gsub("^grp", "", x = coef_name)
    or_ci[[grp_name]] <- stats::setNames(
      object = exp(c(coef_est[coef_name], ci_est[coef_name, , drop = TRUE])),
      nm = c("est", "lcl", "ucl")
    )
  }
  list(or_ci = or_ci, n_tot = c(n_tot = model_fit$n))
}

#' @describeIn odds_ratio Statistics function which estimates the odds ratio
#'   between a treatment and a control. Note that a `variables` list with `arm` and `strata` names
#'   needs to be passed if a stratified analysis is required.
#' @inheritParams split_cols_by_groups
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' set.seed(12)
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50), levels = c("B", "A")),
#'   strata = factor(sample(c("C", "D"), 100, TRUE))
#' )
#'
#' # Unstratified analysis.
#' s_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta
#' )
#'
#' # Stratified analysis.
#' s_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta,
#'   variables = list(arm = "grp", strata = "strata")
#' )
s_odds_ratio <- function(df,
                         .var,
                         .ref_group,
                         .in_ref_col,
                         .df_row,
                         variables = list(arm = NULL, strata = NULL),
                         conf_level = 0.95,
                         groups_list = NULL) {
  y <- list(or_ci = "", n_tot = "")

  if (!.in_ref_col) {
    assertthat::assert_that(
      is_df_with_variables(df, list(rsp = .var)),
      is_df_with_variables(.ref_group, list(rsp = .var)),
      is_proportion(conf_level)
    )

    if (is.null(variables$strata)) {
      data <- data.frame(
        rsp = c(.ref_group[[.var]], df[[.var]]),
        grp = factor(
          rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))),
          levels = c("ref", "Not-ref")
        )
      )
      y <- or_glm(data, conf_level = conf_level)
    } else {
      assertthat::assert_that(
        is_df_with_variables(.df_row, c(list(rsp = .var), variables))
      )

      # The group variable prepared for clogit must be synchronised with combination groups definition.
      if (is.null(groups_list)) {
        ref_grp <- as.character(unique(.ref_group[[variables$arm]]))
        trt_grp <- as.character(unique(df[[variables$arm]]))
        grp <- stats::relevel(factor(.df_row[[variables$arm]]), ref = ref_grp)
      } else {
        # If more than one level in reference col.
        reference <- as.character(unique(.ref_group[[variables$arm]]))
        grp_ref_flag <- vapply(
          X = groups_list,
          FUN.VALUE = TRUE,
          FUN = function(x) all(reference %in% x)
        )
        ref_grp <- names(groups_list)[grp_ref_flag]

        # If more than one level in treatment col.
        treatment <- as.character(unique(df[[variables$arm]]))
        grp_trt_flag <- vapply(
          X = groups_list,
          FUN.VALUE = TRUE,
          FUN = function(x) all(treatment %in% x)
        )
        trt_grp <- names(groups_list)[grp_trt_flag]

        grp <- combine_levels(.df_row[[variables$arm]], levels = reference, new_level = ref_grp)
        grp <- combine_levels(grp, levels = treatment, new_level = trt_grp)
      }

      # The reference level in `grp` must be the same as in the `rtables` column split.
      data <- data.frame(
        rsp = .df_row[[.var]],
        grp = grp,
        strata = interaction(.df_row[variables$strata])
      )
      y_all <- or_clogit(data, conf_level = conf_level)
      assertthat::assert_that(
        assertthat::is.string(trt_grp),
        trt_grp %in% names(y_all$or_ci)
      )
      y$or_ci <- y_all$or_ci[[trt_grp]]
      y$n_tot <- y_all$n_tot
    }
  }

  y$or_ci <- formatters::with_label(
    x = y$or_ci,
    label = paste0("Odds Ratio (", 100 * conf_level, "% CI)")
  )

  y$n_tot <- formatters::with_label(
    x = y$n_tot,
    label = "Total n"
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
#'   .in_ref_col = FALSE,
#'   .df_row = dta
#' )
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
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   estimate_odds_ratio(vars = "rsp")
#'
#' build_table(l, df = dta)
estimate_odds_ratio <- function(lyt,
                                vars,
                                ...,
                                show_labels = "hidden",
                                table_names = vars,
                                .stats = "or_ci",
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
    show_labels = show_labels,
    table_names = table_names
  )
}
