#' Cox Proportional Hazards Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Fits a Cox regression model and estimate hazard ratio to describe the effect
#' size in a survival analysis.
#'
#' @details
#' Cox models are the most commonly used methods to estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the
#' most commonly reported metrics to describe the effect size in survival
#' analysis (NEST Team, 2020).
#'
#' @note The usual formatting arguments for the _layout creating_ function
#'  `summarize_coxreg` are not yet accepted (`.stats`, `.indent_mod`, `.formats`,
#'  `.labels`).
#' @inheritParams argument_convention
#' @seealso [fit_coxreg] for relevant fitting functions, [h_cox_regression] for relevant
#' helper functions, and [tidy_coxreg] for custom tidy methods.
#'
#' @name cox_regression
#'
#' @examples
#' library(survival)
#'
#' # Testing dataset [survival::bladder].
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     TIME = stop,
#'     STATUS = event,
#'     ARM = as.factor(rx),
#'     COVAR1 = as.factor(enum),
#'     COVAR2 = factor(
#'       sample(as.factor(enum)),
#'       levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' dta_bladder$AGE <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#' dta_bladder$STUDYID <- factor("X")
#'
#' plot(
#'   survfit(Surv(TIME, STATUS) ~ ARM + COVAR1, data = dta_bladder),
#'   lty = 2:4,
#'   xlab = "Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
NULL

#' @describeIn cox_regression transforms the tabulated results from [`fit_coxreg_univar()`]
#'  and [`fit_coxreg_multivar()`] into a list. Not much calculation is done here,
#'  it rather prepares the data to be used by the layout creating function.
#'
#' @param .stats (`character`)\cr the name of statistics to be reported among:
#'   * `n`: number of observations
#'   * `hr`: hazard ratio
#'   * `ci`: confidence interval
#'   * `pval`: p-value of the treatment effect
#'   * `pval_inter`: p-value of the interaction effect between the treatment and the covariate
#' @param which_vars (`character`)\cr which rows should statistics be returned for from the given model.
#'   Defaults to "all". Other options include "var_main" for main effects, "inter" for interaction effects,
#'   and "multi_lvl" for multivariate model covariate level rows. When `which_vars` is "all" specific
#'   variables can be selected by specifying `var_nms`.
#' @param var_nms (`character`)\cr the `term` value of rows in `df` for which `.stats` should by returned. Typically
#'   this is the name of a variable. If using variable labels, `var` should be a vector of both the desired
#'   variable name and the variable label in that order to see all `.stats` related to that variable. When `which_vars`
#'   is "var_main" `var_nms` should be only the variable name.
#'
#' @export
#'
#' @examples
#' # s_coxreg
#'
#' # Univariate
#' u1_variables <- list(time = "TIME", event = "STATUS", arm = "ARM", covariates = c("COVAR1", "COVAR2"))
#' univar_model <- fit_coxreg_univar(variables = u1_variables, data = dta_bladder)
#' df1 <- broom::tidy(univar_model)
#' s_coxreg(df = df1, .stats = "hr")
#'
#' # Univariate without treatment arm - only "COVAR2" covariate effects
#' u2_variables <- list(time = "TIME", event = "STATUS", covariates = c("COVAR1", "COVAR2"))
#' univar_covs_model <- fit_coxreg_univar(variables = u2_variables, data = dta_bladder)
#' df1_covs <- broom::tidy(univar_covs_model)
#' s_coxreg(df = df1_covs, .stats = "hr", var_nms = c("COVAR2", "Sex (F/M)"))
#'
#' # Multivariate.
#' m1_variables <- list(time = "TIME", event = "STATUS", arm = "ARM", covariates = c("COVAR1", "COVAR2"))
#' multivar_model <- fit_coxreg_multivar(variables = m1_variables, data = dta_bladder)
#' df2 <- broom::tidy(multivar_model)
#' s_coxreg(df = df2, .stats = "hr")
#'
#' # Multivariate without treatment arm - only "COVAR1" main effect
#' m2_variables <- list(time = "TIME", event = "STATUS", covariates = c("COVAR1", "COVAR2"))
#' multivar_covs_model <- fit_coxreg_multivar(variables = m2_variables, data = dta_bladder)
#' df2_covs <- broom::tidy(multivar_covs_model)
#' s_coxreg(df = df2_covs, .stats = "pval", which_vars = "var_main", var_nms = "COVAR1")
#'
s_coxreg <- function(df, .stats, which_vars = "all", var_nms = NULL) {
  assert_df_with_variables(df, list(term = "term", stat = .stats))
  checkmate::assert_multi_class(df$term, classes = c("factor", "character"))
  df$term <- as.character(df$term)
  var_nms <- var_nms[!is.na(var_nms)]

  if (length(var_nms) > 0) df <- df[df$term %in% var_nms, ]
  if (which_vars == "multi_lvl") df$term <- tail(var_nms, 1)

  # We need a list with names corresponding to the stats to display of equal length to the list of stats.
  y <- split(df, f = df$term, drop = FALSE)
  y <- stats::setNames(y, nm = rep(.stats, length(y)))

  if (which_vars == "var_main") {
    y <- lapply(y, function(x) x[1, ])
  } else if (which_vars %in% c("inter", "multi_lvl")) {
    y <- lapply(y, function(x) if (nrow(y[[1]]) > 1) x[-1, ] else x)
  }

  lapply(
    X = y,
    FUN = function(x) {
      z <- as.list(x[[.stats]])
      stats::setNames(z, nm = x$term_label)
    }
  )
}

a_coxreg <- function(df,
                     labelstr = "",
                     eff = FALSE,
                     var_main = FALSE,
                     multivar = FALSE,
                     variables,
                     at = list(),
                     control = control_coxreg(),
                     .spl_context,
                     .stats,
                     .formats) {
  cov_no_arm <- !multivar && !"arm" %in% names(variables) && control$interaction
  if (eff || multivar || cov_no_arm) {
    control$interaction <- FALSE
  } else {
    cov <- tail(.spl_context$value, 1)
    variables$covariates <- cov
    if (var_main) control$interaction <- TRUE
  }

  if (!multivar) {
    model <- fit_coxreg_univar(variables = variables, data = df, at = at, control = control) %>% broom::tidy()
    if (!var_main) model[, "pval_inter"] <- NA_real_
  } else {
    model <- fit_coxreg_multivar(variables = variables, data = df, control = control) %>% broom::tidy()
  }

  vars_coxreg <- list(var = NULL, which_vars = "all")
  if (eff) {
    if (multivar && !var_main) {
      vars_coxreg[c("var", "which_vars")] <- list(c(variables$arm, var_labels(df)[[variables$arm]]), "multi_lvl")
    } else {
      vars_coxreg["var"] <- variables$arm
      if (var_main) vars_coxreg["which_vars"] <- "var_main"
    }
  } else {
    cov <- tail(.spl_context$value, 1)
    if (!multivar || (multivar && var_main && !is.numeric(df[[cov]]))) {
      vars_coxreg[c("var", "which_vars")] <- list(cov, "var_main")
    } else if (multivar) {
      vars_coxreg[c("var", "which_vars")] <- list(c(cov, var_labels(df)[[cov]]), "multi_lvl")
      if (var_main) model[cov, .stats] <- NA_real_
    }
    if ((!multivar && !var_main && control$interaction) || cov_no_arm) vars_coxreg["which_vars"] <- "inter"
  }

  var_vals <- s_coxreg(
    model, .stats,
    var = vars_coxreg$var, which_vars = vars_coxreg$which_vars
  )[[1]]
  var_nms <- if (all(grepl("\\(reference = ", names(var_vals))) && labelstr != tail(.spl_context$value, 1)) {
    paste(c(labelstr, tail(strsplit(names(var_vals), " ")[[1]], 3)), collapse = " ")
  } else if (!multivar && !eff && !(!var_main && control$interaction) && nchar(labelstr) > 0) {
    labelstr
  } else if (multivar && !eff && !var_main && is.numeric(df[[cov]])) {
    "All"
  } else {
    names(var_vals)
  }

  in_rows(
    .list = var_vals, .names = var_nms, .labels = var_nms,
    .formats = setNames(rep(.formats, length(var_nms)), var_nms),
    .format_na_strs = setNames(rep("", length(var_nms)), var_nms)
  )
}

formats_coxreg <- c(
  n = "xx", hr = "xx.xx", ci = "(xx.xx, xx.xx)", pval = "x.xxxx | (<0.0001)", pval_inter = "x.xxxx | (<0.0001)"
)

#' @describeIn cox_regression layout creating function.
#'
#' @inheritParams argument_convention
#' @inheritParams fit_coxreg_univar
#'
#' @param multivar (`flag`)\cr Defaults to `FALSE`. If `TRUE` multivariate Cox regression will run, otherwise
#'   univariate Cox regression will run.
#' @param common_var (`character`)\cr the name of a factor variable in the dataset which takes the same value
#'   for all rows. This should be created during pre-processing if no such variable currently exists.
#' @param split_fun (`function`)\cr split function to implement for row split by covariates. Defaults to `NULL`.
#'   See [rtables::split_funcs] for several pre-made options.
#' @export
#'
#' @examples
#' # summarize_coxreg
#'
#' result_univar <- basic_table() %>%
#'   summarize_coxreg(
#'     variables = u1_variables,
#'     var_labels = c("A Covariate Label", "Sex (F/M)")
#'   ) %>%
#'   build_table(dta_bladder)
#' result_univar
#'
#' result_multivar <- basic_table() %>%
#'   summarize_coxreg(
#'     variables = m1_variables,
#'     multivar = TRUE,
#'     var_labels = c("A Covariate Label", "Sex (F/M)")
#'   ) %>%
#'   build_table(dta_bladder)
#' result_multivar
#'
#' result_univar_covs <- basic_table() %>%
#'   summarize_coxreg(
#'     variables = u2_variables,
#'     var_labels = c("A Covariate Label", "Sex (F/M)")
#'   ) %>%
#'   build_table(dta_bladder)
#' result_univar_covs
#'
#' result_multivar_covs <- basic_table() %>%
#'   summarize_coxreg(
#'     variables = m2_variables,
#'     multivar = TRUE,
#'     var_labels = c("A Covariate Label", "Sex (F/M)")
#'   ) %>%
#'   build_table(dta_bladder)
#' result_multivar_covs
#'
summarize_coxreg <- function(lyt,
                             variables,
                             control = control_coxreg(),
                             at = list(),
                             multivar = FALSE,
                             common_var = "STUDYID",
                             .stats = NULL,
                             .formats = NULL,
                             var_labels = NULL,
                             split_fun = NULL,
                             .indent_mods = NULL,
                             .na_str = NA_character_,
                             .section_div = NA_character_) {
  if (multivar && control$interaction) {
    stop(paste(
      "Interactions are not available for multivariate cox regression using summarize_coxreg.",
      "Please turn off interactions or switch to a univariate model."
    ))
  }
  if (control$interaction && !"arm" %in% names(variables)) {
    stop("To include interactions please specify an arm variable in variables.")
  }

  if (is.null(.stats)) .stats <- if (!multivar) c("n", "hr", "ci", "pval") else c("hr", "ci", "pval")
  .stats <- if (!"arm" %in% names(variables) || multivar) {
    intersect(c("hr", "ci", "pval"), .stats)
  } else {
    intersect(c("n", "hr", "ci", "pval", "pval_inter"), .stats)
  }
  if (is.null(.formats)) .formats <- formats_coxreg[names(formats_coxreg) %in% .stats]
  stat_labels <- c(
    n = "n",
    hr = "Hazard Ratio",
    ci = paste0(control$conf_level * 100, "% CI"),
    pval = "p-value",
    pval_inter = "p-value\n(Interaction)"
  )
  stat_labels <- stat_labels[names(stat_labels) %in% .stats]

  lyt <- lyt %>%
    split_cols_by_multivar(
      vars = rep(common_var, length(.stats)),
      varlabels = stat_labels,
      extra_args = list(.stats = .stats, .formats = .formats)
    )

  if ("arm" %in% names(variables)) {
    lyt <- lyt %>%
      split_rows_by(
        common_var,
        split_label = "Treatment:",
        label_pos = "visible"
      ) %>%
      summarize_row_groups(
        cfun = a_coxreg,
        extra_args = list(
          variables = variables, control = control, multivar = multivar, eff = TRUE, var_main = multivar
        )
      )
    if (multivar) {
      lyt <- lyt %>%
        analyze_colvars(
          afun = a_coxreg,
          extra_args = list(eff = TRUE, control = control, variables = variables, multivar = multivar)
        )
    }
  }

  if ("covariates" %in% names(variables)) {
    lyt <- lyt %>%
      split_rows_by_multivar(
        vars = variables$covariates,
        varlabels = var_labels,
        split_fun = split_fun,
        split_label = "Covariate:",
        nested = FALSE
      ) %>%
      summarize_row_groups(
        cfun = a_coxreg,
        extra_args = list(
          variables = variables, control = control, multivar = multivar,
          var_main = if (multivar) multivar else control$interaction
        )
      )
    if (!"arm" %in% names(variables)) control$interaction <- TRUE
    if (multivar || control$interaction) {
      lyt <- lyt %>%
        analyze_colvars(
          afun = a_coxreg,
          extra_args = list(variables = variables, at = at, control = control, multivar = multivar)
        )
    }
  }

  lyt
}
