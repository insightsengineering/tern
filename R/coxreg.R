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
  if (eff || multivar) {
    control$interaction <- FALSE
  } else {
    cov <- tail(.spl_context$value, 1)
    variables$covariates <- cov
    if (var_main) control$interaction <- TRUE
  }

  if (!multivar) {
    model <- fit_coxreg_univar(
      variables = variables,
      data = df,
      at = at,
      control = control
    ) %>% broom::tidy()
    if (!var_main) model[, "pval_inter"] <- NA_real_
  } else {
    model <- fit_coxreg_multivar(
      variables = variables,
      data = df,
      control = control
    ) %>% broom::tidy()
  }

  vars_coxreg <- list(var = NULL, which_vars = "all")
  if (eff) {
    if (multivar && !var_main) {
      vars_coxreg[c("var", "which_vars")] <- list(c(variables$arm, var_labels(df)[[variables$arm]]), "multi_lvl")
    } else {
      vars_coxreg["var"] <- list(variables$arm)
    }
  } else {
    cov <- tail(.spl_context$value, 1)
    if (!multivar || (multivar && var_main && !is.numeric(df[[cov]]))) {
      vars_coxreg[c("var", "which_vars")] <- list(cov, "var_main")
    } else if (multivar) {
      vars_coxreg[c("var", "which_vars")] <- list(c(cov, var_labels(df)[[cov]]), "multi_lvl")
      if (var_main) model[cov, .stats] <- NA_real_
    }
    if (!multivar && !var_main && control$interaction) vars_coxreg["which_vars"] <- "inter"
  }

  var_vals <- s_coxreg(
    model, .stats,
    var = vars_coxreg$var, which_vars = vars_coxreg$which_vars
  )[[1]]
  var_nms <- if (!multivar && !eff && !(!var_main && control$interaction) && nchar(labelstr) > 0) {
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

summarize_coxreg_new <- function(lyt,
                                 variables,
                                 control = control_coxreg(),
                                 at = list(),
                                 multivar = FALSE,
                                 common_var = "STUDYID",
                                 .stats = c("n", "hr", "ci", "pval"),
                                 .formats = formats_coxreg[names(formats_coxreg) %in% .stats],
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
