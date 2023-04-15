a_cox_univar <- function(df,
                         labelstr = "",
                         eff = FALSE,
                         cov_main = FALSE,
                         variables,
                         at = list(),
                         control = control_coxreg(),
                         .spl_context,
                         .stats,
                         .formats) {
  cov <- tail(.spl_context$value, 1)
  variables$covariates <- cov

  thing <- if (eff) {
    control$interaction <- FALSE
    fit_coxreg_univar(
      variables = variables[names(variables) != "covariates"],
      data = df,
      at = at,
      control = control
    )
  } else {
    if (cov_main) control$interaction <- TRUE
    fit_coxreg_univar(
      variables = variables,
      data = df,
      at = at,
      control = control
    )
  }
  thing <- thing %>% broom::tidy()

  if (!cov_main) {
    thing[, "pval_inter"] <- NA_real_
  }

  if (eff) {
    retvals <- list(unlist(thing[.stats]))
    nms <- thing$term_label
  } else {
    thing <- thing[thing$effect == "Covariate:", ]
    if (control$interaction) thing[,"pval"] <- NA_real_
    if (cov_main) {
      thing_keep <- thing[nchar(thing$level) == 0, ]
      nms <- labelstr
    } else if (control$interaction) {
      givlevs <- if (is.factor(df[[cov]])) {
        levels(df[[cov]])
      } else if (!is.null(at[[cov]])) {
        as.character(at[[cov]])
      } else {
        as.character(median(df[[cov]]))
      }
      thing_keep <- if (cov_main) thing[!thing$level %in% givlevs,] else thing[thing$level %in% givlevs,]
      nms <- givlevs
    } else {
      thing_keep <- thing
      nms <- labelstr
    }
    retvals <- as.list(apply(thing_keep[.stats], 1, function(x) unlist(x), simplify = FALSE))
  }
  in_rows(
    .list = retvals,
    .names = nms,
    .labels = nms,
    .formats = setNames(rep(.formats, length(nms)), nms),
    .format_na_strs = setNames(rep("", length(nms)), nms)
  )
}

a_cox_multivar <- function(df,
                           labelstr = "",
                           eff = FALSE,
                           var_main = FALSE,
                           variables,
                           at = list(),
                           control = control_coxreg(),
                           .spl_context,
                           .stats,
                           .formats = .formats) {
  thing <- fit_coxreg_multivar(
    variables = variables,
    data = df,
    control = control
  ) %>% broom::tidy()

  if (eff) {
    if (var_main) thing <- thing[1, ] else thing <- thing[-1, ]
    thing_keep <- thing[thing$term %in% c(variables$arm, var_labels(df)[[variables$arm]]), ]
  } else {
    cov <- tail(.spl_context$value, 1)
    if (var_main & is.numeric(df[[cov]])) {
      thing[cov, .stats] <- NA_real_
      thing_keep <- thing[thing$term == var_labels(df)[[cov]], ]
    } else if (var_main) {
      thing_keep <- thing[thing$term == cov, ]
    } else {
      thing_keep <- thing[thing$term == var_labels(df)[[cov]], ]
      if (is.numeric(df[[cov]])) thing_keep$term_label <- "All"
    }
  }
  nms <- thing_keep$term_label
  retvals <- as.list(apply(thing_keep[.stats], 1, function(x) unlist(x), simplify = FALSE))
  in_rows(
    .list = retvals,
    .names = nms,
    .labels = nms,
    .formats = setNames(rep(.formats, length(nms)), nms),
    .format_na_strs = setNames(rep("", length(nms)), nms)
  )
}

formats_coxreg <- c(
  n = "xx",
  hr = "xx.xx",
  ci = "(xx.xx, xx.xx)",
  pval = "x.xxxx | (<0.0001)",
  pval_inter = "x.xxxx | (<0.0001)"
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
                                 .indent_mods = NULL,
                                 .na_str = NA_character_,
                                 .section_div = NA_character_) {
  stat_labels <- c(
    n = "n",
    hr = "Hazard Ratio",
    ci = paste0(control$conf_level * 100, "% CI"),
    pval = "p-value",
    pval_inter = "p-value\n(Interaction)"
  )
  stat_labels <- stat_labels[names(stat_labels) %in% .stats]

  lyt <- lyt %>% split_cols_by_multivar(
    vars = rep(common_var, length(.stats)),
    varlabels = stat_labels,
    extra_args = list(.stats = .stats, .formats = .formats)
  )

  if (!multivar) {
    if (nchar(variables$arm) > 0) {
      lyt <- lyt %>%
        split_rows_by(
          common_var,
          split_label = "Treatment:",
          label_pos = "visible"
        ) %>%
        summarize_row_groups(cfun = a_cox_univar,
                             extra_args = list(variables = variables, control = control, eff = TRUE))
    }

    lyt <- lyt %>% split_rows_by_multivar(
      vars = variables$covariates,
      varlabels = var_labels,
      split_label = "Covariate:",
      nested = FALSE
    ) %>%
      summarize_row_groups(
        cfun = a_cox_univar,
        extra_args = list(variables = variables, control = control, cov_main = control$interaction)
      )

    if (control$interaction) {
      lyt <- lyt %>%
        analyze_colvars(
          afun = a_cox_univar,
          extra_args = list(variables = variables, at = at, control = control)
        )
    }
  } else {
    control$interaction <- FALSE
    if (nchar(variables$arm) > 0) {
      lyt <- lyt %>%
        split_rows_by(
          common_var,
          split_label = "Treatment:",
          label_pos = "visible"
        ) %>%
        summarize_row_groups(cfun = a_cox_multivar,
                             extra_args = list(eff = TRUE, var_main = TRUE, control = control, variables = variables)
        ) %>%
        analyze_colvars(
          afun = a_cox_multivar,
          extra_args = list(eff = TRUE, control = control, variables = variables)
        )
    }

    lyt <- lyt %>% split_rows_by_multivar(
      vars = variables$covariates,
      varlabels = var_labels,
      split_label = "Covariate:",
      nested = FALSE
    ) %>%
      summarize_row_groups(
        cfun = a_cox_multivar,
        extra_args = list(control = control, variables = variables, var_main = TRUE)
      ) %>%
      analyze_colvars(
        afun = a_cox_multivar,
        extra_args = list(control = control, variables = variables)
      )
  }

  lyt
}
