extract_survival_biomarkers <- function(variables,
                                        data,
                                        groups_lists = list(),
                                        control = control_coxreg(),
                                        label_all = "All Patients") {
  assert_that(
    is.list(variables),
    is.character(variables$subgroups) || is.null(variables$subgroups),
    is.string(label_all)
  )
  # Start with all patients.
  result_all <- h_coxreg_mult_cont_df(
    variables = variables,
    data = data,
    control = control
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"
  if (is.null(variables$subgroups)) {
    # Only return result for all patients.
    result_all
  } else {
    # Add subgroups results.
    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)
    l_result <- lapply(l_data, function(grp) {
      result <- h_coxreg_mult_cont_df(
        variables = variables,
        data = grp$df,
        control = control
      )
      result_labels <- grp$df_labels[rep(1, times = nrow(result)), ]
      cbind(result, result_labels)
    })
    result_subgroups <- do.call(rbind, args = c(l_result, make.row.names = FALSE))
    result_subgroups$row_type <- "analysis"
    rbind(
      result_all,
      result_subgroups
    )
  }
}

h_surv_to_coxreg_variables <- function(variables, biomarker) {
  list(
    time = variables$tte,
    event = variables$is_event,
    arm = biomarker,
    covariates = variables$covariates,
    strata = variables$strata
  )
}

h_coxreg_mult_cont_df <- function(variables,
                                  data,
                                  control = control_coxreg()) {
  assert_that(
    is.list(variables),
    is_character_vector(variables$biomarkers),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_fully_named_list(control)
  )
  # If there is any data, run model, otherwise return empty results.
  result_df <- if (nrow(data) > 0) {
    bm_cols <- match(variables$biomarkers, names(data))
    data_constant <- data[, - bm_cols]
    l_result <- lapply(variables$biomarkers, function(bm) {
      data <- droplevels(na.omit(cbind(
        data_constant,
        data[bm]
      )))
      coxreg_list <- fit_coxreg_multivar(
        variables = h_surv_to_coxreg_variables(variables, bm),
        data = data,
        control = control
      )
      result <- do.call(
        h_coxreg_multivar_extract,
        c(list(var = bm), coxreg_list[c("mod", "data", "control")])
      )
      median <- s_surv_time(
        df = data,
        .var = variables$tte,
        is_event = variables$is_event
      )$median
      data.frame(
        # Dummy column needed downstream to create a nested header.
        biomarker = bm,
        biomarker_label = var_labels(data[bm], fill = TRUE),
        n_tot = nrow(data),
        n_tot_events = sum(data$is_event),
        median = as.numeric(median),
        result[1L, c("hr", "lcl", "ucl")],
        conf_level = control[["conf_level"]],
        pval = result[1L, "pval"],
        pval_label = paste0(
          "p-value (", stringr::str_to_sentence(control[["pval_method"]]), ")"
        ),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, args = c(l_result, make.row.names = FALSE))
  } else {
    data.frame(
      biomarker = variables$biomarkers,
      biomarker_label = var_labels(data[variables$biomarkers], fill = TRUE),
      n_tot = 0L,
      n_tot_events = 0L,
      hr = NA,
      lcl = NA,
      ucl = NA,
      conf_level = control[["conf_level"]],
      pval = NA,
      pval_label = paste0(
        "p-value (", stringr::str_to_sentence(control[["pval_method"]]), ")"
      ),
      stringsAsFactors = FALSE
    )
  }
  result_df
}

h_tab_surv_one_biomarker <- function(df_sub,
                                     afun_lst,
                                     colvars) {
  lyt <- basic_table()
  # Row split by row type - only keep the content rows here.
  lyt_sub <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE,
    indent_mod = 1L
  )
  # Summarize rows with all patients.
  lyt_sub <- summarize_row_groups(
    lyt = lyt_sub,
    var = "var_label",
    cfun = afun_lst
  )
  # Split cols by the multiple variables to populate into columns.
  lyt_sub <- split_cols_by_multivar(
    lyt = lyt_sub,
    vars = colvars$vars,
    varlabels = colvars$labels
  )

  # If there is any subgroup variables, we extend the layout accordingly.
  if ("analysis" %in% df$row_type) {

    # Now only continue with the subgroup rows.
    lyt_sub <- split_rows_by(
      lyt = lyt_sub,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden"
    )
    # Split by the subgroup variable.
    lyt_sub <- split_rows_by(
      lyt = lyt_sub,
      var = "var",
      labels_var = "var_label",
      nested = TRUE,
      child_labels = "visible",
      indent_mod = 1L
    )
    # Then analyze colvars for each subgroup.
    lyt_sub <- summarize_row_groups(
      lyt = lyt_sub,
      cfun = afun_lst,
      var = "subgroup"
    )
  }

  result <- build_table(lyt_sub, df = df_sub)
}

tabulate_survival_biomarkers <- function(lyt,
                                         df,
                                         vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
                                         time_unit = NULL) {

  conf_level <- df$conf_level[1]
  method <-  df$pval_label[1]
  afun_lst <- a_survival_subgroups()[vars]
  colvars <- tern:::d_survival_subgroups_colvars(
    vars,
    conf_level = conf_level,
    method = method,
    time_unit = time_unit
  )

  df_subs <- split(df, f = df$biomarker)
  tabs <- lapply(df_subs, FUN = function(df_sub) {
    tab_sub <- h_tab_surv_one_biomarker(
      df_sub = df_sub,
      afun_lst = afun_lst,
      colvars = colvars
    )
    insert_rrow(tab_sub, rrow(df_sub$biomarker_label[1]))
  })
  result <- do.call(rbind, tabs)

  n_tot_ids <- grep("^n_tot", colvars$vars)
  hr_id <- match("hr", colvars$vars)
  ci_id <- match("lcl", colvars$vars)
  structure(
    result,
    # todo: determine forest_header
    # forest_header = paste0(rev(levels(df$survtime$arm)), "\nBetter"),
    col_x = hr_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_ids[1]
  )
}
