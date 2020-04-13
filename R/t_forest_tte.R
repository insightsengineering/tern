#' @include utils.R
NULL

#' Time-to-event Table as used for Forest Plot
#'
#' The time-to-event forest plot table summarizes time-to-event data by groups. The function returns event counts and
#' median survival time for each analysis arm, as well as a hazard ratio and the corresponding confidence interval
#' from a Cox proportional hazard model.
#'
#' @inheritParams argument_convention
#' @inheritParams t_forest_rsp
#' @inheritParams t_el_forest_tte
#'
#' @details
#' Cox proportional hazard model is used for hazard ratio calculation
#'
#' The returned table contains one row per analysis within a subgroup of data
#' (indicated by the row name). The analysis is summarized with the following 9
#' columns:
#'
#' \describe{
#'   \item{1}{\emph{Total n} the total number of subjects included in analysis population}
#'   \item{2-4}{Survival statistics for reference arm, \emph{n} and \emph{Events} are the total number
#' of patients and the number of events in reference arm, respectively.
#'   \code{Median (month)} is the survival time estimated by Kaplan-Meier method. Time
#'  unit can be modified per study needs.}
#'   \item{5-7}{same analysis as for reference arm now for comparison arm}
#'   \item{8}{\emph{Hazard Ratio} ranges from 0 to infinity. The hazard ratio is an estimate of
#'  the ratio of the hazard rate in the comparison group versus that in the reference group.
#'    Univariate Cox proportional hazard model is applied to obtain the estimated hazard ratio.
#'   Hazard ratio > 1 implies better treatment effect in reference arm, and hazard ratio < 1 when
#'   comparison arm is better. }
#'   \item{9}{\emph{Wald CI} The confidence interval indicates the level of uncertainty
#'   around the measure of effect (Hazard Ratio). Because only a small sample of the overall
#'   population is included in the analysis, by having an upper and lower confidence limit
#'   we can infer that the true treatment effect lies in between. By default, the 95% confidence
#'   interval is calculated. If it includes 1, then we say that the difference between two arms
#'   is not significant at a significance level of 0.05.}
#' }
#'
#' @export
#'
#' @template author_song24
#'
#' @seealso \code{\link{t_el_forest_tte}}, \code{\link{t_tte}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- ADTTE %>%
#'   dplyr::filter(PARAMCD == "OS", ARMCD %in% c("ARM B", "ARM A")) %>%
#'   mutate(ARMCD = droplevels(ARMCD))
#'
#' tbl <- t_forest_tte(
#'   tte = ADTTE_f$AVAL,
#'   is_event = ADTTE_f$CNSR == 0,
#'   col_by = ADTTE_f$ARMCD,
#'   row_by_list = droplevels(ADTTE_f[, c("SEX", "RACE")]),
#'   ties = "exact",
#'   conf_int = 0.9,
#'   dense_header = TRUE
#' )
#'
#' tbl2 <- t_forest_tte(
#'   tte = ADTTE_f$AVAL,
#'   is_event = ADTTE_f$CNSR == 0,
#'   col_by = ADTTE_f$ARMCD,
#'   row_by_list = droplevels(ADTTE_f[,  "BMRKR2" ]),
#'   strata_data = ADTTE_f[ , c("STRATA1", "STRATA2")],
#'   ties = "exact",
#'   dense_header = TRUE
#' )
#'
#' tbl2
#'
#' \dontrun{
#' Viewer(tbl)
#' }
#'
#' # table tree
#' tbls <- t_forest_tte(
#'   tte = ADTTE_f$AVAL,
#'   is_event = ADTTE_f$CNSR == 0,
#'   col_by = ADTTE_f$ARMCD,
#'   row_by_list = droplevels(ADTTE_f[, c("SEX", "RACE")]),
#'   ties = "exact",
#'   dense_header = TRUE,
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' to_rtable(tbls)
#'
t_forest_tte <- function(tte,
                         is_event,
                         col_by,
                         row_by_list = NULL,
                         total = "ALL",
                         strata_data = NULL,
                         ties = "exact",
                         time_unit = "month",
                         conf_int = 0.95,
                         dense_header = FALSE,
                         table_tree = FALSE) {

  stopifnot(
    is.numeric(tte),
    is.logical(is_event),
    is.null(total) || is_character_single(total),
    is.null(row_by_list) || is.list(row_by_list),
    is_numeric_single(conf_int) && (0 < conf_int) && (conf_int < 1)
  )
  check_strata(strata_data)

  do.call(
    check_same_n,
    c(list(
      tte = tte,
      is_event = is_event,
      col_by = col_by,
      strata_data = strata_data), row_by_list
    )
  )

  row_by_list <- row_by_list %>% map(explicit_na)

  # take label if it exists, otherwise rowname
  # equivalent of var_labels(as.data.frame(by), fill = TRUE) for non data.frames
  names(row_by_list) <- Map(`%||%`, lapply(row_by_list, label), names(row_by_list))

  df <- if (is.null(strata_data)) {
    data.frame(tte = tte, is_event = is_event, col_by = col_by)
  } else {
    data.frame(tte = tte, is_event = is_event, col_by = col_by, strata_data = strata_data)
  }

  dfs <- lapply(row_by_list, function(rows_by) esplit(df, rows_by))

  data_tree <- nested_list_to_tree(dfs, format_data = node_format_data(children_gap =  0), max_depth = 2)

  if (!is.null(total)) {
    data_tree@children <- c(list(node(total, df)), data_tree@children)
  }

  tree <- rapply_tree(data_tree, function(name, content, ...) {
    if (is.data.frame(content)) {
      list(
        name = invisible_node_name(name),
        content = t_el_forest_tte(
          tte = content$tte,
          is_event = content$is_event,
          col_by = content$col_by,
          strata_data = if (is.null(strata_data)) NULL else subset(content, select = -c(tte, is_event, col_by)),
          ties = ties,
          row_name = name,
          conf_int = conf_int,
          dense_header = dense_header
        )
      )
    } else {
      list(name = name, content = NULL)
    }
  })

  tree@format_data <- node_format_data(children_gap = 1)
  model_type <- if (is.null(strata_data)) {
    "Unstratified Analysis"
  } else {
    n_strata <- length(strata_data)
    paste(
      "* Stratified by",
      ifelse(
        n_strata < 2,
        names(strata_data),
        paste(paste(names(strata_data)[-n_strata], collapse = ", "), "and", names(strata_data)[(n_strata)])
      )
    )
  }

  if (table_tree) {
    tree
  } else {
    rtbl <- to_rtable(tree)
    footnotes(rtbl) <- model_type
    rtbl
  }

}


#' Elementary Table for Forest Time to Event Plot
#'
#'
#' @inheritParams survival::coxph
#' @inheritParams argument_convention
#' @param strata_data (\code{character}, \code{factor} or \code{data.frame})\cr
#'   Used for \code{\link[survival:strata]{stratification factors}}
#'   (categorical variables). If \code{NULL}, no stratified analysis is performed.
#' @param ties (\code{character} value) the method used for tie handling in \code{\link[survival]{coxph}}.
#' @param time_unit (\code{character} value) The unit of median survival time. Default is \code{"months"}.
#' @param row_name (\code{character} value) name of row
#' @param conf_int (\code{numeri} value) confidence level of the interval
#' @param dense_header (\code{logical} value) Display the table headers in multiple rows.
#'
#' @return \code{rtable} with one row
#'
#' @export
#'
#' @seealso \code{\link{t_forest_tte}}
#'
#' @examples
#'
#' n <- 200
#' tte <- rexp(n)
#' set.seed(1)
#' is_event <- sample(c(TRUE, FALSE), n, TRUE)
#' strata_data <- data.frame(
#'   STRATA1 = sample(c("STR1", "STR2"), n, TRUE),
#'   STRATA2 = sample(c("low", "medium", "high"), n, TRUE)
#' )
#' col_by = factor(sample(c("ARM A", "ARM B"), n, TRUE), levels = c("ARM A", "ARM B"))
#' t_el_forest_tte(
#'   tte = tte,
#'   is_event = is_event,
#'   col_by = col_by,
#'   conf_int = 0.9
#' )
#'
#' t_el_forest_tte(
#'   tte = tte,
#'   is_event = is_event,
#'   col_by = col_by,
#'   strata_data = strata_data
#' )
#'
#' t_el_forest_tte(
#'   tte = tte,
#'   is_event = is_event,
#'   col_by = factor(rep("ARM A", n),
#'   levels = c("ARM A", "ARM B"))
#' )
#'
#' t_el_forest_tte(
#'   tte = tte, is_event = is_event,
#'   col_by = factor(rep("ARM B", n), levels = c("ARM A", "ARM B"))
#' )
t_el_forest_tte <- function(tte,
                            is_event,
                            col_by,
                            strata_data = NULL,
                            ties = "exact",
                            time_unit = "month",
                            row_name = "",
                            conf_int = 0.95,
                            dense_header = TRUE) {
  stopifnot(
    is.factor(col_by),
    is_numeric_single(conf_int) && 0 < conf_int && conf_int < 1
  )
  check_same_n(tte = tte, is_event = is_event, col_by = col_by, strata_data = strata_data)

  if (any(is.na(tte) | is.na(is_event))) {
    stop("NAs in tte and is_event is not allowed")
  }

  if (!is.factor(col_by) && nlevels(col_by) != 2) {
    stop("two levels required for col_by")
  }

  # Four scenarios:
  # 1. two arms
  # 2. ref arm has no records
  # 3. comp arm has no records
  # 4. both arms have no records

  col_N <- table(col_by) #nolintr

  if (any(col_N) > 0) {
    s_fit_km <- summary(survfit(Surv(tte, is_event) ~ col_by))$table
  }

  x <- if (all(col_N > 0)) {
    s_fit_km <- as.data.frame(s_fit_km)

    cox_sum  <- if (any(is_event == TRUE)) {
      if (is.null(strata_data)) {
        # use coxph.control argument to set iteration max to avoid warning if exceeded
        summary(coxph(Surv(tte, is_event) ~ col_by, ties = ties), conf.int = conf_int)
      } else {
        summary(coxph(Surv(tte, is_event) ~ col_by + strata(strata_data), ties = ties),
                conf.int = conf_int)
      }
    } else {
      # no events
      list(
        conf.int = c(NA, NA, NA, NA)
      )
    }

    list(
      total_n = length(tte),
      ref_n        = s_fit_km$records[1],
      ref_events   = s_fit_km$events[1],
      ref_median   = s_fit_km$median[1],
      comp_n        = s_fit_km$records[2],
      comp_events   = s_fit_km$events[2],
      comp_median   = s_fit_km$median[2],
      cox_hr   = cox_sum$conf.int[1],
      cox_lcl  = cox_sum$conf.int[3],
      cox_ucl  = cox_sum$conf.int[4]
    )
  } else if (col_N[1] > 0 && col_N[2] == 0) {
    s_fit_km <- as.list(s_fit_km)
    list(
      total_n = length(tte),
      ref_n        = s_fit_km$records,
      ref_events   = s_fit_km$events,
      ref_median   = s_fit_km$median,
      comp_n        = 0,
      comp_events   = 0,
      comp_median   = NA,
      cox_hr   = NA,
      cox_lcl  = NA,
      cox_ucl  = NA
    )
  } else if (col_N[1] == 0 && col_N[2] > 0) {
    s_fit_km <- as.list(s_fit_km)
    list(
      total_n = length(tte),
      comp_n        = s_fit_km$records,
      comp_events   = s_fit_km$events,
      comp_median   = s_fit_km$median,
      ref_n        = 0,
      ref_events   = 0,
      ref_median   = NA,
      cox_hr   = NA,
      cox_lcl  = NA,
      cox_ucl  = NA
    )
  } else {
    list(
      total_n = length(tte),
      comp_n        = 0,
      comp_events   = 0,
      comp_median   = NA,
      ref_n        = 0,
      ref_events   = 0,
      ref_median   = NA,
      cox_hr   = NA,
      cox_lcl  = NA,
      cox_ucl  = NA
    )
  }

  # format values

  format_hr <- if (!is.na(x$cox_hr) & x$cox_hr > 999.9) {
    ">999.9"
  } else {
    "xx.xx"
  }

  format_ci <- if (!is.na(x$cox_lcl) & x$cox_ucl > 999.9) {
    sprintf_format("(%.2f, >999.9)")
  } else {
    "(xx.xx, xx.xx)"
  }

  if (x$total_n != x$ref_n + x$comp_n) {
    stop("count inconsistency")
  }

  table_header <- if (dense_header) {
    rheader(
      rrow(
        row.name = "",
        NULL,
        rcell(levels(col_by)[1], colspan = 3),
        rcell(levels(col_by)[2], colspan = 3),
        NULL,
        NULL
      ),
      rrow(
        row.name = "Baseline Risk",
        "Total",
        "n", "Events", "Median",
        "n", "Events", "Median",
        "Hazard",
        paste0((conf_int) * 100, "%")
      ),
      rrow(
        row.name = "Factors",
        "n",
        NULL, NULL, paste0("(", time_unit, ")"),
        NULL, NULL, paste0("(", time_unit, ")"),
        if (is.null(strata_data)) "Ratio" else "Ratio*",
        "Wald CI"
      )
    )
  } else {
    rheader(
      rrow(
        row.name = "",
        NULL,
        rcell(levels(col_by)[1], colspan = 3),
        rcell(levels(col_by)[2], colspan = 3),
        NULL,
        NULL
      ),
      rrow(
        row.name = "Baseline Risk Factors",
        "Total n",
        "n", "Events", paste0("Median (", time_unit, ")"),
        "n", "Events", paste0("Median (", time_unit, ")"),
        if (is.null(strata_data)) "Hazard Ratio" else "Hazard Ratio*",
        paste0((conf_int) * 100, "% Wald CI")
      )
    )
  }

  rtable(
    header = table_header,
    rrow(
      row_name,
      rcell(x$total_n, "xx"),
      rcell(x$ref_n, "xx"),
      rcell(x$ref_events, "xx"),
      rcell(x$ref_median, "xx.xx"),
      rcell(x$comp_n, "xx"),
      rcell(x$comp_events, "xx"),
      rcell(x$comp_median, "xx.xx"),
      rcell(x$cox_hr, format_hr),
      rcell(c(x$cox_lcl, x$cox_ucl), format = format_ci)
    )
  )

}
