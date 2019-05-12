#' Time-to-event Table as used for Forest Plot
#'
#' The time-to-event forest plot table summarizes time-to-event data by groups. The function returns event counts and
#' median survival time for each analysis arm, as well as a hazard ratio and the corresponding 95\% confidence interval
#' from a Cox proportional hazard model.
#'
#' @inheritParams argument_convention
#' @inheritParams t_el_forest_tte
#' @param by \code{list} or \code{data.frame} with one factor variable to calculate the \code{t_el_forest_tte}
#' @param strata_data currently not supported
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
#'   \item{9}{\emph{95\% Wald CI} The 95% confidence interval indicates the level of uncertainty
#'   around the measure of effect (Hazard Ratio). Because only a small sample of the overall
#'   population is included in the analysis, by having an upper and lower confidence limit
#'   we can infer that the true treatment effect lies in between. If the 95% confidence interval
#'   includes 1, then we say that the difference between two arms is not significant at a significance level of 0.05.}
#' }
#'
#' @export
#'
#' @template author_song24
#'
#' @seealso \code{\link{t_tte}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(seed = 1)
#'
#' ADTTE <- radtte(ADSL, seed = 2)
#' ADTTE_f <- ADTTE %>%
#'   dplyr::filter(PARAMCD == "OS", ARMCD %in% c("ARM B", "ARM A")) %>%
#'   mutate(ARMCD = droplevels(ARMCD))
#'
#' tbl <- t_forest_tte(
#'   tte = ADTTE_f$AVAL,
#'   is_event = ADTTE_f$CNSR == 0,
#'   col_by = ADTTE_f$ARMCD,
#'   by = droplevels(ADTTE_f[, c("SEX", "RACE")]),
#'   ties = "exact",
#'   dense_header = TRUE
#' )
#'
#' tbl
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
#'   by = droplevels(ADTTE_f[, c("SEX", "RACE")]),
#'   ties = "exact",
#'   dense_header = TRUE,
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' tbl <- rbind_table_tree(lapply(tbls, rbindl_rtables))
#'
t_forest_tte <- function(tte, is_event, col_by, by, total = "All",
                         strata_data = NULL,
                         ties = "exact",
                         time_unit = "month",
                         dense_header = FALSE,
                         table_tree = FALSE) {

  if (!is.null(strata_data)) {
    stop("strata_data argument is currently not implemented")
  }

  check_same_n(tte = tte, is_event = is_event)

  by <- get_forest_by(by, length(tte))

  df <- data.frame(tte = tte, is_event = is_event, col_by = col_by)

  dfs <- get_forest_data_tree(df, by, total)

  tbls <- lapply(dfs, function(x) {
    Map(function(x_level, level_name) {
      t_el_forest_tte(
        tte = x_level$tte,
        is_event = x_level$is_event,
        col_by = x_level$col_by,
        ties = ties,
        row_name = level_name,
        dense_header = dense_header
      )
    }, x, names(x))
  })


  if (table_tree) {
    table_tree(tbls)
  } else {
    tbl <- rbind_table_tree(lapply(tbls, rbindl_rtables))
    tbl <- tbl[-1, ] # remove total row
    attr(tbl[[1]], "indent") <- 0
    tbl
  }

}


#' Elementary Table for Forest Time to Event Plot
#'
#'
#' @inheritParams survival::coxph
#' @inheritParams argument_convention
#' @param ties the method used for tie handling in \code{\link[survival]{coxph}}.
#' @param time_unit The unit of median survival time. Default is \code{months}.
#' @param row_name name of row
#' @param dense_header Display the table headers in multiple rows.
#'
#' @return rtable with one row
#'
#' @export
#'
#' @examples
#'
#' n <- 200
#' tte <- rexp(n)
#' is_event <- sample(c(TRUE, FALSE), n, TRUE)
#'
#' t_el_forest_tte(
#'   tte = tte, is_event = is_event,
#'   col_by = factor(sample(c("ARM A", "ARM B"), n, TRUE), levels = c("ARM A", "ARM B"))
#' )
#'
#' t_el_forest_tte(
#'   tte = tte, is_event = is_event,
#'   col_by = factor(rep("ARM A", n), levels = c("ARM A", "ARM B"))
#' )
#'
#' t_el_forest_tte(
#'   tte = tte, is_event = is_event,
#'   col_by = factor(rep("ARM B", n), levels = c("ARM A", "ARM B"))
#' )
#'
t_el_forest_tte <- function(tte, is_event, col_by, ties = "exact",
                            time_unit = "month", row_name = "", dense_header = TRUE) {

  check_same_n(tte = tte, is_event = is_event, col_by = col_by)

  if (any(is.na(tte) | is.na(is_event))) {
    stop("NAs in tte and is_event is not allowed")
  }

  if (!is.factor(col_by) && nlevels(col_by) != 2) {
    stop("two levels required for col_by")
  }

  s_fit_km <- summary(survfit(Surv(tte, is_event) ~ col_by))$table

  # Three scenarios:
  # 1. two arms
  # 2. ref arm has no records
  # 3. comp arm has no records

  col_N <- table(col_by) #nolintr

  x <- if (all(col_N > 0)) {
    s_fit_km <- as.data.frame(s_fit_km)
    cox_sum  <- summary(coxph(Surv(tte, is_event) ~ col_by, ties = ties))

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
    stop("Invalid Arm Counts")
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
        "95%"
      ),
      rrow(
        row.name = "Factors",
        "n",
        NULL, NULL, paste0("(", time_unit, ")"),
        NULL, NULL, paste0("(", time_unit, ")"),
        "Ratio",
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
        "Hazard Ratio",
        "95% Wald CI"
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


get_forest_by <- function(by, n) {
  if (!is.null(by)) {
    if (!is.list(by)) {
      stop("by is required to be either a list or NULL")
    }
    if (any(vapply(by, length, numeric(1)) != n)) {
      stop("elements in by do not length n")
    }
    by <- all_as_factor(by)
    by <- lapply(by, na_as_level)
    names(by) <- var_labels(as.data.frame(by), fill = TRUE) # TODO: as.data.frame should not be necessary
  }
  by
}

#' Data Tree for Forest Plot
#'
#' @noRd
#'
#' @importFrom stats setNames
get_forest_data_tree <- function(df, by, total) {
  dfs <- if (!is.null(by)) {
    lapply(by, function(by_i) {
      split(df, by_i)
    })
  } else {
    NULL
  }

  if (!is.null(total)) {
    dfs <- c(list(total = setNames(list(df), total)), dfs)
  }

  dfs
}
