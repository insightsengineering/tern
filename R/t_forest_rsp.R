#' Response Table as used for Forest Plot
#'
#' The Response forest plot table summarizes response data by groups. The
#' function returns sample sizes and responder counts and response rates for
#' each analysis arm, as well as a odds ratio and the corresponding
#' confidence interval from a univariate logistic model.
#'
#' @inheritParams argument_convention
#' @inheritParams t_el_forest_rsp
#' @param row_by_list \code{list} or \code{data.frame} with one factor variable to calculate
#'   the \code{t_el_forest_tte}
#' @param total string of total row added. If \code{NULL} then no total row is added.
#'
#' @details
#' Logistic regression is used for odds ratio calculation.
#'
#' Each row in the returned table contains the analysis statistics for a
#' subgroup of data (indicated by the row name). The summary table is consist of
#' the following 9 columns:
#'
#' \describe{
#'
#'   \item{1}{\emph{Total n} the total number of subjects included in analysis
#'   population}
#'
#'   \item{3-4}{Summary of responders in reference arm, \emph{n} and
#'   \emph{Responders} are the total number of patients and the number of
#'   responders in reference arm, respectively. \code{Response.Rate} is the
#'   percentage of responders in reference arm.}
#'
#'   \item{5-7}{same statistics as for reference arm now for comparison arm}
#'
#'   \item{8}{\emph{Odds Ratio} ranges from 0 to infinity, estimated by applying
#'   {\code{tern::odds_ratio}}. If stratification factors are added,
#'   Cochran-Mantel-Haensel test is performed instead. Statistics from the
#'   stratification-adjusted test will be reported for the p-value of test of
#'   equal proportions, odds ratio and its corresponding 95\% confidence
#'   interval. Odds ratio greater than 1 indicates better performance in comparison arm; odds
#'   ratio less than 1 indicates better performance in reference arm.}
#'
#'   \item{9}{\emph{Confidence Interval} The confidence interval indicates the level of
#'   uncertainty around the measure of effect (Odds Ratio). Because only a small
#'   sample of the overall population is included in the analysis, by having an
#'   upper and lower confidence limit we can infer that the true treatment
#'   effect lies in between. By default, the 95% confidence interval is calculated.
#'   If it includes 1, then we say that the difference between two arms is not
#'   significant at a significance level of 0.05.}
#'
#' }
#'
#' @template return_rtable
#'
#' @importFrom purrr map
#' @export
#'
#' @template author_song24
#'
#' @seealso \code{\link{t_el_forest_rsp}}, \code{\link{t_rsp}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' library(purrr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL$'FAKE Name > -1.3 Flag' <- rep(c('Y', 'N'), 50)
#' attr(ADSL$'FAKE Name > -1.3 Flag', "label") <- "dummy"
#'
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   dplyr::filter(ARM != 'C: Combination') %>%
#'   mutate(ARM = droplevels(ARM))
#'
#' tbl <- t_forest_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = as_factor_keep_attributes(ADRS_f$ARM),
#'   row_by_list = ADRS_f[, c("SEX", "RACE", "FAKE Name > -1.3 Flag")] %>%
#'     map(as_factor_keep_attributes),
#'   conf_int = 0.9
#' )
#'
#' tbl
#'
#' \dontrun{
#' Viewer(tbl)
#' }
#'
#' tbl2 <- t_forest_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = as_factor_keep_attributes(ADRS_f$ARM),
#'   row_by_list = ADRS_f[, c("BMRKR2")] %>%
#'     map(as_factor_keep_attributes),
#'   strata_data = ADRS_f[ , "STRATA1"]
#' )
#'
#' \dontrun{
#' Viewer(tbl2)
#' }
#'
#' # table tree
#' tbls <- t_forest_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = ADRS_f$ARM,
#'   row_by_list = ADRS_f[, c("SEX", "RACE", "FAKE Name > -1.3 Flag")] %>%
#'     map(as_factor_keep_attributes),
#'   table_tree = TRUE
#' )
#' summary(tbls)
t_forest_rsp <- function(rsp,
                         col_by,
                         row_by_list = NULL,
                         total = "ALL",
                         strata_data = NULL,
                         conf_int = 0.95,
                         dense_header = FALSE,
                         table_tree = FALSE) {

  stopifnot(
    is.logical(rsp),
    is.null(total) || is_character_single(total),
    is.list(row_by_list),
    is_numeric_single(conf_int) && (0 < conf_int) && (conf_int < 1)
  )

  check_strata(strata_data)
  do.call(check_same_n, c(list(rsp = rsp, col_by = col_by, strata_data = strata_data), row_by_list))

  row_by_list <-  row_by_list %>% map(explicit_na)
  # take label if it exists, otherwise rowname
  # equivalent of var_labels(as.data.frame(by), fill = TRUE) for non data.frames
  names(row_by_list) <- Map(`%||%`, lapply(row_by_list, label), names(row_by_list))

  df <- if (is.null(strata_data)){
    data.frame(rsp = rsp, col_by = col_by)
  } else {
    data.frame(rsp = rsp, col_by = col_by, strata_data = strata_data)
  }

  dfs <- lapply(row_by_list, function(rows_by) esplit(df, rows_by))

  # nested structure, e.g. list dfs$SEX$M -> tree accessed like dfs[['SEX']][['M']]
  data_tree <- nested_list_to_tree(dfs, format_data = node_format_data(children_gap =  0), max_depth = 2)

  if (!is.null(total)) {
    data_tree@children <- c(list(node(name = total, content = df)), data_tree@children)
  }

  tree <- rapply_tree(data_tree, function(name, content, ...) {
    if (is.data.frame(content)) {
      list(
        name = invisible_node_name(name),
        content = t_el_forest_rsp(
          rsp = content$rsp,
          col_by = content$col_by,
          strata_data = if (is.null(strata_data)) NULL else subset(content, select = -c(rsp, col_by)),
          row_name = name,
          conf_int = conf_int,
          dense_header = dense_header
        )
      )
    } else {
      # data_tree has empty nodes except for leaf nodes
      list(name = name, content = NULL)
    }
  })

  tree@format_data <- node_format_data(children_gap = 1)

  model_type <- if (is.null(strata_data)){
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



#' Elementary Table for Forest Response Plot
#'
#'
#' @inheritParams argument_convention
#' @inheritParams t_rsp
#' @param strata_data data for stratification factors (categorical variables).
#'   If \code{NULL}, no stratified analysis is performed.
#' @param row_name name of row
#' @param conf_int confidence level of the interval
#' @param dense_header Display the table headers in multiple rows.
#'
#' @return rtable with one row
#'
#' @importFrom stats binomial confint glm
#' @export
#'
#' @seealso \code{\link{t_forest_rsp}}
#'
#' @examples
#' rsp = sample(c(TRUE, FALSE), 200, TRUE)
#' col_by = factor(sample(c("ARM A", "ARM B"), 200, TRUE), levels = c("ARM A", "ARM B"))
#' strata_data <- data.frame(
#'   STRATA1 = sample(c("STR1", "STR2"), 200, TRUE),
#'   STRATA2 = sample(c("low", "medium", "high"), 200, TRUE)
#' )
#' t_el_forest_rsp(
#'   rsp = rsp,
#'   col_by = col_by
#' )
#'
#' t_el_forest_rsp(
#'   rsp = rsp,
#'   col_by = col_by,
#'   strata_data = strata_data
#' )
#'
t_el_forest_rsp <- function(rsp,
                            col_by,
                            strata_data = NULL,
                            conf_int = 0.95,
                            row_name = "",
                            dense_header = FALSE) {
  # currently only works for factor
  stopifnot(is.factor(col_by))

  check_same_n(rsp = rsp, col_by = col_by, strata_data = strata_data)
  col_N <- table(col_by) #nolintr
  check_col_by_factor(rsp, col_by, col_N,  min_num_levels = 2)
  stopifnot(is.logical(rsp),
      is_numeric_single(conf_int) && 0 < conf_int && conf_int < 1 )

  if (nlevels(col_by) != 2) {
    stop("col_by number of levels is restricted to two")
  }

  rsp_s <- split(rsp, col_by)

  x_descr <- list(
    total_n = length(rsp),
    ref_n = length(rsp_s[[1]]),
    ref_resp = sum(rsp_s[[1]]),
    comp_n = length(rsp_s[[2]]),
    comp_resp = sum(rsp_s[[2]])
  )
  x_descr[["ref_resp_rate"]] <- if (x_descr$ref_n != 0) {
    x_descr$ref_resp / x_descr$ref_n
  } else {
    NA
  }
  x_descr[["comp_resp_rate"]] <- if (x_descr$comp_n != 0) {
    x_descr$comp_resp / x_descr$comp_n
  } else {
    NA
  }

  x_fit <- if (all(col_N > 0)) {
      if (is.null(strata_data)){
        or_tbl <- table(rsp, col_by)
        if (all(dim(or_tbl) == 2)){
          odds_ratio(or_tbl, conf_level = conf_int)
        } else {
          list(estimator = NA,
               conf.int = c(NA, NA))
        }

      } else {
        strat <- do.call(strata, strata_data)
        or_tbl <- table(rsp, col_by, strat)
        if (all(dim(or_tbl)[1:2] == 2) && all(apply(or_tbl, 3L, sum) >= 2)){
             mantelhaen.test(or_tbl, correct = FALSE, conf.level = conf_int)
        } else {
          list(estimate = NA,
               conf.int = c(NA, NA))
        }

      }
  } else {
    NULL
  }


  x_or_ci <- if (is.null(x_fit)) {
    list(
       oddr = NA,
       lcl = NA,
       ucl = NA
    )
  } else {
        list(
          oddr = if (is.null(strata_data)) x_fit$estimator else x_fit$estimate,
          lcl = x_fit$conf.int[1],
          ucl = x_fit$conf.int[2]
        )
  }


  ## format values
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
        row.name = "Baseline",
        "Total",
        NULL, NULL, "Response",
        NULL, NULL, "Response",
        "Odds", NULL
      ),
      rrow(
        row.name = "Risk Factors",
        "n",
        "n", "Responders", "Rate",
        "n", "Responders", "Rate",
        if (is.null(strata_data)) "Ratio" else "Ratio*",
        paste0((conf_int)*100, "% CI")
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
        "n", "Responders", "Response.Rate",
        "n", "Responders", "Response.Rate",
        if (is.null(strata_data)) "Odds Ratio" else "Odds Ratio*",
        paste0((conf_int)*100, "% CI")
      )
    )
  }


  format_or <- if (!is.na(x_or_ci$oddr) & x_or_ci$oddr > 999.9) {
    ">999.9"
  } else {
    "xx.xx"
  }
  format_ci <- if (!is.na(x_or_ci$ucl) & x_or_ci$ucl > 999.9) {
    sprintf_format("(%.2f, >999.9)")
  } else {
    "(xx.xx, xx.xx)"
  }

  rtable(
    header = table_header,
    rrow(
      row.name = row_name,
      rcell(x_descr$total_n, "xx"),
      rcell(x_descr$ref_n, "xx"),
      rcell(x_descr$ref_resp, "xx"),
      rcell(x_descr$ref_resp_rate, "xx.xx"),
      rcell(x_descr$comp_n, "xx"),
      rcell(x_descr$comp_resp, "xx"),
      rcell(x_descr$comp_resp_rate, "xx.xx"),
      rcell(x_or_ci$oddr, format_or),
      rcell(c(x_or_ci$lcl, x_or_ci$ucl), format_ci)
    )
  )
}
