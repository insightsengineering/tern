#' Response Table as used for Forest Plot
#'
#' The Response forest plot table summarizes response data by groups. The
#' function returns sample sizes and responder counts and response rates for
#' each analysis arm, as well as a odds ratio and the corresponding 95\%
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
#'   univariate logistic regression. Binary response status (responder or
#'   non-responder) is the outcome and arm is the explanatory variable. Odds
#'   ratio greater than 1 indicates better performance in comparison arm; odds
#'   ratio less than 1 indicates better performance in reference arm.}
#'
#'   \item{9}{\emph{95 \% CI} The 95% confidence interval indicates the level of
#'   uncertainty around the measure of effect (Odds Ratio). Because only a small
#'   sample of the overall population is included in the analysis, by having an
#'   upper and lower confidence limit we can infer that the true treatment
#'   effect lies in between. If the 95% confidence interval includes 1, then we
#'   say that the difference between two arms is not significant at a
#'   significance level of 0.05.}
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
#' ADSL <- radsl(seed = 1)
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
#'     map(as_factor_keep_attributes)
#' )
#'
#' tbl
#'
#' \dontrun{
#' Viewer(tbl)
#' }
#'
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
#'
#' # todo: remove as_factor_keep_attributes above
t_forest_rsp <- function(rsp,
                         col_by,
                         row_by_list = NULL,
                         total = "ALL",
                         dense_header = FALSE,
                         table_tree = FALSE) {

  stopifnot(
    is.logical(rsp),
    is.null(total) || is_character_single(total),
    is.list(row_by_list)
  )
  do.call(check_same_n, c(list(rsp = rsp, col_by = col_by), row_by_list))

  row_by_list <-  row_by_list %>% map(na_as_level)
  # take label if it exists, otherwise rowname
  # equivalent of var_labels(as.data.frame(by), fill = TRUE) for non data.frames
  names(row_by_list) <- Map(`%||%`, lapply(row_by_list, label), names(row_by_list))

  # cannot be a data.frame as col_by may be a matrix
  df <- list(rsp = rsp, col_by = col_by)

  dfs <- lapply(row_by_list, function(rows_by) esplit(df, rows_by))

  # nested structure, e.g. list dfs$SEX$M -> tree accessed like dfs[['SEX']][['M']]
  data_tree <- nested_list_to_tree(dfs, format_data = node_format_data(children_gap =  0), max_depth = 1)

  if (!is.null(total)) {
    data_tree@children <- c(list(node(name = total, content = df)), data_tree@children)
  }

  tree <- rapply_tree(data_tree, function(name, content, ...) {
    if (!is.null(content)) {
      list(
        name = invisible_node_name(name),
        content = t_el_forest_rsp(
          rsp = content$rsp,
          col_by = content$col_by,
          row_name = name,
          dense_header = dense_header
        )
      )
    } else {
      # data_tree has empty nodes except for leaf nodes
      list(name = name, content = NULL)
    }
  })

  tree@format_data <- node_format_data(children_gap = 1)

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}



#' Elementary Table for Forest Response Plot
#'
#'
#' @inheritParams argument_convention
#' @inheritParams t_rsp
#' @param row_name name of row
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
#'
#' t_el_forest_rsp(
#'   rsp = sample(c(TRUE, FALSE), 200, TRUE),
#'   col_by = factor(sample(c("ARM A", "ARM B"), 200, TRUE), levels = c("ARM A", "ARM B"))
#' )
#'
t_el_forest_rsp <- function(rsp, col_by, row_name = "", dense_header = FALSE) {

  # todo: we can possibly use by_hierarchical here

  # currently only works for factor
  col_by <- col_by_to_factor(col_by)

  check_same_n(rsp = rsp, col_by = col_by)
  col_N <- table(col_by) #nolintr
  check_col_by_factor(rsp, col_by, col_N,  min_num_levels = 2)
  stopifnot(is.logical(rsp))

  if (nlevels(col_by) != 2) {
    stop("col_by number of levels is restricted to two")
  }


  rsp_s <- split(rsp, col_by)

  x_descr <- list(
    total_n = length(rsp),
    ref_n = length(rsp_s[[1]]),
    ref_resp = sum(rsp_s[[1]]),
    comp_n = length(rsp_s[[2]]),
    comp_resp = sum(rsp_s[[1]])
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

  glm_fit <- if (all(col_N > 0)) {
    try(
      glm(rsp ~ col_by, family = binomial(link = "logit"))
    )
  } else {
    NULL
  }


  x_logistic <- if (is.null(glm_fit) || is(glm_fit, "try-error")) {
    list(
      glm_or = NA,
      glm_lcl = NA,
      glm_ucl = NA,
      glm_pval = NA
    )
  } else {
    glm_sum <- summary(glm_fit)

    suppressWarnings({
      suppressMessages({
        list(
          glm_or = exp(glm_sum$coefficients[2, 1]),
          glm_lcl = tryCatch(
            exp(confint(glm_fit)[2, 1]),
            error = function(e) NA
          ),
          glm_ucl = tryCatch(
            exp(confint(glm_fit)[2, 2]),
            error = function(e) NA
          ),
          glm_pval = glm_sum$coefficients[2, 4]
        )
      })
    })
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
        "Ratio",
        "95% CI"
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
        "Odds Ratio",
        "95% CI"
      )
    )
  }


  format_or <- if (!is.na(x_logistic$glm_or) & x_logistic$glm_or > 999.9) {
    ">999.9"
  } else {
    "xx.xx"
  }
  format_ci <- if (!is.na(x_logistic$glm_ucl) & x_logistic$glm_ucl > 999.9) {
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
      rcell(x_logistic$glm_or, format_or),
      rcell(c(x_logistic$glm_lcl, x_logistic$glm_ucl), format_ci)
    )
  )
}
