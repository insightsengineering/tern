#' Elementary table for analysis of covariance (ANCOVA).
#'
#' This function returns an elementary summary table for an ANCOVA analysis, which compares the
#' covariate adjusted means of multiple groups with a reference group. Specifically, the
#' first level of the `arm()` factor variable on the right-hand side of the provided `formula`
#' is taken as the reference group.
#'
#' @inheritParams argument_convention
#' @inheritParams s_ancova
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{s_ancova}}
#'
#' @importFrom rtables rtable rrowl header_add_N
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS_filtered <- radqs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "FKSI-FWB", AVISIT == "WEEK 1 DAY 8")
#' t_el_ancova(
#'   formula = CHG ~ BASE + STRATA1 + arm(ARMCD),
#'   data = ADQS_filtered,
#'   col_N = table(ADSL$ARMCD)
#' )
t_el_ancova <- function(formula,
                        data,
                        col_N, # nolint
                        conf_level = 0.95) {

  ancova_summaries <- s_ancova(
    formula = formula,
    data = data,
    conf_level = conf_level
  )
  sum_fit <- ancova_summaries$sum_fit
  sum_diff <- ancova_summaries$sum_contrasts
  sum_diff_ci <- Map(
    function(x, y) c(x, y),
    sum_diff$lower.CL,
    sum_diff$upper.CL
  )
  tbl <- rtables::rtable(
    header = as.character(sum_fit[, 1]),
    rtables::rrowl(
      "n",
      sum_fit$n_complete,
      format = "xx"
    ),
    rtables::rrowl(
      "Adjusted Mean",
      sum_fit$emmean,
      format = "xx.xx"
    ),
    rtables::rrowl(
      "Difference in Adjusted Means",
      c(list(NULL),
        sum_diff$estimate),
      format = "xx.xx"
    ),
    rtables::rrowl(
      paste0(conf_level * 100, "% CI"),
      c(list(NULL),
        sum_diff_ci),
      format = "xx.xx - xx.xx",
      indent = 1
    ),
    rtables::rrowl(
      "p-value",
      c(list(NULL),
        sum_diff$p.value),
      format = "x.xxxx | (<0.0001)",
      indent = 1
    )
  )

  result <- rtables::header_add_N(tbl, col_N)
  return(result)
}


#' Summary table for analysis of covariance (ANCOVA).
#'
#' This is a wrapper around the basic \code{\link{t_el_ancova}} function with the difference that it has a `row_by`
#' argument. This can e.g. be used to analyze multiple endpoints and/or multiple timepoints within the same response
#' variable.
#'
#' @inheritParams t_el_ancova
#' @inheritParams argument_convention
#' @param row_by (\code{factor} or \code{data.frame})\cr
#'   Defines how data from `data` is split into sub-tables. Dimensions must match
#'   dimensions of `data` and no missing values are allowed. Multi-level nesting is possible when
#'   `row_by` is a \code{data.frame}. Columns should be ordered with the first column specifying
#'   the first variable to split by and the last column specifying the last variable to split by.
#'
#' @return Either \code{rtable} or \code{node} object (see `table_tree` argument).
#'   For every unique combination of levels of `row_by` an ANCOVA table using \code{\link{t_el_ancova}}
#'   will be created. The individual tables are then stacked together.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS <- radqs(cached = TRUE) %>%
#'   dplyr::filter(AVISIT == "WEEK 1 DAY 8")
#'
#' # Multiple endpoints.
#' ADQS_week1day8 <- dplyr::filter(ADQS, AVISIT == "WEEK 1 DAY 8")
#' t_ancova(
#'   formula = CHG ~ BASE + STRATA1 + arm(ARMCD),
#'   data = ADQS_week1day8,
#'   row_by = ADQS_week1day8$PARAMCD,
#'   col_N = table(ADSL$ARMCD)
#' )
#'
#' # Multiple visits (in this data set there is only one visit, though).
#' ADQS_fksifwb <- dplyr::filter(ADQS, PARAMCD == "FKSI-FWB")
#' t_ancova(
#'   formula = CHG ~ BASE + STRATA1 + arm(ARMCD),
#'   data = ADQS_fksifwb,
#'   row_by = ADQS_fksifwb$AVISIT,
#'   col_N = table(ADSL$ARMCD)
#' )
#'
#' # Multiple timepoints and visits.
#' t_ancova(
#'   formula = CHG ~ BASE + STRATA1 + arm(ARMCD),
#'   data = ADQS,
#'   row_by = ADQS[, c("PARAMCD", "AVISIT")],
#'   col_N = table(ADSL$ARMCD)
#' )
t_ancova <- function(formula,
                     data,
                     row_by,
                     col_N, # nolint
                     conf_level = 0.95,
                     table_tree = FALSE) {

  if (!is_nested_by(row_by)) {
    check_row_by(row_by, data)
  }
  if (!is.list(row_by)) {
    row_by <- list(row_by)
  }

  tree_data <- rsplit_to_tree(
    lst = data,
    by_lst = row_by,
    drop_empty_levels = TRUE
  )
  tree <- rapply_tree(
    tree_data,
    function(name, content, path, is_leaf) {
      content <-
        if (is_leaf) {
          t_el_ancova(
            data = content,
            formula = formula,
            col_N = col_N, # nolint
            conf_level = conf_level
          )
        } else {
          NULL
        }
      list(
        name = name,
        content = content
      )
    }
  )

  tree@name <- invisible_node_name(tree@name)
  result <-
    if (table_tree) {
      tree
    } else {
      to_rtable(tree)
    }
  return(result)
}
