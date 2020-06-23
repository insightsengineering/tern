#' Elementary table for analysis of covariance (ANCOVA).
#'
#' This function returns an elementary summary table for an ANCOVA analysis, which compares the
#' covariate adjusted means of multiple groups with a reference group. Specifically, the
#' first level of the `arm()` factor variable on the right-hand side of the provided `formula`
#' is taken as the reference group.
#'
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
#' ADQS_filtered <- radqs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "FKSI-FWB", AVISIT == "WEEK 1 DAY 8")
#' t_el_ancova(
#'   formula = CHG ~ BASE + STRATA1 + arm(ARMCD),
#'   data = ADQS_filtered
#' )
t_el_ancova <- function(formula,
                        data,
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

  result <- rtables::header_add_N(tbl, sum_fit$n_total)
  return(result)
}
