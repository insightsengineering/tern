#' Response Table
#'
#' The response table function summarizes response data by groups. The function
#' produces frequency counts and rates for responders and each response
#' categories, as well as conducts comparisons between groups' response rates
#' and odds ratio.
#'
#' @inheritParams argument_convention
#' @param partition_rsp_by (\code{factor} vector)
#'   Contains one or more response categories, generate additional statistics
#'   partitioned by each response categories. If \code{NULL}, tabulation by each
#'   response categories will not be performed.
#' @param strata_data (\code{character}, \code{factor} or \code{data.frame})\cr
#'   Used for \code{\link[survival:strata]{stratification factors}}
#'   If \code{NULL}, no stratified analysis is performed. See details for
#'   further explanation.
#'
#' @details For the test of difference in response rates, Wald confidence
#'   interval with and without continuity correction are both reported. If no
#'   stratification factors are specified, Chi-squared test is performed by
#'   default to test for equality of proportions in response rate between
#'   reference and comparison groups. If stratification factors are added,
#'   Cochran-Mantel-Haensel test is performed instead. Statistics from the
#'   stratification-adjusted test will be reported for the p-value of test of
#'   equal proportions, odds ratio and its corresponding 95\% confidence
#'   interval.
#'
#'   The display order of response categories in partitioned statistics section
#'   inherits the factor level order of \code{partition_rsp_by}. Use
#'   \code{\link[base]{factor}} and its \code{levels} argument to include or
#'   exclude response categories and arrange display order. If response values
#'   contains missing or "Non Evaluable (NE)", 95\% confidence interval will not
#'   be calculated.
#'
#' @template return_rtable
#'
#' @importFrom stats binom.test prop.test mantelhaen.test
#' @importFrom purrr compact
#'
#' @export
#'
#' @template author_liaoc10
#'
#' @seealso \code{\link{t_forest_rsp}}
#'
#' @examples
#' set.seed(1)
#' t_rsp(rsp = sample(c(TRUE, FALSE), 200, TRUE), col_by = factor(rep(c("A", "B"), each = 100)))
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' ADRS <- radrs(cached = TRUE)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI")
#'
#' # Example 1 - ARM B as reference
#' #    "NON CR/PD" response category dropped from partition section since no observations
#' #     model with no stratifiaction factors, Chi-square test is performed
#' tbl <- t_rsp(
#'   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'   col_by = relevel(factor(ADRS_f$ARMCD), "ARM B"),
#'   partition_rsp_by = droplevels(factor(
#'     ADRS_f$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
#'   ))
#' )
#'
#' tbl
#'
#' \dontrun{
#' Viewer(tbl)
#' }
#'
#' \dontrun{
#' # Example 2 - ARM B as reference, ARM C displayed before ARM A
#' #    "NON CR/PD" response category displayed in partition section, "NE" responses
#' #     are not displayed model with two stratifiaction factors, CMH test performed
#' tbl2 <- t_rsp(
#'  rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'  col_by = factor(ADRS_f$ARMCD, c("ARM B", "ARM C", "ARM A")),
#'  partition_rsp_by = factor(ADRS_f$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD")),
#'  strata_data = ADRS_f[c("RACE")]
#' )
#'
#' tbl2
#' }
#'
#' \dontrun{
#' Viewer(tbl2)
#' }
#'
#' # Example 3 - when all observations are non-responders
#' ADRS <- data.frame(
#'   rsp = FALSE,
#'   arm = rep(c("A", "B"), each = 200),
#'   stringsAsFactors = FALSE
#' )
#'
#' t_rsp(rsp = ADRS$rsp, col_by = factor(ADRS$arm))
#'
#'
#' # table_tree
#' tbls <- t_rsp(rsp = ADRS$rsp, col_by = factor(ADRS$arm), table_tree = TRUE)
#' summary(tbls)
#'
#' # Example 4 - Single Arm study
#' t_rsp(
#'    rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#'    col_by = factor(rep("Single ARM", nrow(ADRS_f))),
#'    partition_rsp_by = droplevels(factor(
#'      ADRS_f$AVALC, levels = c("CR", "PR", "MR", "SD", "PD")
#'   ))
#' )
#'
#' # Example 5 - Stratified
#'
#' t_rsp(
#' rsp = ADRS_f$AVALC %in% c("CR", "PR"),
#' col_by = relevel(factor(ADRS_f$ARMCD), "ARM B"),
#' strata_data = ADSL[,c("STRATA2", "STRATA1")]
#' )
#'
t_rsp <- function(rsp,
                  col_by,
                  partition_rsp_by = NULL,
                  strata_data = NULL,
                  table_tree = FALSE) {

  col_by <- col_by_to_factor(col_by)
  stopifnot(is.factor(col_by))
  col_N <- get_N(col_by) # nolint
  check_col_by_factor(rsp, col_by, col_N, min_num_levels = 1)
  check_same_n(rsp = rsp, col_by = col_by, partition_rsp_by = partition_rsp_by, strata_data = strata_data)
  if (!is.null(strata_data)) {
    check_data_frame(strata_data)
  }
  stopifnot(
    is.logical(rsp),
    !any(is.na(rsp))
  )

  # Calculations for table in sections
  #####################################
  # Responder section
  tbl_response <- rbind(
    rtabulate(
      rsp,
      col_by = col_by,
      positives_and_proportion,
      format = "xx.xx (xx.xx%)",
      row.name = "Responders"
    ),
    rtabulate(
      !rsp,
      col_by = col_by,
      positives_and_proportion,
      format = "xx.xx (xx.xx%)",
      row.name = "Non-Responders"
    )
  )

  # Response Rate section
  tbl_clopper_pearson <- rtabulate(
    x = rsp,
    col_by = col_by,
    function(x) {
      binom.test(sum(x), length(x))$conf.int * 100
    },
    format = "(xx.xx, xx.xx)",
    row.name = "95% CI for Response Rates (Clopper-Pearson)"
  )

  # Difference in Response Rates section

  if (nlevels(col_by) == 1) {
    tbl_difference <- NULL
    tbl_odds_ratio <- NULL
  } else {
    tbl_difference <- rbind(
      tabulate_pairwise(
        rsp,
        col_by,
        function(x, by) {
          diff(tapply(x, by, mean)) * 100
        },
        format = "xx.xx",
        row.name = "Difference in Response Rates (%)"
      ),

      # wald test without continuity correction
      indent(rbind(
        tabulate_pairwise(
          rsp,
          col_by,
          function(x, by) {
            t.tbl <- table(by, x)
            if (all(dim(t.tbl) == 2)) {
              t_wc <- prop.test(t.tbl, correct = FALSE)
              rcell(t_wc$conf.int * 100, format = "(xx.xx, xx.xx)")
            } else {
              rcell("-")
            }
          },
          row.name = "95% CI for difference (Wald without correction)"
        ),
        # wald test with  continuety correction
        tabulate_pairwise(
          rsp,
          col_by,
          function(x, by) {
            t.tbl <- table(by, x)
            if (all(dim(t.tbl) == 2)) {
              t_wc <- prop.test(t.tbl, correct = TRUE)
              rcell(t_wc$conf.int * 100, format = "(xx.xx, xx.xx)")
            } else {
              rcell("-")
            }
          },
          row.name = "95% CI for difference (Wald with continuity correction)"
        ),

        # p-value dependent on strata_data
        if (is.null(strata_data)) {
          tabulate_pairwise(
            rsp,
            col_by,
            function(x, by) {
              t.tbl <- table(by, x)
              if (all(dim(t.tbl) == 2)) {
                t_wc <- prop.test(table(by, x), correct = FALSE)
                rcell(t_wc$p.value, format = "xx.xxxx")
              } else {
                rcell("-")
              }
            },
            row.name = "p-value (Chi-squared)"
          )
        } else {
          tabulate_pairwise(
            data.frame(
              rsp = rsp,
              strata = do.call(strata, strata_data)
            ),
            col_by,
            function(x, by) {
              if (any(tapply(x$rsp, x$strata, length) < 5)) {
                rcell("<5 data points")
              } else {
                t.tbl <- table(by, x$rsp, x$strata)
                if (all(dim(t.tbl)[1:2] == 2)) {
                  t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$by), , ] # nolint
                  t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
                  rcell(t_m$p.value, format = "xx.xxxx")
                } else {
                  rcell("-")
                }
              }
            },
            row.name = "p-value (Cochran-Mantel-Haenszel)*"
          )
        }
      ), 1)
    )


    # Odds Ratio
    tbl_odds_ratio <- rbind(
      tabulate_pairwise(
        if (is.null(strata_data)) {
          rsp
        } else {
          data.frame(
            rsp = rsp,
            strata = do.call(strata, strata_data)
          )
        },
        col_by,
        function(x, by) {
          if (is.null(strata_data)) {
            t.tbl <- table(by, x)
            if (all(dim(t.tbl) == 2)) {
              fit <- odds_ratio(t.tbl)
              rcell(fit$estimator, "xx.xx")
            } else {
              rcell("-")
            }
          } else {
            if (any(tapply(x$rsp, x$strata, length) < 5)) {
              rcell("<5 data points")
            } else {
              t.tbl <- table(by, x$rsp, x$strata)
              if (all(dim(t.tbl)[1:2] == 2)) {
                t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$by), , ] # nolint
                t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
                rcell(t_m$estimate, format = "xx.xx")
              } else {
                rcell("-")
              }
            }
          }
        },
        row.name = ifelse(is.null(strata_data), "Odds Ratio", "Odds Ratio*")
      ),

      tabulate_pairwise(
        if (is.null(strata_data)) {
          rsp
        } else {
          data.frame(
            rsp = rsp,
            strata = do.call(strata, strata_data)
          )
        },
        col_by,
        function(x, by) {
          if (is.null(strata_data)) {
            t.tbl <- table(by, x)
            if (all(dim(t.tbl) == 2)) {
              fit <- odds_ratio(table(by, x))
              rcell(fit$conf.int, "(xx.xx, xx.xx)")
            } else {
              rcell("-")
            }

          } else {
            if (any(tapply(x$rsp, x$strata, length) < 5)) {
              rcell("<5 data points")
            } else {
              t.tbl <- table(by, x$rsp, x$strata)
              if (all(dim(t.tbl)[1:2] == 2)) {
                t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$by), , ] # nolint
                t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
                rcell(t_m$conf.int, format = "(xx.xx, xx.xx)")
              } else {
                rcell("-")
              }
            }
          }
        },
        row.name = "95% CI",
        indent = 1
      )
    )
  }



  # Partition by response categories
  tbl_partition <- if (is.null(partition_rsp_by)) {
    NULL
  } else {
    values <- lapply(split(col_by, partition_rsp_by, drop = FALSE),
                     function(x) {

                       x <- factor(x, levels = levels(col_by))
                       x_x <- split(x, x, drop = FALSE)

                       vals <- Map(function(y, arm) {

                         n_arm <- sum(col_by == arm)
                         n_y <- length(y)

                         if (is.na(n_arm) || n_arm == 0) {
                           list(n_p = rcell("-"), ci = rcell("-"))
                         } else if (n_y == 0) {
                           list(n_p = rcell(n_y, "xx.xx"),
                                ci = rcell("-"))
                         } else {
                           list(
                             n_p = rcell(n_y * c(1, 1 / n_arm), "xx.xx (xx.xx%)"),
                             ci = rcell(binom.test(n_y, n_arm)$conf.int * 100, "(xx.xx, xx.xx)")
                           )
                         }
                       }, x_x, names(x_x))

                     }
    )

    # Display full labels for responses in controlled codelist
    rsp_full_label <- c(
      CR          = "Complete Response (CR)",
      PR          = "Partial Response (PR)",
      SD          = "Stable Disease (SD)",
      `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
      PD          = "Progressive Disease (PD)",
      NE          = "Not Evaluable (NE)",
      Missing     = "Missing",
      `NE/Missing` = "Missing or unevaluable"
    )

    values_label <- ifelse(
      names(values) %in% names(rsp_full_label),
      rsp_full_label[names(values)],
      names(values)
    )

    tbls_part <- Map(function(vals, name) {
      rtable(
        header = header(tbl_response),
        rrowl(name, lapply(vals, `[[`, "n_p")),
        if (name %in% c("Not Evaluable (NE)", "Missing or unevaluable", "Missing")) {
          rrow(NULL)
        } else {
          rrowl("95% CI", lapply(vals, `[[`, "ci"), indent = 1)
        }
      )

    }, values, values_label)

    rbindl_rtables(tbls_part, gap = 1)
  }

  #--- Footer section, if any for notes on stratification ---#
  tbl_footer <- if (is.null(strata_data)) {
    NULL
  } else {
    n_strata <- length(strata_data)
    rtable(
      header = header(tbl_response),
      rrow(
        paste(
          "* Model stratified by",
          ifelse(
            n_strata < 2,
            names(strata_data),
            paste(paste(names(strata_data)[-n_strata], collapse = ", "), "and", names(strata_data)[(n_strata)])
          )
        )
      )
    )
  }

  node_if_non_null <- function(name, tbl) {
    if (is.null(tbl)) {
      NULL
    } else {
      # add header
      header(tbl) <- rheader(rrowl("", levels(col_by)))
      header_add_N(tbl, col_N)
      node(name = invisible_node_name(name), content = tbl)
    }
  }
  tree <- invisible_node(
    children = compact(list(
      node_if_non_null("Responders", tbl_response),
      node_if_non_null("Clopper Pearson", tbl_clopper_pearson),
      node_if_non_null("Difference Tests", tbl_difference),
      node_if_non_null("Odds Ratio", tbl_odds_ratio),
      node_if_non_null("Partition", tbl_partition),
      node_if_non_null("Footer", tbl_footer)
    ))
  )

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

#' Function to calculate odds ratio and confidence interval
#'
#' @param x a matrix or table of 2-by-2 dimensions
#' @param conf_level confidence level for the returned confidence interval
#'
#' @noRd
#'
#' @examples
#' odds_ratio(table(mtcars$vs, mtcars$am))
#' odds_ratio(matrix(c(12,6,7,7), nrow = 2, byrow = TRUE), conf_level = 0.90)
odds_ratio <- function(x, conf_level = 0.95) {

  theta <- x[1, 1] * x[2, 2] / (x[2, 1] * x[1, 2])
  # Asymptotic standard error
  ase <- sqrt(sum(1 / x))
  ci <- exp(log(theta) + c(-1, 1) * qnorm(0.5 * (1 + conf_level)) * ase)

  list(
    estimator = theta,
    se = ase,
    conf.int = ci,
    conf_level = conf_level
  )
}
