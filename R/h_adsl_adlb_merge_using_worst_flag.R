#' Helper Function for Deriving Analysis Datasets for LBT13 and LBT14
#'
#' Helper function that merges ADSL and ADLB datasets so that missing lab test records are inserted in the
#' output dataset.
#'
#' @param adsl (`data frame`) ADSL dataframe
#' @param adlb (`data frame`) ADLB dataframe
#' @param worst_flag (named `vector`)
#' Worst post-baseline lab flag variable
#' @param by_visit (`logical`) defaults to `FALSE` to generate worst grade per patient.
#' If worst grade per patient per visit is specified for `worst_flag`, then
#' `by_visit` should be `TRUE` to generate worst grade patient per visit.
#' @param no_fillin_visits (named `character`)
#' Visits that are not considered for post-baseline worst toxicity grade. Defaults to `c("SCREENING", "BASELINE")`.
#'
#' @return `df` containing variables shared between `adlb` and `adsl` along with variables relevant for analysis:
#' `PARAM`, `PARAMCD`, `ATOXGR`, and `BTOXGR`.  Optionally `AVISIT`, `AVISITN` are included when `by_visit = TRUE` and
#' `no_fillin_visits = c("SCREENING", "BASELINE")`.
#'
#' @export
#'
#' @details In the result data missing records will be created for the following situations:
#'  * patients who are present in `adsl` but have no lab data in `adlb` (both baseline and post-baseline)
#'  * patients who do not have any post-baseline lab values
#'  * patients without any post-baseline values flagged as the worst
#'
#' @importFrom dplyr select filter pull left_join
#'
#' @examples
#' library(scda)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#' adsl <- synthetic_cdisc_data("latest")$adsl
#'
#' # `h_adsl_adlb_merge_using_worst_flag`
#' adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl, adlb, worst_flag = c("WGRHIFL" = "Y"))
#'
#' # `h_adsl_adlb_merge_using_worst_flag` by visit example
#' adlb_out_by_visit <- h_adsl_adlb_merge_using_worst_flag(
#'   adsl,
#'   adlb,
#'   worst_flag = c("WGRLOVFL" = "Y"),
#'   by_visit = TRUE
#' )
#'
h_adsl_adlb_merge_using_worst_flag <- function( #nolint
  adsl,
  adlb,
  worst_flag = c("WGRHIFL" = "Y"),
  by_visit = FALSE,
  no_fillin_visits = c("SCREENING", "BASELINE")
) {
  col_names <- names(worst_flag)
  filter_values <- worst_flag

  temp <- Map(
    function(x, y) which(adlb[[x]] == y),
    col_names,
    filter_values
  )

  position_satisfy_filters <- Reduce(intersect, temp)

  adsl_adlb_common_columns <- intersect(colnames(adsl), colnames(adlb))
  columns_from_adlb <- c("USUBJID", "PARAM", "PARAMCD", "AVISIT", "AVISITN", "ATOXGR", "BTOXGR")

  adlb_f <- adlb[position_satisfy_filters, ] %>%
    filter(!.data[["AVISIT"]] %in% no_fillin_visits) %>%
    select(all_of(columns_from_adlb))

  avisits_grid <- adlb %>%
    filter(!.data[["AVISIT"]] %in% no_fillin_visits) %>%
    pull(.data[["AVISIT"]]) %>%
    unique()

  if (by_visit) {
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      AVISIT = avisits_grid,
      PARAMCD = unique(adlb$PARAMCD)
    )

    adsl_lb <- adsl_lb %>%
      left_join(unique(adlb[c("AVISIT", "AVISITN")]), by = "AVISIT") %>%
      left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl_lb <- adsl %>%
      select(all_of(adsl_adlb_common_columns)) %>%
      merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "AVISIT", "AVISITN", "PARAMCD", "PARAM")

    adlb_btoxgr <- adlb %>%
      select(c("USUBJID", "PARAMCD", "BTOXGR")) %>%
      unique() %>%
      rename("BTOXGR_MAP" = "BTOXGR")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
      )
    adlb_out <- adlb_out %>%
      left_join(adlb_btoxgr, by = c("USUBJID", "PARAMCD")) %>%
      mutate(BTOXGR = BTOXGR_MAP) %>%
      select(-BTOXGR_MAP)

    adlb_var_labels <- c(var_labels(adlb[by_variables_from_adlb]),
                         var_labels(adlb[columns_from_adlb[! columns_from_adlb %in% by_variables_from_adlb]]),
                         var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
                         )

    }else{
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      PARAMCD = unique(adlb$PARAMCD)
      )

    adsl_lb <- adsl_lb %>% left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl_lb <- adsl %>%
      select(all_of(adsl_adlb_common_columns)) %>%
      merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "PARAMCD", "PARAM")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
    )

    adlb_var_labels <- c(var_labels(adlb[by_variables_from_adlb]),
                         var_labels(adlb[columns_from_adlb[! columns_from_adlb %in% by_variables_from_adlb]]),
                         var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
    )
  }

  adlb_out$ATOXGR <- as.factor(adlb_out$ATOXGR) #nolint
  adlb_out$BTOXGR <- as.factor(adlb_out$BTOXGR) #nolint

  adlb_out <- df_explicit_na(adlb_out)
  var_labels(adlb_out) <- adlb_var_labels

  adlb_out
}
