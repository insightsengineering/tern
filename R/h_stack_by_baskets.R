#' Helper Function to create a new `SMQ` variable in ADAE by stacking
#' `SMQ` and/or `CMQ` records.
#'
#' Helper Function to create a new `SMQ` variable in ADAE that
#' consists of all adverse events belonging to selected
#' Standardized/Customized queries.
#' The new dataset will only contain records of the adverse events
#' belonging to any of the selected baskets.
#'
#' @inheritParams argument_convention
#' @param baskets (`character`)\cr character vector specifying
#'  variable names of the selected Standardized/Customized
#' queries.
#' @param smq_varlabel (`string`)\cr string specifying
#'  the label of the new variable created.
#' @param keys (`character`)\cr character vector specifying
#'  names of the key variables to be returned along with
#'  the new variable created.
#'
#' @export
#'
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[ 1:20 , ]
#'
#' adae_smq <- h_stack_by_baskets(df = adae,
#' smq_varlabel = "Standardized MedDRA Query"
#' )
#'
h_stack_by_baskets <- function(df,
                               baskets = grep("^(SMQ|CQ).*NAM$", names(df), value = TRUE),
                               smq_varlabel = "Standardized MedDRA Query",
                               keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM")
) {

  assert_that(
    is.character(baskets),
    is.string(smq_varlabel),
    is.data.frame(df),
    all(startsWith(baskets, "SMQ") | startsWith(baskets, "CQ")),
    all(endsWith(baskets, "NAM")),
    all(baskets %in% names(df)),
    all(keys %in% names(df))
  )

  df_keys <- df[, names(df) %in% keys]
  df_keys_baskets <- df[, names(df) %in% c(keys, baskets)]

  smq_baskets <- baskets[startsWith(baskets, "SMQ")]
  smq_scopes <- str_replace(smq_baskets, "NAM", "SC")
  df_keys_baskets_scope <- tibble(df_keys_baskets, df[, c(smq_scopes)])
  names(df_keys_baskets_scope) <- c(names(df_keys_baskets), smq_scopes)

  result_labels <- c(var_labels(df_keys), smq_varlabel)

  df_long_list <- lapply(baskets, function(ae_grp) {
    keep <- !(is.na(df_keys_baskets_scope[[ae_grp]]))
    df_long <- df_keys_baskets_scope[keep, ]
    if (substr(ae_grp, 1, 3) == "SMQ") {
      ae_scope <- str_replace(ae_grp, "NAM", "SC")
      df_long[["SMQ"]] <- aesi_label(
        as.character(df_long[[ae_grp]]),
        scope = as.character(df_long[[ae_scope]])
      )
    } else {
      df_long[["SMQ"]] <- df_long[[ae_grp]]
    }
    df_long
  })
  result <- do.call(rbind, df_long_list)
  if (nrow(result) == 0) {
    SMQ <- "" #nolintr
    result <- tibble(result, SMQ)
  }
  result <- result[, names(result) %in% c(keys, "SMQ")]
  var_labels(result) <- result_labels
  result
}
