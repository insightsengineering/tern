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
#' @importFrom data.table melt
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

  #Matching NAM SC pairs
  names_smq <- baskets[startsWith(baskets, "SMQ")]
  names_smq_concat <- lapply(names_smq, FUN = function(name_smq) {
    name_sc <- str_replace(name_smq, "NAM", "SC")
    name_smq_concat <- c(name_smq, name_sc)
  })

  #Concatenating NAM SC columns and adding them into the DF
  df_anl <- df[, c(keys, baskets, str_replace(names_smq, "NAM", "SC"))]
  df_keys_cq <- df[, c(keys, baskets[startsWith(baskets, "CQ")])]
  df_keys_cq_smq_concat <- df_keys_cq

  for (names in names_smq_concat){
    name_smq_nam_tmp <- names[1]
    name_smq_sc_tmp <- names[2]
    smq_nam_tmp <- df_anl[[name_smq_nam_tmp]]
    smq_sc_tmp <- df_anl[[name_smq_sc_tmp]]
    nam_sc_concat_tmp <- rep(NA, length(smq_nam_tmp))
    nam_sc_concat_all_tmp <- paste0(smq_nam_tmp,"(", smq_sc_tmp, ")")
    pos_not_na_tmp <- which(!is.na(smq_nam_tmp))
    nam_sc_concat_tmp[pos_not_na_tmp] <- nam_sc_concat_all_tmp[pos_not_na_tmp]
    col_name_tmp <- paste0(names[1],"_",names[2])
    df_keys_cq_smq_concat[col_name_tmp] <- nam_sc_concat_tmp
  }

  #Transforming DF from wide to long format
  df_long <- tibble(
    data.table::melt(data.table::setDT(df_keys_cq_smq_concat), id.vars = keys)
    )

  result <- tibble(df_long[!is.na(df_long$value), c(keys,"value")])
  names(result) <- c(keys, "SMQ")
  var_labels(result) <- c(var_labels(result[names(result) %in% keys]), smq_varlabel)

  result
}

