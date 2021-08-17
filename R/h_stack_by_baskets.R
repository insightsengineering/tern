#' Helper Function to create a new `SMQ` variable in ADAE by stacking
#' `SMQ` and/or `CQ` records.
#'
#' Helper Function to create a new `SMQ` variable in ADAE that
#' consists of all adverse events belonging to selected
#' Standardized/Customized queries.
#' The new dataset will only contain records of the adverse events
#' belonging to any of the selected baskets.
#'
#' @inheritParams argument_convention
#' @param baskets (`character`)\cr variable names of the selected Standardized/Customized queries.
#' @param smq_varlabel (`string`)\cr a label for the new variable created.
#' @param keys (`character`)\cr names of the key variables to be returned
#' along with the new variable created.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' adae <- synthetic_cdisc_data("rcd_2021_05_05")$adae[1:20 , ]
#' h_stack_by_baskets(df = adae)
#'
h_stack_by_baskets <- function(df,
                               baskets = grep("^(SMQ|CQ).+NAM$", names(df), value = TRUE),
                               smq_varlabel = "Standardized MedDRA Query",
                               keys = c("STUDYID", "USUBJID", "ASTDTM", "AESEQ", "AETERM")) {

  smq_nam <- baskets[startsWith(baskets, "SMQ")]
  # SC corresponding to NAM
  smq_sc <- gsub(pattern = "NAM", replacement = "SC", x = smq_nam, fixed = TRUE)
  smq <- setNames(smq_sc, smq_nam)

  assert_that(
    is.character(baskets),
    is.string(smq_varlabel),
    is.data.frame(df),
    all(startsWith(baskets, "SMQ") | startsWith(baskets, "CQ")),
    all(endsWith(baskets, "NAM")),
    all(baskets %in% names(df)),
    all(keys %in% names(df)),
    all(smq_sc %in% names(df))
  )

  # Concatenate SMQxxxNAM with corresponding SMQxxxSC
  df_cnct <- df[, c(keys, baskets[startsWith(baskets, "CQ")])]

  for (nam in names(smq)) {

    sc <- smq[nam] # SMQxxxSC corresponding to SMQxxxNAM
    nam_notna <- !is.na(df[[nam]])
    new_colname <- paste(nam, sc, sep = "_")
    df_cnct[nam_notna, new_colname] <- paste0(df[[nam]], "(", df[[sc]], ")")[nam_notna]

  }

  # Transform from wide to long format
  df_long <- tidyr::pivot_longer(
    data = df_cnct, cols = -keys, names_to = NULL, values_to = "SMQ", values_drop_na = TRUE
  )

  var_labels(df_long[, "SMQ"]) <- smq_varlabel

  df_long

}
