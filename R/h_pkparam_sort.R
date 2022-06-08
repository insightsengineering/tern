#' Sort `PK PARAM` variable
#' @param pk_data (`data.frame`)\cr Pharmacokinetics dataframe
#' @param key_var (`character`)\cr key varialbe used to merge pk_data and metadata created by `d_pkparam()`.
#' @return dataframe with `PK PARAM` variable ordered
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' adpp <- synthetic_cdisc_data("latest")$adpp
#' adpp <- adpp %>% mutate(PKPARAM = factor(paste0(PARAM, " (", AVALU, ")")))
#'
#' pk_ordered_data <- h_pkparam_sort(adpp)
h_pkparam_sort <- function(pk_data, key_var="PARAMCD") {
  ordered_pk_data <- d_pkparam()

  # Add the numeric values from ordered_pk_data to pk_data
  joined_data <- merge(pk_data, ordered_pk_data, by = key_var, suffix = c("", ".y"))

  joined_data <- joined_data[, -grep(".*.y$", colnames(joined_data))]

  joined_data$TLG_ORDER <- as.numeric(joined_data$TLG_ORDER) # nolint

  # Then order PARAM based on this column
  joined_data$PARAM <- factor(joined_data$PARAM, # nolint
    levels = unique(joined_data$PARAM[order(joined_data$TLG_ORDER)]),
    ordered = TRUE
  )

  joined_data$TLG_DISPLAY <- factor(joined_data$TLG_DISPLAY, # nolint
    levels = unique(joined_data$TLG_DISPLAY[order(joined_data$TLG_ORDER)]),
    ordered = TRUE
  )

  joined_data
}
