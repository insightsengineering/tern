#' Sort Data by `PK PARAM` Variable
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param pk_data (`data.frame`)\cr `Pharmacokinetics` dataframe
#' @param key_var (`character`)\cr key variable used to merge pk_data and metadata created by `d_pkparam()`
#'
#' @return A PK `data.frame` sorted by a `PARAM` variable.
#'
#' @examples
#' library(dplyr)
#'
#' adpp <- tern_ex_adpp %>% mutate(PKPARAM = factor(paste0(PARAM, " (", AVALU, ")")))
#' pk_ordered_data <- h_pkparam_sort(adpp)
#'
#' @export
h_pkparam_sort <- function(pk_data, key_var = "PARAMCD") {
  assert_df_with_variables(pk_data, list(key_var = key_var))
  pk_data$PARAMCD <- pk_data[[key_var]]

  ordered_pk_data <- d_pkparam()

  # Add the numeric values from ordered_pk_data to pk_data
  joined_data <- merge(pk_data, ordered_pk_data, by = "PARAMCD", suffix = c("", ".y"))

  joined_data <- joined_data[, -grep(".*.y$", colnames(joined_data))]

  joined_data$TLG_ORDER <- as.numeric(joined_data$TLG_ORDER)

  # Then order PARAM based on this column
  joined_data$PARAM <- factor(joined_data$PARAM,
    levels = unique(joined_data$PARAM[order(joined_data$TLG_ORDER)]),
    ordered = TRUE
  )

  joined_data$TLG_DISPLAY <- factor(joined_data$TLG_DISPLAY,
    levels = unique(joined_data$TLG_DISPLAY[order(joined_data$TLG_ORDER)]),
    ordered = TRUE
  )

  joined_data
}
