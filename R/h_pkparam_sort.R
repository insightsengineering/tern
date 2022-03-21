#' Generate PK reference dataset
#' @describeIn Generates a dataframe of the correct order of the PK parameters
#' @return dataframe of PK parameters
#' @export
#'
#' @examples
#' pk_reference_dataset <- d_pkparam()

library(scda)
library(rtables)
library(dplyr)
library(tern)

adpp <- synthetic_cdisc_data("latest")$adpp
adpp <- adpp %>% mutate(PKPARAM = factor(paste0(PARAM, " (", AVALU, ")")))

adpp <- synthetic_cdisc_data("latest")$adpp
adpp <- adpp %>% mutate(PKPARAM = factor(paste0(PARAM, " (", AVALU, ")")))
pk_data <- adpp

h_pkparam_sort <- function(pk_data) {
  #ordered_pk_data <- d_pkparam()
  ordered_pk_data <- pk_dataset
  unq_param
  pk_data <- pk_data %>% mutate(PARAM = factor(PARAM, levels = ))

}
