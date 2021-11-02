#' Helper Function to create a map dataframe that can be used in `trim_levels_to_map` split function.
#'
#' Helper Function to create a map dataframe from the input dataset, which can be used as an argument in the
#' `trim_levels_to_map` split function. Based on different method, the map is constructed differently.
#'
#' @inheritParams argument_convention
#' @param abnormal (`named list`)\cr identifying the abnormal range level(s) in `df`. Default to
#' `list(Low = "LOW LOW", High = "HIGH HIGH")` but you can also group different levels into the name list, for example,
#' `abnormal = list(Low = "LOW", High = "HIGH"))`
#' @param method (`string`)\cr indicates how the returned map will be constructed. Can be either `"default"` or
#' `"range"`.
#' If method is `"default"`, the returned map will only have the abnormals that are observed in the `df`, and
#' records with all normals will be excluded to avoid error in creating layout.
#' If method is `"range"`, the returned map will be based on the rule that at least one observation with ANRLO > 0 for
#' low direction and at least one observation with ANRHI is not missing for high direction.
#'
#' @export
#'
#'
#' @examples
#'
#' library(scda)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#'
h_map_for_count_abnormal <- function(
  df,
  variables = list(anl = "ANRIND", split_rows = c("PARAM"), range_low = "ANRLO", range_high = "ANRHI"),
  abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
  method = c("default", "range"),
  na_level = "<Missing>"
) {
  method <- match.arg(method)
  assert_that(
  )
}
