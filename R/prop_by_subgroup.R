#' Helper Functions for Tabulating Binary Response by Subgroup
#'
#' Helper functions that extract statistics such as response rate and odds ratio
#' for population subgroups.
#'
#' @inheritParams argument_convention
#' @name helpers_prop_by_subgroup
#' @order 1
#' @examples
#'
#' # Testing dataset.
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adrs <- radrs(cached = TRUE)
#' adrs_labels <- var_labels(adrs)
#'
#' adrs <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = forcats::fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' var_labels(adrs) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn helpers_prop_by_subgroup Helper to prepare a data frame of binary responses by arm.
#' @param arm (`factor`)\cr the treatment group variable.
#' @export
#' @examples
#'
#' h_proportion_df(
#'   c(TRUE, FALSE, FALSE),
#'   arm = factor(c("A", "A", "B"), levels = c("A", "B"))
#' )
#'
h_proportion_df <- function(rsp, arm) {

  assert_that(
    is_logical_vector(rsp),
    is_valid_factor(arm),
    is_equal_length(rsp, arm)
  )

  lst_rsp <- split(rsp, arm)
  lst_results <- Map(function(x, arm) {

    s_prop <- s_proportion(x)
    data.frame(
      arm = arm,
      n = length(x),
      n_rsp = unname(s_prop$n_prop[1]),
      prop = unname(s_prop$n_prop[2]),
      stringsAsFactors = FALSE
    )

  }, lst_rsp, names(lst_rsp))

  df <- do.call(rbind, args = c(lst_results, make.row.names = FALSE))
  df$arm <- factor(df$arm, levels = levels(arm))
  df
}

#' @describeIn helpers_prop_by_subgroup Summarizes proportion of binary responses by arm and across subgroups
#'    in a data frame. `variables` corresponds to the names of variables found in `data`, passed as a named list and
#'    requires elements `rsp`, `arm` and `subgroups`.
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @export
#' @examples
#'
#' h_proportion_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs
#' )
#'
h_proportion_subgroups_df <- function(variables, data) {

  assert_that(
    is.character(variables$rsp),
    is.character(variables$arm),
    is.character(variables$subgroups),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_valid_factor(data[[variables$arm]]),
    are_equal(nlevels(data[[variables$arm]]), 2)
  )

  subgroup_labels <- var_labels(data[, variables$subgroups], fill = TRUE)

  l_subgroups <- lapply(variables$subgroups, function(grp_i){

    l_df <- lapply(split(data, data[[grp_i]]), function(x) {
      h_proportion_df(x[[variables$rsp]], x[[variables$arm]])
    })

    df <- do.call(rbind, args = c(l_df, make.row.names = FALSE))
    df$subgroup <- rep(names(l_df), each = 2)
    df$var <- grp_i
    df$var_label <- subgroup_labels[grp_i]

    df
  })

  do.call(rbind, args = c(l_subgroups, make.row.names = FALSE))

}
