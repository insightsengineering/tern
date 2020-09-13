
#' Dummy function to use currently unused functions
#'
#' to delete eventually. The function was introduced in the refactor.
#'
#' @param ... dummy arguments
#'
#' @export
use_stuff <- function(...) {
  capitalize(...)
  check_data_frame(...)
  check_strata(...)
  count_n(...)
  count_perc_col_N(...)
  explicit_special(...)
  iqr_num3(...)
  mean_sd(...)
  mean_sd3(...)
  median_t3(...)
  n_not_na3(...)
  positives_and_proportion(...)
  range_t3(...)
  start_with_null(...)
  trunc_if_longer(...)
  has_special_strata(...)
}

#' Dummy Statistics Function
#'
#' @param x numbers
#'
#' @export
#'
#' @examples
#' s_dummy_sum(c(1, 2))
#'
s_dummy_sum <- function(x) {
  list(
    sum = sum(x, na.rm = TRUE)
  )
}


#' Dummy Analysis Function
#'
#' @param lyt layout
#' @param vars vector of variable names
#'
#' @export
#'
#' @examples
#'
#' basic_table() %>%
#'   split_cols_by("Species") %>%
#'   dummy_sum("Sepal.Length") %>%
#'   build_table(iris)
#'
#' basic_table() %>%
#'   split_cols_by("Species") %>%
#'   dummy_sum(c("Sepal.Length", "Petal.Length")) %>%
#'   build_table(iris)
#'
dummy_sum <- function(lyt, vars) {
  analyze(lyt, vars, afun = function(x, .var) {
    in_rows(.list = s_dummy_sum(x), .labels = paste("sum of", .var))
  }, show_labels = "hidden")
}
