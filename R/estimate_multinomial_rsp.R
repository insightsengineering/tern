#' Estimation of Proportions per Level of Factor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the proportion along with confidence interval of a proportion
#' regarding the level of a factor.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("estimate_multinomial_response")`
#'   to see available statistics for this function.
#'
#' @seealso Relevant description function [d_onco_rsp_label()].
#'
#' @name estimate_multinomial_rsp
#' @order 1
NULL

#' Description of Standard Oncology Response
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Describe the oncology response in a standard way.
#'
#' @param x (`character`)\cr the standard oncology code to be described.
#'
#' @return Response labels.
#'
#' @seealso [estimate_multinomial_rsp()]
#'
#' @examples
#' d_onco_rsp_label(
#'   c("CR", "PR", "SD", "NON CR/PD", "PD", "NE", "Missing", "<Missing>", "NE/Missing")
#' )
#'
#' # Adding some values not considered in d_onco_rsp_label
#'
#' d_onco_rsp_label(
#'   c("CR", "PR", "hello", "hi")
#' )
#'
#' @export
d_onco_rsp_label <- function(x) {
  x <- as.character(x)
  desc <- c(
    CR           = "Complete Response (CR)",
    PR           = "Partial Response (PR)",
    MR           = "Minimal/Minor Response (MR)",
    MRD          = "Minimal Residual Disease (MRD)",
    SD           = "Stable Disease (SD)",
    PD           = "Progressive Disease (PD)",
    `NON CR/PD`  = "Non-CR or Non-PD (NON CR/PD)",
    NE           = "Not Evaluable (NE)",
    `NE/Missing` = "Missing or unevaluable",
    Missing      = "Missing",
    `NA`         = "Not Applicable (NA)",
    ND           = "Not Done (ND)"
  )

  values_label <- vapply(
    X = x,
    FUN.VALUE = character(1),
    function(val) {
      if (val %in% names(desc)) desc[val] else val
    }
  )

  return(factor(values_label, levels = c(intersect(desc, values_label), setdiff(values_label, desc))))
}

#' @describeIn estimate_multinomial_rsp Statistics function which feeds the length of `x` as number
#'   of successes, and `.N_col` as total number of successes and failures into [s_proportion()].
#'
#' @return
#' * `s_length_proportion()` returns statistics from [s_proportion()].
#'
#' @examples
#' s_length_proportion(rep("CR", 10), .N_col = 100)
#' s_length_proportion(factor(character(0)), .N_col = 100)
#'
#' @export
s_length_proportion <- function(x,
                                .N_col, # nolint
                                ...) {
  checkmate::assert_multi_class(x, classes = c("factor", "character"))
  checkmate::assert_vector(x, min.len = 0, max.len = .N_col)
  checkmate::assert_vector(unique(x), min.len = 0, max.len = 1)

  n_true <- length(x)
  n_false <- .N_col - n_true
  x_logical <- rep(c(TRUE, FALSE), c(n_true, n_false))
  s_proportion(df = x_logical, ...)
}

#' @describeIn estimate_multinomial_rsp Formatted analysis function which is used as `afun`
#'   in `estimate_multinomial_response()`.
#'
#' @return
#' * `a_length_proportion()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_length_proportion(rep("CR", 10), .N_col = 100)
#' a_length_proportion(factor(character(0)), .N_col = 100)
#'
#' @export
a_length_proportion <- make_afun(
  s_length_proportion,
  .formats = c(
    n_prop = "xx (xx.x%)",
    prop_ci = "(xx.xx, xx.xx)"
  )
)

#' @describeIn estimate_multinomial_rsp Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()] and
#'   [rtables::summarize_row_groups()].
#'
#' @return
#' * `estimate_multinomial_response()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_length_proportion()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' # Use of the layout creating function.
#' dta_test <- data.frame(
#'   USUBJID = paste0("S", 1:12),
#'   ARM     = factor(rep(LETTERS[1:3], each = 4)),
#'   AVAL    = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
#' ) %>% mutate(
#'   AVALC = factor(AVAL,
#'     levels = c(0, 1),
#'     labels = c("Complete Response (CR)", "Partial Response (PR)")
#'   )
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   estimate_multinomial_response(var = "AVALC")
#'
#' tbl <- build_table(lyt, dta_test)
#'
#' tbl
#'
#' @export
#' @order 2
estimate_multinomial_response <- function(lyt,
                                          var,
                                          na_str = default_na_str(),
                                          nested = TRUE,
                                          ...,
                                          show_labels = "hidden",
                                          table_names = var,
                                          .stats = "prop_ci",
                                          .formats = NULL,
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  extra_args <- list(...)

  afun <- make_afun(
    a_length_proportion,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  lyt <- split_rows_by(lyt, var = var)
  lyt <- summarize_row_groups(lyt, na_str = na_str)

  analyze(
    lyt,
    vars = var,
    afun = afun,
    show_labels = show_labels,
    table_names = table_names,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}
