#' Estimation of Proportions per Level of Factor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the proportion along with confidence interval of a proportion
#' regarding the level of a factor.
#'
#' @name estimate_multinomial_rsp
#'
NULL

#' @describeIn estimate_multinomial_rsp
#' Standard Oncology Response
#'
#' Describe the oncology response in a standard way.
#'
#' @param x (`character`)\cr the standard oncology code to be described.
#' @export
#'
#' @examples
#'
#' d_onco_rsp_label(
#'   c("CR", "PR", "SD", "NON CR/PD", "PD", "NE", "Missing", "<Missing>", "NE/Missing")
#' )
#'
#' # Adding some values not considered in d_onco_rsp_label
#'
#' d_onco_rsp_label(
#'   c("CR", "PR", "hello", "hi")
#' )
d_onco_rsp_label <- function(x) { # nolint

  x <- as.character(x)
  desc <- c(
    CR           = "Complete Response (CR)",
    Missing      = "Missing",
    MR           = "Minimal/Minor Response (MR)",
    MRD          = "Minimal Residual Disease (MRD)",
    `NA`         = "Not Applicable (NA)",
    ND           = "Not Done (ND)",
    NE           = "Not Evaluable (NE)",
    `NE/Missing` = "Missing or unevaluable",
    `NON CR/PD`  = "Non-CR or Non-PD (NON CR/PD)",
    PD           = "Progressive Disease (PD)",
    PR           = "Partial Response (PR)",
    SD           = "Stable Disease (SD)"
  )

  values_label <- vapply(
    X = x,
    FUN.VALUE = character(1),
    function(val) {
      if (val %in% names(desc)) desc[val] else val
    }
  )

  return(factor(values_label))
}


#' @describeIn estimate_multinomial_rsp Statistics function which takes the length of the input `x` and takes that
#'   as the number of successes, and the column number `.N_col` as the total number, and feeds that into
#'   [s_proportion()].
#' @inheritParams argument_convention
#' @return See [s_proportion()] for statistics and additional possible arguments.
#'
#' @export
#'
#' @examples
#' s_length_proportion(rep("CR", 10), .N_col = 100)
#' s_length_proportion(factor(character(0)), .N_col = 100)
s_length_proportion <- function(x,
                                .N_col, # nolint snake_case
                                ...) {
  checkmate::assert_int(length(x), lower = 0, upper = .N_col)
  checkmate::assert_int(length(unique(x)), lower = 0, upper = 1)
  checkmate::assert_multi_class(x, classes = c("factor", "character"))

  n_true <- length(x)
  n_false <- .N_col - n_true
  x_logical <- rep(c(TRUE, FALSE), c(n_true, n_false))
  s_proportion(x = x_logical, ...)
}

#' @describeIn estimate_multinomial_rsp Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_length_proportion(rep("CR", 10), .N_col = 100)
#' a_length_proportion(factor(character(0)), .N_col = 100)
a_length_proportion <- make_afun(
  s_length_proportion,
  .formats = c(
    n_prop = "xx (xx.x%)",
    prop_ci = "(xx.xx, xx.xx)"
  )
)

#' @describeIn estimate_multinomial_rsp Analyze Function which adds the multinomial proportion analysis to
#'   the input layout. Note that additional formatting arguments can be used
#'   here.
#' @inheritParams argument_convention
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
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
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   estimate_multinomial_response(var = "AVALC")
#'
#' tbl <- build_table(lyt, dta_test)
#'
#' html <- as_html(tbl)
#' html
#' \dontrun{
#' Viewer(html)
#' }
estimate_multinomial_response <- function(lyt,
                                          var,
                                          ...,
                                          show_labels = "hidden",
                                          table_names = var,
                                          .stats = "prop_ci",
                                          .formats = NULL,
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  afun <- make_afun(
    a_length_proportion,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  lyt <- split_rows_by(lyt, var = var)
  lyt <- summarize_row_groups(lyt)

  analyze(
    lyt,
    vars = var,
    afun = afun,
    show_labels = show_labels,
    table_names = table_names,
    extra_args = list(...)
  )
}
