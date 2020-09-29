#' Estimation of Proportions per Level of Factor
#'
#' Estimate the proportion along with confidence interval of a proportion
#' regarding the level of a factor.
#'
#' @template formatting_arguments
#' @order 1
#' @name estimate_multinomial_rsp
#'
NULL

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
#'   c("CR", "PR", "SD", "NON CR/PD", "PD", "NE", "Missing", "NE/Missing")
#' )
#'
d_onco_rsp_label <- function(x) { # nousage # nolint

  desc <- c(
    CR          = "Complete Response (CR)",
    PR          = "Partial Response (PR)",
    SD          = "Stable Disease (SD)",
    `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
    PD          = "Progressive Disease (PD)",
    NE          = "Not Evaluable (NE)",
    Missing     = "Missing",
    `NE/Missing` = "Missing or unevaluable"
  )

  assert_that(all(x %in% names(desc)))
  desc[x]

}


#' Helper Function: Proportion Estimations
#'
#' Helps in estimating the proportion for a given vector of responses.
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#' @param denom (`number`)\cr denominator for the responder proportions.
#' @param rsp_lab (`string`)\cr the label attribute for the responder proportion.
#'
h_prop_ci <- function(rsp,
                      denom = length(rsp),
                      method = c(
                        "waldcc", "wald", "clopper-pearson",
                        "wilson", "agresti-coull", "jeffreys"
                      ),
                      conf_level,
                      long = TRUE,
                      rsp_lab = "Responders") {

  method <- match.arg(method)
  assert_that(
    is.logical(rsp),
    is.number(denom),
    is_proportion(conf_level),
    is.flag(long),
    is.string(rsp_lab)
  )

  y <- list()
  n <- sum(rsp)
  y$n_prop <- with_label(x = c(n, n / denom), label = rsp_lab)

  prop_ci <- switch(
    method,
    "clopper-pearson" = prop_clopper_pearson(rsp, conf_level),
    wilson = prop_wilson(rsp, conf_level),
    wald = prop_wald(rsp, conf_level),
    waldcc = prop_wald(rsp, conf_level, correct = TRUE),
    "agresti-coull" = prop_agresti_coull(rsp, conf_level),
    jeffreys = prop_jeffreys(rsp, conf_level)
  )

  y$prop_ci <- with_label(
    x = prop_ci,
    label = d_proportion(conf_level, method, long = long)
  )

  y
}


#' @inheritParams argument_convention
#' @inheritParams s_proportion
#'
#' @describeIn estimate_multinomial_rsp statistics function estimating a
#'   proportion per factor level along with its confidence interval for the level.
#'
#' @export
#' @order 2
#' @examples
#'
#' # Basic use of the statistics function
#' s_multinomial_response(c(0, 1, 0, 1))
#'
s_multinomial_response <- function(x,
                                   conf_level = 0.95,
                                   method = c(
                                     "waldcc", "wald", "clopper-pearson",
                                     "wilson", "agresti-coull", "jeffreys"
                                   ),
                                   long = FALSE) {

  method <- match.arg(method)
  assert_that(is.atomic(x))

  denom <- length(x)
  x <- split(x, f = x)
  rsp_lab <- names(x)
  n <- lapply(x, length)
  rsp <- lapply(n, function(z) rep(c(TRUE, FALSE), c(z, denom - z)))

  y <- Map(
    rsp = rsp,
    denom = denom,
    method = method,
    conf_level = conf_level,
    long = long,
    rsp_lab = rsp_lab,
    f = h_prop_ci
  )

  flatten_list(y)
}

#' @inheritParams argument_convention
#' @param ... other arguments are ultimately conveyed to [s_multinomial_response()].
#' @export
#' @describeIn estimate_multinomial_rsp used in a `rtables` pipeline.
#' @order 3
#' @examples
#'
#' # Use of the layout creating function.
#' dta_test <- data.frame(
#'   USUBJID = paste0("S", 1:12),
#'   ARM     = rep(LETTERS[1:3], each = 4),
#'   AVAL    = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
#' )
#' dta_test$AVALC <- as.factor(c(
#'   "Complete Response (CR)", "Partial Response (PR)"
#' )[dta_test$AVAL + 1])
#'
#' lyt <- split_cols_by(lyt = NULL, var = "ARM") %>%
#'   estimate_multinomial_response(var = "AVALC")
#'
#' as_html(build_table(lyt = lyt, df = dta_test)) %>% Viewer
#'
estimate_multinomial_response <- function(lyt,
                                          var,
                                          vars = var,
                                          ...) {
  afun <- format_wrap_x(
    sfun = s_multinomial_response,
    formats =  c(n_prop = "xx (xx.xx%)", prop_ci = "(xx.xx, xx.xx)"),
    indent_mods = c(n_prop = 0L, prop_ci = 0L)
  )

  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...)
  )
}
