#' Estimation of Proportions
#'
#' Estimate the proportion of responders within a studied population.
#'
#' @name estimate_proportions
#' @order 1
#'
NULL


#' @describeIn estimate_proportions the Wilson interval calls [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#' @export
#' @order 2
#' @examples
#' rsp <- c(
#'   TRUE, TRUE, TRUE, TRUE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' prop_wilson(rsp, conf_level = 0.9)
prop_wilson <- function(rsp, conf_level, correct = FALSE) {
  y <- stats::prop.test(
    sum(rsp),
    length(rsp),
    correct = correct,
    conf.level = conf_level
  )

  as.numeric(y$conf.int)
}


#' @describeIn estimate_proportions the Clopper-Pearson interval calls
#'   [stats::binom.test()]. Also referred to as the `exact` method.
#' @inheritParams argument_convention
#' @export
#' @order 2
#' @examples
#'
#' prop_clopper_pearson(rsp, conf_level = .95)
prop_clopper_pearson <- function(rsp,
                                 conf_level) {
  y <- stats::binom.test(
    x = sum(rsp),
    n = length(rsp),
    conf.level = conf_level
  )
  as.numeric(y$conf.int)
}

#' @describeIn estimate_proportions the Wald interval follows the usual
#'   textbook definition for a single proportion confidence interval using the
#'   normal approximation.
#' @inheritParams argument_convention
#' @param correct (`flag`)\cr apply continuity correction.
#' @order 2
#' @export
#' @examples
#'
#' prop_wald(rsp, conf_level = 0.95)
#' prop_wald(rsp, conf_level = 0.95, correct = TRUE)
prop_wald <- function(rsp, conf_level, correct = FALSE) {
  n <- length(rsp)
  p_hat <- mean(rsp)
  z <- stats::qnorm((1 + conf_level) / 2)
  q_hat <- 1 - p_hat
  correct <- if (correct) 1 / (2 * n) else 0

  err <- z * sqrt(p_hat * q_hat) / sqrt(n) + correct
  l_ci <- max(0, p_hat - err)
  u_ci <- min(1, p_hat + err)

  c(l_ci, u_ci)
}


#' @describeIn estimate_proportions the Agresti-Coull interval was created by
#'   Alan Agresti and Brent Coull and can be understood (for 95% CI) as adding
#'   two successes and two failures to the data, and then using the Wald
#'   formula to construct a CI.
#' @inheritParams argument_convention
#' @export
#' @order 2
#' @examples
#'
#' prop_agresti_coull(rsp, conf_level = 0.95)
prop_agresti_coull <- function(rsp, conf_level) {
  n <- length(rsp)
  x_sum <- sum(rsp)
  z <- stats::qnorm((1 + conf_level) / 2)

  # Add here both z^2 / 2 successes and failures.
  x_sum_tilde <- x_sum + z^2 / 2
  n_tilde <- n + z^2

  # Then proceed as with the Wald interval.
  p_tilde <- x_sum_tilde / n_tilde
  q_tilde <- 1 - p_tilde
  err <- z * sqrt(p_tilde * q_tilde) / sqrt(n_tilde)
  l_ci <- max(0, p_tilde - err)
  u_ci <- min(1, p_tilde + err)

  c(l_ci, u_ci)
}


#' @describeIn estimate_proportions the Jeffreys interval is an equal-tailed
#'   interval based on the non-informative Jeffreys prior for a binomial
#'   proportion.
#' @inheritParams argument_convention
#' @order 2
#' @export
#' @examples
#'
#' prop_jeffreys(rsp, conf_level = 0.95)
prop_jeffreys <- function(rsp,
                          conf_level) {
  n <- length(rsp)
  x_sum <- sum(rsp)

  alpha <- 1 - conf_level
  l_ci <- ifelse(
    x_sum == 0,
    0,
    stats::qbeta(alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  u_ci <- ifelse(
    x_sum == n,
    1,
    stats::qbeta(1 - alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  c(l_ci, u_ci)
}


#' @inheritParams argument_convention
#' @param x (`logical`)\cr whether each subject is a responder or not.
#' `TRUE` represents a successful outcome.
#' @param method (`string`) \cr
#'   the method used to construct the confidence interval for proportion of
#'   successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`, `wilson`,
#'   `agresti-coull` or `jeffreys`.
#' @param long (`flag`)\cr a long description is required.
#'
#' @describeIn estimate_proportions statistics function estimating a
#'   proportion along with its confidence interval.
#'
#' @export
#' @order 3
#' @examples
#' s_proportion(c(1, 0, 1, 0))
s_proportion <- function(x,
                         conf_level = 0.95,
                         method = c(
                           "waldcc", "wald", "clopper-pearson",
                           "wilson", "wilsonc", "agresti-coull", "jeffreys"
                         ),
                         long = FALSE) {
  x <- as.logical(x)

  method <- match.arg(method)
  assertthat::assert_that(
    conf_level >= 0,
    conf_level <= 1,
    assertthat::is.flag(long)
  )

  rsp <- x
  n <- sum(rsp)
  p_hat <- mean(rsp)

  prop_ci <- switch(method,
    "clopper-pearson" = prop_clopper_pearson(rsp, conf_level),
    wilson = prop_wilson(rsp, conf_level),
    wilsonc = prop_wilson(rsp, conf_level, correct = TRUE),
    wald = prop_wald(rsp, conf_level),
    waldcc = prop_wald(rsp, conf_level, correct = TRUE),
    "agresti-coull" = prop_agresti_coull(rsp, conf_level),
    jeffreys = prop_jeffreys(rsp, conf_level)
  )

  list(
    "n_prop" = formatters::with_label(c(n, p_hat), "Responders"),
    "prop_ci" = formatters::with_label(
      x = 100 * prop_ci, label = d_proportion(conf_level, method, long = long)
    )
  )
}

#' @describeIn estimate_proportions Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_proportion(c(1, 0, 1, 0))
a_proportion <- make_afun(
  s_proportion,
  .formats = c(n_prop = "xx (xx.x%)", prop_ci = "(xx.x, xx.x)")
)

#' @inheritParams rtables::analyze
#' @param ... other arguments are ultimately conveyed to [s_proportion()].
#' @export
#' @describeIn estimate_proportions used in a `rtables` pipeline.
#' @order 4
#'
#' @examples
#' dta_test <- data.frame(
#'   USUBJID = paste0("S", 1:12),
#'   ARM     = rep(LETTERS[1:3], each = 4),
#'   AVAL    = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
#' )
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   estimate_proportion(vars = "AVAL") %>%
#'   build_table(df = dta_test)
estimate_proportion <- function(lyt,
                                vars,
                                ...,
                                show_labels = "hidden",
                                table_names = vars,
                                .stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  afun <- make_afun(
    a_proportion,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...),
    show_labels = show_labels,
    table_names = table_names
  )
}


#' Description of the Proportion Summary
#'
#' This is a helper function that describes the analysis in [s_proportion()].
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#' @param long (`flag`)\cr Whether a long or a short (default) description is required.
#'
#' @return String describing the analysis.
#' @keywords internal
#'
d_proportion <- function(conf_level,
                         method,
                         long = FALSE) {
  label <- paste0(conf_level * 100, "% CI")

  if (long) label <- paste(label, "for Response Rates")

  method_part <- switch(method,
    "clopper-pearson" = "Clopper-Pearson",
    "waldcc" = "Wald, with correction",
    "wald" = "Wald, without correction",
    "wilson" = "Wilson, without correction",
    "wilsonc" = "Wilson, with correction",
    "agresti-coull" = "Agresti-Coull",
    "jeffreys" = "Jeffreys",
    stop(paste(method, "does not have a description"))
  )

  paste0(label, " (", method_part, ")")
}
