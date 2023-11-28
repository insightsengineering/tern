#' Estimation of Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the proportion of responders within a studied population.
#'
#' @inheritParams prop_strat_wilson
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("estimate_proportion")`
#'   to see available statistics for this function.
#' @param method (`string`)\cr the method used to construct the confidence interval
#'   for proportion of successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`,
#'   `wilson`, `wilsonc`, `strat_wilson`, `strat_wilsonc`, `agresti-coull` or `jeffreys`.
#' @param long (`flag`)\cr a long description is required.
#'
#' @seealso [h_proportions]
#'
#' @name estimate_proportions
#' @order 1
NULL

#' @describeIn estimate_proportions Statistics function estimating a
#'   proportion along with its confidence interval.
#'
#' @param df (`logical` or `data.frame`)\cr if only a logical vector is used,
#'   it indicates whether each subject is a responder or not. `TRUE` represents
#'   a successful outcome. If a `data.frame` is provided, also the `strata` variable
#'   names must be provided in `variables` as a list element with the strata strings.
#'   In the case of `data.frame`, the logical vector of responses must be indicated as a
#'   variable name in `.var`.
#'
#' @return
#' * `s_proportion()` returns statistics `n_prop` (`n` and proportion) and `prop_ci` (proportion CI) for a
#'   given variable.
#'
#' @examples
#' # Case with only logical vector.
#' rsp_v <- c(1, 0, 1, 0, 1, 1, 0, 0)
#' s_proportion(rsp_v)
#'
#' # Example for Stratified Wilson CI
#' nex <- 100 # Number of example rows
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' s_proportion(
#'   df = dta,
#'   .var = "rsp",
#'   variables = list(strata = c("f1", "f2")),
#'   conf_level = 0.90,
#'   method = "strat_wilson"
#' )
#'
#' @export
s_proportion <- function(df,
                         .var,
                         conf_level = 0.95,
                         method = c(
                           "waldcc", "wald", "clopper-pearson",
                           "wilson", "wilsonc", "strat_wilson", "strat_wilsonc",
                           "agresti-coull", "jeffreys"
                         ),
                         weights = NULL,
                         max_iterations = 50,
                         variables = list(strata = NULL),
                         long = FALSE) {
  method <- match.arg(method)
  checkmate::assert_flag(long)
  assert_proportion_value(conf_level)

  if (!is.null(variables$strata)) {
    # Checks for strata
    if (missing(df)) stop("When doing stratified analysis a data.frame with specific columns is needed.")
    strata_colnames <- variables$strata
    checkmate::assert_character(strata_colnames, null.ok = FALSE)
    strata_vars <- stats::setNames(as.list(strata_colnames), strata_colnames)
    assert_df_with_variables(df, strata_vars)

    strata <- interaction(df[strata_colnames])
    strata <- as.factor(strata)

    # Pushing down checks to prop_strat_wilson
  } else if (checkmate::test_subset(method, c("strat_wilson", "strat_wilsonc"))) {
    stop("To use stratified methods you need to specify the strata variables.")
  }
  if (checkmate::test_atomic_vector(df)) {
    rsp <- as.logical(df)
  } else {
    rsp <- as.logical(df[[.var]])
  }
  n <- sum(rsp)
  p_hat <- mean(rsp)

  prop_ci <- switch(method,
    "clopper-pearson" = prop_clopper_pearson(rsp, conf_level),
    "wilson" = prop_wilson(rsp, conf_level),
    "wilsonc" = prop_wilson(rsp, conf_level, correct = TRUE),
    "strat_wilson" = prop_strat_wilson(rsp,
      strata,
      weights,
      conf_level,
      max_iterations,
      correct = FALSE
    )$conf_int,
    "strat_wilsonc" = prop_strat_wilson(rsp,
      strata,
      weights,
      conf_level,
      max_iterations,
      correct = TRUE
    )$conf_int,
    "wald" = prop_wald(rsp, conf_level),
    "waldcc" = prop_wald(rsp, conf_level, correct = TRUE),
    "agresti-coull" = prop_agresti_coull(rsp, conf_level),
    "jeffreys" = prop_jeffreys(rsp, conf_level)
  )

  list(
    "n_prop" = formatters::with_label(c(n, p_hat), "Responders"),
    "prop_ci" = formatters::with_label(
      x = 100 * prop_ci, label = d_proportion(conf_level, method, long = long)
    )
  )
}

#' @describeIn estimate_proportions Formatted analysis function which is used as `afun`
#'   in `estimate_proportion()`.
#'
#' @return
#' * `a_proportion()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @export
a_proportion <- make_afun(
  s_proportion,
  .formats = c(n_prop = "xx (xx.x%)", prop_ci = "(xx.x, xx.x)")
)

#' @describeIn estimate_proportions Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `estimate_proportion()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_proportion()` to the table layout.
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
#'
#' @export
#' @order 2
estimate_proportion <- function(lyt,
                                vars,
                                conf_level = 0.95,
                                method = c(
                                  "waldcc", "wald", "clopper-pearson",
                                  "wilson", "wilsonc", "strat_wilson", "strat_wilsonc",
                                  "agresti-coull", "jeffreys"
                                ),
                                weights = NULL,
                                max_iterations = 50,
                                variables = list(strata = NULL),
                                long = FALSE,
                                na_str = default_na_str(),
                                nested = TRUE,
                                ...,
                                show_labels = "hidden",
                                table_names = vars,
                                .stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  extra_args <- list(
    conf_level = conf_level, method = method, weights = weights, max_iterations = max_iterations,
    variables = variables, long = long, ...
  )

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
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' Helper Functions for Calculating Proportion Confidence Intervals
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to calculate different proportion confidence intervals for use in [estimate_proportion()].
#'
#' @inheritParams argument_convention
#' @inheritParams estimate_proportions
#'
#' @return Confidence interval of a proportion.
#'
#' @seealso [estimate_proportions], descriptive function [d_proportion()],
#'  and helper functions [strata_normal_quantile()] and [update_weights_strat_wilson()].
#'
#' @name h_proportions
NULL

#' @describeIn h_proportions Calculates the Wilson interval by calling [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#'
#' @examples
#' rsp <- c(
#'   TRUE, TRUE, TRUE, TRUE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' prop_wilson(rsp, conf_level = 0.9)
#'
#' @export
prop_wilson <- function(rsp, conf_level, correct = FALSE) {
  y <- stats::prop.test(
    sum(rsp),
    length(rsp),
    correct = correct,
    conf.level = conf_level
  )

  as.numeric(y$conf.int)
}

#' @describeIn h_proportions Calculates the stratified Wilson confidence
#'   interval for unequal proportions as described in \insertCite{Yan2010-jt;textual}{tern}
#'
#' @param strata (`factor`)\cr variable with one level per stratum and same length as `rsp`.
#' @param weights (`numeric` or `NULL`)\cr weights for each level of the strata. If `NULL`, they are
#'   estimated using the iterative algorithm proposed in \insertCite{Yan2010-jt;textual}{tern} that
#'   minimizes the weighted squared length of the confidence interval.
#' @param max_iterations (`count`)\cr maximum number of iterations for the iterative procedure used
#'   to find estimates of optimal weights.
#' @param correct (`flag`)\cr include the continuity correction. For further information, see for example
#'   [stats::prop.test()].
#'
#' @references
#' \insertRef{Yan2010-jt}{tern}
#'
#' @examples
#' # Stratified Wilson confidence interval with unequal probabilities
#'
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' strata <- interaction(strata_data)
#' n_strata <- ncol(table(rsp, strata)) # Number of strata
#'
#' prop_strat_wilson(
#'   rsp = rsp, strata = strata,
#'   conf_level = 0.90
#' )
#'
#' # Not automatic setting of weights
#' prop_strat_wilson(
#'   rsp = rsp, strata = strata,
#'   weights = rep(1 / n_strata, n_strata),
#'   conf_level = 0.90
#' )
#'
#' @export
prop_strat_wilson <- function(rsp,
                              strata,
                              weights = NULL,
                              conf_level = 0.95,
                              max_iterations = NULL,
                              correct = FALSE) {
  checkmate::assert_logical(rsp, any.missing = FALSE)
  checkmate::assert_factor(strata, len = length(rsp))
  assert_proportion_value(conf_level)

  tbl <- table(rsp, strata)
  n_strata <- length(unique(strata))

  # Checking the weights and maximum number of iterations.
  do_iter <- FALSE
  if (is.null(weights)) {
    weights <- rep(1 / n_strata, n_strata) # Initialization for iterative procedure
    do_iter <- TRUE

    # Iteration parameters
    if (is.null(max_iterations)) max_iterations <- 10
    checkmate::assert_int(max_iterations, na.ok = FALSE, null.ok = FALSE, lower = 1)
  }
  checkmate::assert_numeric(weights, lower = 0, upper = 1, any.missing = FALSE, len = n_strata)
  sum_weights <- checkmate::assert_int(sum(weights))
  if (as.integer(sum_weights + 0.5) != 1L) stop("Sum of weights must be 1L.")


  xs <- tbl["TRUE", ]
  ns <- colSums(tbl)
  use_stratum <- (ns > 0)
  ns <- ns[use_stratum]
  xs <- xs[use_stratum]
  ests <- xs / ns
  vars <- ests * (1 - ests) / ns

  strata_qnorm <- strata_normal_quantile(vars, weights, conf_level)

  # Iterative setting of weights if they were not set externally
  weights_new <- if (do_iter) {
    update_weights_strat_wilson(vars, strata_qnorm, weights, ns, max_iterations, conf_level)$weights
  } else {
    weights
  }

  strata_conf_level <- 2 * stats::pnorm(strata_qnorm) - 1

  ci_by_strata <- Map(
    function(x, n) {
      # Classic Wilson's confidence interval
      suppressWarnings(stats::prop.test(x, n, correct = correct, conf.level = strata_conf_level)$conf.int)
    },
    x = xs,
    n = ns
  )
  lower_by_strata <- sapply(ci_by_strata, "[", 1L)
  upper_by_strata <- sapply(ci_by_strata, "[", 2L)

  lower <- sum(weights_new * lower_by_strata)
  upper <- sum(weights_new * upper_by_strata)

  # Return values
  if (do_iter) {
    list(
      conf_int = c(
        lower = lower,
        upper = upper
      ),
      weights = weights_new
    )
  } else {
    list(
      conf_int = c(
        lower = lower,
        upper = upper
      )
    )
  }
}

#' @describeIn h_proportions Calculates the Clopper-Pearson interval by calling [stats::binom.test()].
#'   Also referred to as the `exact` method.
#'
#' @examples
#' prop_clopper_pearson(rsp, conf_level = .95)
#'
#' @export
prop_clopper_pearson <- function(rsp,
                                 conf_level) {
  y <- stats::binom.test(
    x = sum(rsp),
    n = length(rsp),
    conf.level = conf_level
  )
  as.numeric(y$conf.int)
}

#' @describeIn h_proportions Calculates the Wald interval by following the usual textbook definition
#'   for a single proportion confidence interval using the normal approximation.
#'
#' @param correct (`flag`)\cr apply continuity correction.
#'
#' @examples
#' prop_wald(rsp, conf_level = 0.95)
#' prop_wald(rsp, conf_level = 0.95, correct = TRUE)
#'
#' @export
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

#' @describeIn h_proportions Calculates the `Agresti-Coull` interval (created by `Alan Agresti` and `Brent Coull`) by
#'   (for 95% CI) adding two successes and two failures to the data and then using the Wald formula to construct a CI.
#'
#' @examples
#' prop_agresti_coull(rsp, conf_level = 0.95)
#'
#' @export
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

#' @describeIn h_proportions Calculates the Jeffreys interval, an equal-tailed interval based on the
#'   non-informative Jeffreys prior for a binomial proportion.
#'
#' @examples
#' prop_jeffreys(rsp, conf_level = 0.95)
#'
#' @export
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

#' Description of the Proportion Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a helper function that describes the analysis in [s_proportion()].
#'
#' @inheritParams s_proportion
#' @param long (`flag`)\cr whether a long or a short (default) description is required.
#'
#' @return String describing the analysis.
#'
#' @export
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
    "strat_wilson" = "Stratified Wilson, without correction",
    "wilsonc" = "Wilson, with correction",
    "strat_wilsonc" = "Stratified Wilson, with correction",
    "agresti-coull" = "Agresti-Coull",
    "jeffreys" = "Jeffreys",
    stop(paste(method, "does not have a description"))
  )

  paste0(label, " (", method_part, ")")
}

#' Helper Function for the Estimation of Stratified Quantiles
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function wraps the estimation of stratified percentiles when we assume
#' the approximation for large numbers. This is necessary only in the case
#' proportions for each strata are unequal.
#'
#' @inheritParams argument_convention
#' @inheritParams prop_strat_wilson
#'
#' @return Stratified quantile.
#'
#' @seealso [prop_strat_wilson()]
#'
#' @examples
#' strata_data <- table(data.frame(
#'   "f1" = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' ))
#' ns <- colSums(strata_data)
#' ests <- strata_data["TRUE", ] / ns
#' vars <- ests * (1 - ests) / ns
#' weights <- rep(1 / length(ns), length(ns))
#'
#' strata_normal_quantile(vars, weights, 0.95)
#'
#' @export
strata_normal_quantile <- function(vars, weights, conf_level) {
  summands <- weights^2 * vars
  # Stratified quantile
  sqrt(sum(summands)) / sum(sqrt(summands)) * stats::qnorm((1 + conf_level) / 2)
}

#' Helper Function for the Estimation of Weights for `prop_strat_wilson`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function wraps the iteration procedure that allows you to estimate
#' the weights for each proportional strata. This assumes to minimize the
#' weighted squared length of the confidence interval.
#'
#' @inheritParams prop_strat_wilson
#' @param vars (`numeric`)\cr normalized proportions for each strata.
#' @param strata_qnorm (`numeric`)\cr initial estimation with identical weights of the quantiles.
#' @param initial_weights (`numeric`)\cr initial weights used to calculate `strata_qnorm`. This can
#'   be optimized in the future if we need to estimate better initial weights.
#' @param n_per_strata (`numeric`)\cr number of elements in each strata.
#' @param max_iterations (`count`)\cr maximum number of iterations to be tried. Convergence is always checked.
#' @param tol (`number`)\cr tolerance threshold for convergence.
#'
#' @return A `list` of 3 elements: `n_it`, `weights`, and `diff_v`.
#'
#' @seealso For references and details see [prop_strat_wilson()].
#'
#' @examples
#' vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
#' sq <- 0.674
#' ws <- rep(1 / length(vs), length(vs))
#' ns <- c(22, 18, 17, 17, 14, 12)
#'
#' update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)
#'
#' @export
update_weights_strat_wilson <- function(vars,
                                        strata_qnorm,
                                        initial_weights,
                                        n_per_strata,
                                        max_iterations = 50,
                                        conf_level = 0.95,
                                        tol = 0.001) {
  it <- 0
  diff_v <- NULL

  while (it < max_iterations) {
    it <- it + 1
    weights_new_t <- (1 + strata_qnorm^2 / n_per_strata)^2
    weights_new_b <- (vars + strata_qnorm^2 / (4 * n_per_strata^2))
    weights_new <- weights_new_t / weights_new_b
    weights_new <- weights_new / sum(weights_new)
    strata_qnorm <- strata_normal_quantile(vars, weights_new, conf_level)
    diff_v <- c(diff_v, sum(abs(weights_new - initial_weights)))
    if (diff_v[length(diff_v)] < tol) break
    initial_weights <- weights_new
  }

  if (it == max_iterations) {
    warning("The heuristic to find weights did not converge with max_iterations = ", max_iterations)
  }

  list(
    "n_it" = it,
    "weights" = weights_new,
    "diff_v" = diff_v
  )
}
