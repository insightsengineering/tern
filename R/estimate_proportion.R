#' Estimation of Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the proportion of responders within a studied population.
#'
#' @name estimate_proportions
#'
NULL


#' @describeIn estimate_proportions the Wilson interval calls [stats::prop.test()].
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

#' Helper function for the estimation of stratified quantiles
#'
#' @description
#'   This function wraps the estimation of stratified percentiles when we assume
#'   the approximation for large numbers. This is necessary only in the case
#'   proportions for each strata are unequal.
#'
#' @inheritParams prop_strat_wilson
#'
#' @seealso [prop_strat_wilson()]
#'
#' @examples
#' \dontrun{
#' strata_data <- table(data.frame(
#'   "f1" = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' ))
#' ns <- colSums(strata_data)
#' ests <- strata_data["TRUE", ] / ns
#' vars <- ests * (1 - ests) / ns
#' weights <- rep(1 / length(ns), length(ns))
#' strata_normal_quantile(vars, weights, 0.95)
#' }
#'
#' @keywords internal
strata_normal_quantile <- function(vars, weights, conf_level) {
  summands <- weights^2 * vars
  # Stratified quantile
  sqrt(sum(summands)) / sum(sqrt(summands)) * stats::qnorm((1 + conf_level) / 2)
}

#' Helper function for the estimation of weights for `prop_strat_wilson`
#'
#' @description
#'   This function wraps the iteration procedure that allows you to estimate
#'   the weights for each proportional strata. This assumes to minimize the
#'   weighted squared length of the confidence interval. See [prop_strat_wilson()]
#'   for references and details.
#'
#' @inheritParams prop_strat_wilson
#' @param vars (`vector` of `numeric`) \cr
#'   normalized proportions for each strata.
#' @param strata_qnorm (`numeric`) \cr
#'   initial estimation with identical weights of the quantiles.
#' @param ws_old (`vector` of `numeric`) \cr
#'   initial weights used to calculate `strata_qnorm`. This can be optimized in
#'   the future if we need to estimate better initial weights.
#' @param max_nit (`integer`) \cr
#'   maximum number of iterations to be tried. Convergence is always checked.
#' @param tol (`numeric`) \cr
#'   tolerance threshold for convergence.
#'
#' @seealso [prop_strat_wilson()]
#'
#' @examples
#' \dontrun{
#' vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
#' sq <- 0.674
#' ws <- rep(1 / length(vs), length(vs))
#' ns <- c(22, 18, 17, 17, 14, 12)
#'
#' update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)
#' }
#'
#' @keywords internal
update_weights_strat_wilson <- function(vars,
                                        strata_qnorm,
                                        ws_old,
                                        n_per_strata,
                                        max_nit = 50,
                                        conf_level = 0.95,
                                        tol = 0.001) {
  it <- 1
  diff_v <- NULL
  while (it <= max_nit) {
    ws_new_t <- (1 + strata_qnorm^2 / n_per_strata)^2
    ws_new_b <- (vars + strata_qnorm^2 / (4 * n_per_strata^2))
    ws_new <- ws_new_t / ws_new_b
    ws_new <- ws_new / sum(ws_new)
    strata_qnorm <- strata_normal_quantile(vars, ws_new, conf_level)
    diff_v <- c(diff_v, sum(abs(ws_new - ws_old)))
    if (diff_v[length(diff_v)] < tol) break
    ws_old <- ws_new
    it <- it + 1
  }
  return(list(
    "n_it" = it,
    "weights" = ws_new,
    "diff_v" = diff_v
  ))
}
#' @describeIn estimate_proportions Calculates the stratified Wilson confidence
#'   interval for unequal proportions as described in (`Yan` and `Su` 2010).
#'
#' @param strata (`factor`)\cr
#'   with one level per stratum and same length as `rsp`.
#' @param weights (`vector` of `numeric`) \cr
#'   weights for each level of the strata. If missing, they are
#'   estimated using the iterative algorithm proposed in (`Yan` and `Su` 2010)
#'   that minimizes the weighted squared length of the confidence interval.
#' @param max_nit (`integer`) \cr
#'   maximum number of iterations for the iterative procedure used
#'   to find estimates of optimal weights.
#' @param correct (`flag`)\cr
#'   include the continuity correction. For further information, see for example
#'   [stats::prop.test()].
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
#' n_strata <- ncol(table(rsp, strata)) # Number of weights or centers
#'
#' prop_strat_wilson(
#'   rsp = rsp, strata = strata,
#'   conf_level = 0.90
#' )
#'
#' prop_strat_wilson(
#'   rsp = rsp, strata = strata,
#'   weights = rep(1 / n_strata, n_strata), # Not automatic setting of weights
#'   conf_level = 0.90
#' )
#' @references
#' \itemize{
#'   \item Yan, Xin, and Xiao Gang Su. 2010. “Stratified Wilson and Newcombe Confidence Intervals for Multiple Binomial Proportions.” Statistics in Biopharmaceutical Research 2 (3): 329–35.
#' }
#'
#' @export
prop_strat_wilson <- function(rsp,
                              strata,
                              weights = NULL,
                              conf_level = 0.95,
                              max_nit = NULL,
                              correct = FALSE) {

  # Checking the inputs
  checkmate::assert_logical(rsp, any.missing = FALSE)
  checkmate::assert_factor(strata, len = length(rsp))
  assert_proportion_value(conf_level)

  tbl <- table(rsp, strata)
  n_strata <- ncol(tbl) # Number of centers and weights

  # Checking the weights and maximum number of iterations.
  do_iter <- FALSE
  if (is.null(weights)) {
    weights <- rep(1 / n_strata, n_strata) # Initialization for iterative procedure
    do_iter <- TRUE

    # Iteration parameters
    if (is.null(max_nit)) max_nit <- 10
    checkmate::assert_int(max_nit, na.ok = FALSE, null.ok = FALSE, lower = 1)
  }
  checkmate::assert_numeric(weights, lower = 0, upper = 1, any.missing = FALSE, len = ncol(tbl))
  checkmate::assert_int(sum(weights), lower = 1, upper = 1)


  xs <- tbl["TRUE", ]
  ns <- colSums(tbl)
  ests <- xs / ns
  vars <- ests * (1 - ests) / ns

  # Estimating quantile for normal distribution large number limit approximation
  strata_qnorm <- strata_normal_quantile(vars, weights, conf_level)

  # Iterative setting of weights if they were not set externally
  if (do_iter) {
    ws_new <- update_weights_strat_wilson(vars, strata_qnorm, weights, ns, max_nit, conf_level)$weights
  } else {
    ws_new <- weights
  }

  strata_conf_level <- 2 * pnorm(strata_qnorm) - 1

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

  lower <- sum(ws_new * lower_by_strata)
  upper <- sum(ws_new * upper_by_strata)

  # Return values
  if (do_iter) {
    list(
      conf.int = c(
        lower = lower,
        upper = upper
      ),
      weights = ws_new
    )
  } else {
    list(
      conf.int = c(
        lower = lower,
        upper = upper
      )
    )
  }
}

#' @describeIn estimate_proportions the Clopper-Pearson interval calls
#'   [stats::binom.test()]. Also referred to as the `exact` method.
#' @inheritParams argument_convention
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

#' @describeIn estimate_proportions the Wald interval follows the usual
#'   textbook definition for a single proportion confidence interval using the
#'   normal approximation.
#' @inheritParams argument_convention
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


#' @describeIn estimate_proportions the Agresti-Coull interval was created by
#'   Alan Agresti and Brent Coull and can be understood (for 95% CI) as adding
#'   two successes and two failures to the data, and then using the Wald
#'   formula to construct a CI.
#' @inheritParams argument_convention
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


#' @describeIn estimate_proportions the Jeffreys interval is an equal-tailed
#'   interval based on the non-informative Jeffreys prior for a binomial
#'   proportion.
#' @inheritParams argument_convention
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

#' @describeIn estimate_proportions statistics function estimating a
#'   proportion along with its confidence interval.
#'
#' @param df (`logical` vector or `data.frame`)\cr
#'   if only logical indicates whether each subject is a responder or not.
#'   `TRUE` represents a successful outcome. If a `data.frame` is provided,
#'   also the `strata` parameters in `variables` must be provided.
#' @inheritParams argument_convention
#' @param method (`string`) \cr
#'   the method used to construct the confidence interval for proportion of
#'   successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`, `wilson`,
#'   `wilsonc`, `strat_wilson`, `strat_wilsonc`, `agresti-coull` or `jeffreys`.
#' @inheritParams rtables::
#' @param long (`flag`)\cr a long description is required.
#'
#' @examples
#'
#' # Case with only vector of logical
#' s_proportion(c(1, 0, 1, 0))
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
                         variables = list(strata = NULL, weights = NULL, max_nit = 10),
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
    weights <- variables$weights
    max_nit <- variables$max_nit
  } else if (checkmate::test_subset(method, c("strat_wilson", "strat_wilsonc"))) {
    stop("To use stratified methods you need to specify the strata variables.")
  }
  if (missing(.var)) {
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
    "strat_wilson" = prop_strat_wilson(rsp, strata, weights,
      conf_level, max_nit,
      correct = FALSE
    )$conf.int,
    "strat_wilsonc" = prop_strat_wilson(rsp, strata, weights,
      conf_level, max_nit,
      correct = TRUE
    )$conf.int,
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

#' @describeIn estimate_proportions Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @export
a_proportion <- make_afun(
  s_proportion,
  .formats = c(n_prop = "xx (xx.x%)", prop_ci = "(xx.x, xx.x)")
)

#' @describeIn estimate_proportions used in a `rtables` pipeline.
#' @inheritParams rtables::analyze
#' @param ... other arguments are ultimately conveyed to [s_proportion()].
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
#' @description `r lifecycle::badge("stable")`
#'
#' This is a helper function that describes the analysis in [s_proportion()].
#'
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#' @param long (`flag`)\cr
#'   whether a long or a short (default) description is required.
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
