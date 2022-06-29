#' Estimation of Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the proportion of responders within a studied population.
#'
#' @seealso [h_prop()] for relevant helper functions.
#'
#' @name estimate_proportions
NULL

#' @describeIn estimate_proportions statistics function estimating a
#'   proportion along with its confidence interval.
#'
#' @inheritParams argument_convention
#' @param x (`logical`)\cr whether each subject is a responder or not.
#' `TRUE` represents a successful outcome.
#' @param method (`string`) \cr
#'   the method used to construct the confidence interval for proportion of
#'   successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`, `wilson`,
#'   `agresti-coull` or `jeffreys`.
#' @param long (`flag`)\cr a long description is required.
#'
#' @examples
#' s_proportion(c(1, 0, 1, 0))
#'
#' @export
s_proportion <- function(x,
                         conf_level = 0.95,
                         method = c(
                           "waldcc", "wald", "clopper-pearson",
                           "wilson", "wilsonc", "agresti-coull", "jeffreys"
                         ),
                         long = FALSE) {
  x <- as.logical(x)

  method <- match.arg(method)
  checkmate::assert_flag(long)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)

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
#'
#' @examples
#' a_proportion(c(1, 0, 1, 0))
#'
#' @export
a_proportion <- make_afun(
  s_proportion,
  .formats = c(n_prop = "xx (xx.x%)", prop_ci = "(xx.x, xx.x)")
)

#' @describeIn estimate_proportions used in a `rtables` pipeline.
#'
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
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#' @param long (`flag`)\cr Whether a long or a short (default) description is required.
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
    "wilsonc" = "Wilson, with correction",
    "agresti-coull" = "Agresti-Coull",
    "jeffreys" = "Jeffreys",
    stop(paste(method, "does not have a description"))
  )

  paste0(label, " (", method_part, ")")
}
