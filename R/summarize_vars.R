#' `summarize_vars`
#'
#' @inheritParams rtables::analyze
#' @param ... arguments passed to `s_summary()`.
#'
#' @describeIn summarize_variables Adds a descriptive analyze layer to `rtables`
#'   pipelines. The analysis is applied to a vector and return the summary,
#'   in `rcells`. The ellipsis (`...`) conveys arguments to [s_summary()], for
#'   instance `na.rm = FALSE` if missing data should be accounted for.
#' @order 4
#' @template formatting_arguments
#'
#' @export
#' @md
#' @examples
#'
#'
#' # `summarize_vars()` in rtables pipelines
#'
#' ## Fabricated dataset.
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   PARAMCD = rep("lab", 6*3),
#'   AVISIT  = rep(paste0("V", 1:3), 6),
#'   ARM     = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL    = c(9:1, rep(NA, 9))
#' )
#'
#' ## Default output within a `rtables` pipeline.
#' l <- split_cols_by(lyt = NULL, var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   summarize_vars(vars = "AVAL")
#'
#' build_table(l, df = dta_test)
#'
#' ## Select and format statistics output.
#' l <- split_cols_by(lyt = NULL, var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   summarize_vars(
#'     vars = "AVAL",
#'     .stats = c("n", "mean_sd"),
#'     .formats = c("mean_sd" = "xx.x, xx.x"),
#'     .labels = c(n = "n", mean_sd = "Mean, SD")
#'   )
#'
#' results <- build_table(l, df = dta_test)
#' as_html(results)
#'
#' ## Use arguments interpreted by `s_summary`.
#' l <- split_cols_by(lyt = NULL, var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   summarize_vars(vars = "AVAL", na.rm = FALSE)
#'
#' results <- build_table(l, df = dta_test)
#' Viewer(results)
summarize_vars <- function(lyt,
                           vars,
                           ...) {

  afun <- format_wrap_x(
    sfun = s_summary,
    indent_mods = c(n = 0L, mean_sd = 0L, median = 0L, range = 0L),
    formats = c(
      n = "xx", mean_sd = "xx.x (xx.x)", median = "xx.x", range = "xx.x - xx.x"
    )
  )

  analyze(
    lyt, vars,
    afun = afun,
    extra_args = list(...),
    inclNAs = TRUE
  )

}
