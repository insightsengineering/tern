#' Summarize Variables
#'
#' We use the new S3 generic function [s_summary()] to implement summaries for
#' different `x` objects. This is used as Statistics Function in combination
#' with the new Analyze Function [summarize_vars()].
#'
#' @name summarize_variables
#' @order 1
#'
NULL

#' @inheritParams argument_convention
#' @describeIn summarize_variables `s_summary` is a S3 generic function to produce
#'   an object description.
#'
#' @export
#' @order 2
#'
s_summary <- function(x,
                      na.rm = TRUE,  # nolint
                      denom,
                      .N_row,  # nolint
                      .N_col,  # nolint
                      ...) {
  assert_that(is.flag(na.rm))
  UseMethod("s_summary", x)
}

#' @describeIn summarize_variables Method for numeric class. Note that,
#'   if `x` is an empty vector, `NA` is returned. This is the expected
#'   feature so as to return `rcell` content in `rtables` when the
#'   intersection of a column and a row delimits an empty data selection.
#'   Also, when the `mean` function is applied to an empty vector, `NA` will
#'   be returned instead of `NaN`, the latter being standard behavior in R.
#' @return If `x` is of class `numeric`, returns a list with named items:
#' - `n`: the [length()] of `x`.
#' - `mean_sd`: the [mean()] and [sd()].
#' - `median`: the [median()].
#' - `range`: the [range()].
#' @method s_summary numeric
#' @order 3
#'
#' @importFrom stats sd median
#' @export
#'
#' @examples
#' # `s_summary.numeric`
#'
#' ## Basic usage: empty numeric returns NA-filled items.
#' s_summary(numeric())
#'
#' ## Management of NA values.
#' x <- c(NA_real_, 1)
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' x <- c(NA_real_, 1, 2)
#' s_summary(x)
#'
#' ## Benefits in `rtables` contructions:
#' require(rtables)
#' dta_test <- data.frame(
#'   Group = rep(LETTERS[1:3], each = 2),
#'   sub_group = rep(letters[1:2], each = 3),
#'   x = 1:6
#' )
#'
#' ## The summary obtained in with `rtables`:
#' split_cols_by(lyt = NULL, var = "Group") %>%
#'   split_rows_by(var = "sub_group") %>%
#'   analyze(vars = "x", afun = s_summary) %>%
#'   build_table(df = dta_test)
#'
#' ## By comparison with `lapply`:
#' X <- split(dta_test, f = with(dta_test, interaction(Group, sub_group)))
#' lapply(X, function(x) s_summary(x$x))
#'
s_summary.numeric <- function(x,
                              na.rm = TRUE, # nolint
                              ...) {
  assert_that(is.numeric(x))

  if (na.rm) x <- x[!is.na(x)]

  y <- list()

  dn <- length(x)
  y$n <- with_label(dn, "n")

  y$mean_sd <- with_label(
    x = c(
      mean = if (dn > 0) mean(x) else NA_real_,
      sd = stats::sd(x)
    ),
    label = "Mean (SD)"
  )

  y$median <- with_label(
    x = stats::median(x),
    label = "Median"
  )

  y$range <- with_label(
    x = if (dn > 0) range(x) else rep(NA_real_, 2),
    label = "Min - Max"
  )

  y
}

#' @describeIn summarize_variables Method for factor class. Note that,
#'   if `x` is an empty factor, then still a list is returned for `counts` with one element
#'   per factor level. If there are no levels in `x`, the function fails.
#' @param denom (`string`)\cr choice of denominator for factor proportions:\cr
#'   can be `n` (number of values in this row and column intersection), `N_row` (total
#'   number of values in this row across columns), or `N_col` (total number of values in
#'   this column across rows).
#' @return If `x` is of class `factor` or converted from `character`, returns a list with
#'   named items:
#'   - `n`: the [length()] of `x`.
#'   - `count`: a list with the number of cases for each level of the
#'      factor `x`
#'   - `count_fraction`: similar to `count` but also includes the proportion of cases for each level of the
#'      factor `x` relative to the denominator, or `NA` if the denominator is zero.
#' @method s_summary factor
#' @order 4
#'
#' @export
#'
#' @examples
#' # `s_summary.factor`
#'
#' ## Basic usage:
#' s_summary(factor(c("a", "a", "b", "c", "a")))
#' # Empty factor returns NA-filled items.
#' s_summary(factor(levels = c("a", "b", "c")))
#'
#' ## Management of NA values.
#' x <- factor(c(NA, "Female"))
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' ## Different denominators.
#' x <- factor(c("a", "a", "b", "c", "a"))
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
s_summary.factor <- function(x,
                             na.rm = TRUE, #nolint
                             denom = c("n", "N_row", "N_col"),
                             .N_row, #nolint
                             .N_col, #nolint
                             ...) {
  assert_that(is_valid_factor(x))
  denom <- match.arg(denom)

  if (na.rm) x <- x[!is.na(x)]

  y <- list()

  y$n <- with_label(length(x), "n")

  y$count <- as.list(table(x, useNA = "ifany"))
  dn <- switch(
    denom,
    n = length(x),
    N_row = .N_row,
    N_col = .N_col
  )
  y$count_fraction <- lapply(
    y$count,
    function(x) {
      c(x, ifelse(dn > 0, x / dn, 0))
    }
  )

  y
}

#' @describeIn summarize_variables Method for character class. This makes an automatic
#'   conversion to factor (with a warning) and then forwards to the method for factors.
#' @method s_summary character
#' @order 4
#'
#' @export
#'
#' @examples
#' # `s_summary.character`
#'
#' ## Basic usage:
#' s_summary(c("a", "a", "b", "c", "a"))
#'
s_summary.character <- function(x, ...) {
  y <- as_factor_keep_attributes(x)
  s_summary(y, ...)
}

#' @describeIn summarize_variables Method for logical class.
#' @param denom (`string`)\cr choice of denominator for proportion:\cr
#'   can be `n` (number of values in this row and column intersection), `N_row` (total
#'   number of values in this row across columns), or `N_col` (total number of values in
#'   this column across rows).
#' @return If `x` is of class `logical`, returns a list with named items:
#'   - `n`: the [length()] of `x` (possibly after removing `NA`s).
#'   - `count_fraction`: count and proportion of `TRUE` in `x` relative to the denominator,
#'      or `NA` if the denominator is zero. Note that `NA`s in `x` are never counted or leading
#'      to `NA` here.
#' @method s_summary logical
#' @order 5
#'
#' @export
#'
#' @examples
#' # `s_summary.logical`
#'
#' ## Basic usage:
#' s_summary(c(TRUE, FALSE, TRUE, TRUE))
#'
#' ## Management of NA values.
#' x <- c(NA, TRUE, FALSE)
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' ## Different denominators.
#' x <- c(TRUE, FALSE, TRUE, TRUE)
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
s_summary.logical <- function(x,
                              na.rm = TRUE, # nolint
                              denom = c("n", "N_row", "N_col"),
                              .N_row, # nolint
                              .N_col, # nolint
                              ...) {
  denom <- match.arg(denom)
  if (na.rm) x <- x[!is.na(x)]
  y <- list()
  y$n <- length(x)
  count <- sum(x, na.rm = TRUE)
  dn <- switch(
    denom,
    n = length(x),
    N_row = .N_row,
    N_col = .N_col
  )
  y$count_fraction <- c(count, ifelse(dn > 0, count / dn, NA))
  y$count <- c(count)
  y
}

#' @describeIn summarize_variables Analyze Function to add a descriptive analyze
#'   layer to `rtables` pipelines. The analysis is applied to a vector and
#'   return the summary, in `rcells`. The ellipsis (`...`) conveys arguments to
#'   [s_summary()], for instance `na.rm = FALSE` if missing data should be
#'   accounted for.
#' @inheritParams rtables::analyze
#' @param ... arguments passed to `s_summary()`.
#'
#' @order 6
#' @template formatting_arguments
#'
#' @export
#' @examples
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
#'
summarize_vars <- function(lyt,
                           vars,
                           var_labels = vars,
                           nested = TRUE,
                           show_labels = "default",
                           .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                           ...) {
  afun <- format_wrap_x(
    sfun = s_summary,
    indent_mods = c(
      n = 0L,
      mean_sd = 0L,
      median = 0L,
      range = 0L,
      count = 0L,
      count_fraction = 0L
    ),
    formats = c(
      n = "xx",
      mean_sd = "xx.x (xx.x)",
      median = "xx.x",
      range = "xx.x - xx.x",
      count = "xx",
      count_fraction = format_count_fraction
    )
  )

  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = afun,
    nested = nested,
    extra_args = c(list(.stats = .stats), list(...)),
    inclNAs = TRUE,
    show_labels = show_labels
  )

}
