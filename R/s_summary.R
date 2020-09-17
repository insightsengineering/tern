#' Summarize variables
#'
#' Produces an object description.
#' @name summarize_variables
#' @order 1
NULL


#' S3 generic for `s_summary`
#'
#' @param x a vector.
#' @param na.rm (`logical`)
#'
#' Indicating whether `NA` values should be
#' stripped before the computation proceeds.
#'
#' @describeIn summarize_variables `s_summary` is a generic function to produce
#'   an object description.
#'
#' @export
#' @md
#' @order 2
s_summary <- function(x, na.rm = TRUE) # nolint
  UseMethod("s_summary", x)


#' `s_summary` of numeric
#'
#'
#' @return
#'
#' If `x` is of class `numeric`, returns a list with named items:
#'
#' - `n`: the [length()] of `x`.
#' - `mean_sd`: the [mean()] and [sd()].
#' - `median`: the [median()].
#' - `range`: the [range()].
#'
#' @method s_summary numeric
#' @describeIn summarize_variables Method for numeric class. Note that,
#'   if `x` is an empty vector, `NA` is returned. This is the expected
#'   feature so as to return `rcell` content in `rtables` when the
#'   intersection of a column and a row delimits an empty data selection.
#'   Also, when the `mean` function is applied to an empty vector, `NA` will
#'   be returned instead of `NaN`, the latter being standard behavior in R.
#' @order 3
#'
#' @md
#'
#' @importFrom stats sd median
#' @import assertthat
#' @import rtables
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
s_summary.numeric <- function(x,
                              na.rm = TRUE # nolint
) {

  assert_that(is.numeric(x), is.flag(na.rm))

  x <- if (na.rm) x[!is.na(x)] else x

  y <- list()

  y$n <- with_label(
    x = length(x),
    label = "n"
  )

  y$mean_sd <- with_label(
    x = c(
      mean = if (y$n > 0) mean(x) else NA_real_,
      sd = stats::sd(x)
    ),
    label = "Mean (SD)"
  )

  y$median <- with_label(
    x = stats::median(x),
    label = "Median"
  )

  y$range <- with_label(
    x = if (y$n > 0) range(x) else rep(NA_real_, 2),
    label = "Min - Max"
  )

  y

}
