#' Control function for descriptive statistics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters for summaries of descriptive statistics. Typically used internally to specify
#' details for [s_summary()]. This function family is mainly used by [analyze_vars()].
#'
#' @inheritParams argument_convention
#' @param quantiles (`numeric(2)`)\cr vector of length two to specify the quantiles to calculate.
#' @param quantile_type (`numeric(1)`)\cr number between 1 and 9 selecting quantile algorithms to be used.
#'   Default is set to 2 as this matches the default quantile algorithm in SAS `proc univariate` set by `QNTLDEF=5`.
#'   This differs from R's default. See more about `type` in [stats::quantile()].
#' @param test_mean (`numeric(1)`)\cr number to test against the mean under the null hypothesis when calculating
#'   p-value.
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export
control_analyze_vars <- function(conf_level = 0.95,
                                 quantiles = c(0.25, 0.75),
                                 quantile_type = 2,
                                 test_mean = 0) {
  checkmate::assert_vector(quantiles, len = 2)
  checkmate::assert_int(quantile_type, lower = 1, upper = 9)
  checkmate::assert_numeric(test_mean)
  lapply(quantiles, assert_proportion_value)
  assert_proportion_value(conf_level)
  list(conf_level = conf_level, quantiles = quantiles, quantile_type = quantile_type, test_mean = test_mean)
}

# Helper function to fix numeric or counts pval if necessary
.correct_num_or_counts_pval <- function(type, .stats) {
  if (type == "numeric") {
    if (!is.null(.stats) && any(grepl("^pval", .stats))) {
      .stats[grepl("^pval", .stats)] <- "pval" # tmp fix xxx
    }
  } else {
    if (!is.null(.stats) && any(grepl("^pval", .stats))) {
      .stats[grepl("^pval", .stats)] <- "pval_counts" # tmp fix xxx
    }
  }
  .stats
}

#' Analyze variables
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [analyze_vars()] creates a layout element to summarize one or more variables, using the S3
#' generic function [s_summary()] to calculate a list of summary statistics. A list of all available statistics for
#' numeric variables can be viewed by running `get_stats("analyze_vars_numeric")` and for non-numeric variables by
#' running `get_stats("analyze_vars_counts")`. Use the `.stats` parameter to specify the statistics to include in your
#' output summary table. Use `compare_with_ref_group = TRUE` to compare the variable with reference groups.
#'
#' @details
#' **Automatic digit formatting:** The number of digits to display can be automatically determined from the analyzed
#' variable(s) (`vars`) for certain statistics by setting the statistic format to `"auto"` in `.formats`.
#' This utilizes the [format_auto()] formatting function. Note that only data for the current row & variable (for all
#' columns) will be considered (`.df_row[[.var]]`, see [`rtables::additional_fun_params`]) and not the whole dataset.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options for numeric variables are: ``r shQuote(get_stats("analyze_vars_numeric"))``
#'
#'   Options for non-numeric variables are: ``r shQuote(get_stats("analyze_vars_counts"))``
#'
#' @name analyze_variables
#' @order 1
NULL

#' @describeIn analyze_variables S3 generic function to produces a variable summary.
#'
#' @return
#' * `s_summary()` returns different statistics depending on the class of `x`.
#'
#' @export
s_summary <- function(x, ...) {
  UseMethod("s_summary", x)
}

#' @describeIn analyze_variables Method for `numeric` class.
#'
#' @param control (`list`)\cr parameters for descriptive statistics details, specified by using
#'   the helper function [control_analyze_vars()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for mean and median.
#'   * `quantiles` (`numeric(2)`)\cr vector of length two to specify the quantiles.
#'   * `quantile_type` (`numeric(1)`)\cr between 1 and 9 selecting quantile algorithms to be used.
#'     See more about `type` in [stats::quantile()].
#'   * `test_mean` (`numeric(1)`)\cr value to test against the mean under the null hypothesis when calculating p-value.
#'
#' @return
#'   * If `x` is of class `numeric`, returns a `list` with the following named `numeric` items:
#'     * `n`: The [length()] of `x`.
#'     * `sum`: The [sum()] of `x`.
#'     * `mean`: The [mean()] of `x`.
#'     * `sd`: The [stats::sd()] of `x`.
#'     * `se`: The standard error of `x` mean, i.e.: (`sd(x) / sqrt(length(x))`).
#'     * `mean_sd`: The [mean()] and [stats::sd()] of `x`.
#'     * `mean_se`: The [mean()] of `x` and its standard error (see above).
#'     * `mean_ci`: The CI for the mean of `x` (from [stat_mean_ci()]).
#'     * `mean_sei`: The SE interval for the mean of `x`, i.e.: ([mean()] -/+ [stats::sd()] / [sqrt()]).
#'     * `mean_sdi`: The SD interval for the mean of `x`, i.e.: ([mean()] -/+ [stats::sd()]).
#'     * `mean_pval`: The two-sided p-value of the mean of `x` (from [stat_mean_pval()]).
#'     * `median`: The [stats::median()] of `x`.
#'     * `mad`: The median absolute deviation of `x`, i.e.: ([stats::median()] of `xc`,
#'       where `xc` = `x` - [stats::median()]).
#'     * `median_ci`: The CI for the median of `x` (from [stat_median_ci()]).
#'     * `quantiles`: Two sample quantiles of `x` (from [stats::quantile()]).
#'     * `iqr`: The [stats::IQR()] of `x`.
#'     * `range`: The [range_noinf()] of `x`.
#'     * `min`: The [max()] of `x`.
#'     * `max`: The [min()] of `x`.
#'     * `median_range`: The [median()] and [range_noinf()] of `x`.
#'     * `cv`: The coefficient of variation of `x`, i.e.: ([stats::sd()] / [mean()] * 100).
#'     * `geom_mean`: The geometric mean of `x`, i.e.: (`exp(mean(log(x)))`).
#'     * `geom_cv`: The geometric coefficient of variation of `x`, i.e.: (`sqrt(exp(sd(log(x)) ^ 2) - 1) * 100`).
#'
#' @note
#' * If `x` is an empty vector, `NA` is returned. This is the expected feature so as to return `rcell` content in
#'   `rtables` when the intersection of a column and a row delimits an empty data selection.
#' * When the `mean` function is applied to an empty vector, `NA` will be returned instead of `NaN`, the latter
#'   being standard behavior in R.
#'
#' @method s_summary numeric
#'
#' @examples
#' # `s_summary.numeric`
#'
#' ## Basic usage: empty numeric returns NA-filled items.
#' s_summary(numeric())
#'
#' ## Management of NA values.
#' x <- c(NA_real_, 1)
#' s_summary(x, na_rm = TRUE)
#' s_summary(x, na_rm = FALSE)
#'
#' x <- c(NA_real_, 1, 2)
#' s_summary(x)
#'
#' ## Benefits in `rtables` contructions:
#' dta_test <- data.frame(
#'   Group = rep(LETTERS[seq(3)], each = 2),
#'   sub_group = rep(letters[seq(2)], each = 3),
#'   x = seq(6)
#' )
#'
#' ## The summary obtained in with `rtables`:
#' basic_table() %>%
#'   split_cols_by(var = "Group") %>%
#'   split_rows_by(var = "sub_group") %>%
#'   analyze(vars = "x", afun = s_summary) %>%
#'   build_table(df = dta_test)
#'
#' ## By comparison with `lapply`:
#' X <- split(dta_test, f = with(dta_test, interaction(Group, sub_group)))
#' lapply(X, function(x) s_summary(x$x))
#'
#' @export
s_summary.numeric <- function(x, control = control_analyze_vars(), ...) {
  checkmate::assert_numeric(x)
  args_list <- list(...)
  .N_row <- args_list[[".N_row"]] # nolint
  .N_col <- args_list[[".N_col"]] # nolint
  na_rm <- args_list[["na_rm"]] %||% TRUE
  compare_with_ref_group <- args_list[["compare_with_ref_group"]]

  if (na_rm) {
    x <- x[!is.na(x)]
  }

  y <- list()

  y$n <- c("n" = length(x))

  y$sum <- c("sum" = ifelse(length(x) == 0, NA_real_, sum(x, na.rm = FALSE)))

  y$mean <- c("mean" = ifelse(length(x) == 0, NA_real_, mean(x, na.rm = FALSE)))

  y$sd <- c("sd" = stats::sd(x, na.rm = FALSE))

  y$se <- c("se" = stats::sd(x, na.rm = FALSE) / sqrt(length(stats::na.omit(x))))

  y$mean_sd <- c(y$mean, "sd" = stats::sd(x, na.rm = FALSE))

  y$mean_se <- c(y$mean, y$se)

  mean_ci <- stat_mean_ci(x, conf_level = control$conf_level, na.rm = FALSE, gg_helper = FALSE)
  y$mean_ci <- formatters::with_label(mean_ci, paste("Mean", f_conf_level(control$conf_level)))

  mean_sei <- y$mean[[1]] + c(-1, 1) * stats::sd(x, na.rm = FALSE) / sqrt(y$n)
  names(mean_sei) <- c("mean_sei_lwr", "mean_sei_upr")
  y$mean_sei <- formatters::with_label(mean_sei, "Mean -/+ 1xSE")

  mean_sdi <- y$mean[[1]] + c(-1, 1) * stats::sd(x, na.rm = FALSE)
  names(mean_sdi) <- c("mean_sdi_lwr", "mean_sdi_upr")
  y$mean_sdi <- formatters::with_label(mean_sdi, "Mean -/+ 1xSD")
  mean_ci_3d <- c(y$mean, y$mean_ci)
  y$mean_ci_3d <- formatters::with_label(mean_ci_3d, paste0("Mean (", f_conf_level(control$conf_level), ")"))

  mean_pval <- stat_mean_pval(x, test_mean = control$test_mean, na.rm = FALSE, n_min = 2)
  y$mean_pval <- formatters::with_label(mean_pval, paste("Mean", f_pval(control$test_mean)))

  y$median <- c("median" = stats::median(x, na.rm = FALSE))

  y$mad <- c("mad" = stats::median(x - y$median, na.rm = FALSE))

  median_ci <- stat_median_ci(x, conf_level = control$conf_level, na.rm = FALSE, gg_helper = FALSE)
  y$median_ci <- formatters::with_label(median_ci, paste("Median", f_conf_level(control$conf_level)))

  median_ci_3d <- c(y$median, median_ci)
  y$median_ci_3d <- formatters::with_label(median_ci_3d, paste0("Median (", f_conf_level(control$conf_level), ")"))

  q <- control$quantiles
  if (any(is.na(x))) {
    qnts <- rep(NA_real_, length(q))
  } else {
    qnts <- stats::quantile(x, probs = q, type = control$quantile_type, na.rm = FALSE)
  }
  names(qnts) <- paste("quantile", q, sep = "_")
  y$quantiles <- formatters::with_label(qnts, paste0(paste(paste0(q * 100, "%"), collapse = " and "), "-ile"))

  y$iqr <- c("iqr" = ifelse(
    any(is.na(x)),
    NA_real_,
    stats::IQR(x, na.rm = FALSE, type = control$quantile_type)
  ))

  y$range <- stats::setNames(range_noinf(x, na.rm = FALSE), c("min", "max"))
  y$min <- y$range[1]
  y$max <- y$range[2]

  y$median_range <- formatters::with_label(c(y$median, y$range), "Median (Min - Max)")

  y$cv <- c("cv" = unname(y$sd) / unname(y$mean) * 100)

  # Convert negative values to NA for log calculation.
  x_no_negative_vals <- x
  x_no_negative_vals[x_no_negative_vals <= 0] <- NA
  y$geom_mean <- c("geom_mean" = exp(mean(log(x_no_negative_vals), na.rm = FALSE)))
  geom_mean_ci <- stat_mean_ci(x, conf_level = control$conf_level, na.rm = FALSE, gg_helper = FALSE, geom_mean = TRUE)
  y$geom_mean_ci <- formatters::with_label(geom_mean_ci, paste("Geometric Mean", f_conf_level(control$conf_level)))

  y$geom_cv <- c("geom_cv" = sqrt(exp(stats::sd(log(x_no_negative_vals), na.rm = FALSE) ^ 2) - 1) * 100) # styler: off

  geom_mean_ci_3d <- c(y$geom_mean, y$geom_mean_ci)
  y$geom_mean_ci_3d <- formatters::with_label(
    geom_mean_ci_3d,
    paste0("Geometric Mean (", f_conf_level(control$conf_level), ")")
  )

  # Compare with reference group
  if (isTRUE(compare_with_ref_group)) {
    .ref_group <- args_list[[".ref_group"]]
    .in_ref_col <- args_list[[".in_ref_col"]]
    checkmate::assert_numeric(.ref_group)
    checkmate::assert_flag(.in_ref_col)

    y$pval <- character()
    if (!.in_ref_col && n_available(x) > 1 && n_available(.ref_group) > 1) {
      y$pval <- stats::t.test(x, .ref_group)$p.value
    }
  }

  y
}

#' @describeIn analyze_variables Method for `factor` class.
#'
#' @return
#'   * If `x` is of class `factor` or converted from `character`, returns a `list` with named `numeric` items:
#'     * `n`: The [length()] of `x`.
#'     * `count`: A list with the number of cases for each level of the factor `x`.
#'     * `count_fraction`: Similar to `count` but also includes the proportion of cases for each level of the
#'       factor `x` relative to the denominator, or `NA` if the denominator is zero.
#'
#' @note
#' * If `x` is an empty `factor`, a list is still returned for `counts` with one element
#'   per factor level. If there are no levels in `x`, the function fails.
#' * If factor variables contain `NA`, these `NA` values are excluded by default. To include `NA` values
#'   set `na_rm = FALSE` and missing values will be displayed as an `NA` level. Alternatively, an explicit
#'   factor level can be defined for `NA` values during pre-processing via [df_explicit_na()] - the
#'   default `na_level` (`"<Missing>"`) will also be excluded when `na_rm` is set to `TRUE`.
#'
#' @method s_summary factor
#'
#' @examples
#' # `s_summary.factor`
#'
#' ## Basic usage:
#' s_summary(factor(c("a", "a", "b", "c", "a")))
#'
#' # Empty factor returns zero-filled items.
#' s_summary(factor(levels = c("a", "b", "c")))
#'
#' ## Management of NA values.
#' x <- factor(c(NA, "Female"))
#' x <- explicit_na(x)
#' s_summary(x, na_rm = TRUE)
#' s_summary(x, na_rm = FALSE)
#'
#' ## Different denominators.
#' x <- factor(c("a", "a", "b", "c", "a"))
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
#' @export
s_summary.factor <- function(x, denom = c("n", "N_col", "N_row"), ...) {
  assert_valid_factor(x)
  args_list <- list(...)
  .N_row <- args_list[[".N_row"]] # nolint
  .N_col <- args_list[[".N_col"]] # nolint
  na_rm <- args_list[["na_rm"]] %||% TRUE
  verbose <- args_list[["verbose"]] %||% TRUE
  compare_with_ref_group <- args_list[["compare_with_ref_group"]]

  if (na_rm) {
    x <- x[!is.na(x)] %>% fct_discard("<Missing>")
  } else {
    x <- x %>% explicit_na(label = "NA")
  }

  y <- list()

  y$n <- list("n" = c("n" = length(x))) # all list of a list

  y$count <- lapply(as.list(table(x, useNA = "ifany")), setNames, nm = "count")

  denom <- match.arg(denom) %>%
    switch(
      n = length(x),
      N_row = .N_row,
      N_col = .N_col
    )

  y$count_fraction <- lapply(
    y$count,
    function(x) {
      c(x, "p" = ifelse(denom > 0, x / denom, 0))
    }
  )

  y$count_fraction_fixed_dp <- y$count_fraction

  y$fraction <- lapply(
    y$count,
    function(count) c("num" = unname(count), "denom" = denom)
  )

  y$n_blq <- list("n_blq" = c("n_blq" = sum(grepl("BLQ|LTR|<[1-9]|<PCLLOQ", x))))


  if (isTRUE(compare_with_ref_group)) {
    .ref_group <- as_factor_keep_attributes(args_list[[".ref_group"]], verbose = verbose)
    .in_ref_col <- args_list[[".in_ref_col"]]
    checkmate::assert_flag(.in_ref_col)
    assert_valid_factor(x)
    assert_valid_factor(.ref_group)

    if (na_rm) {
      x <- x[!is.na(x)] %>% fct_discard("<Missing>")
      .ref_group <- .ref_group[!is.na(.ref_group)] %>% fct_discard("<Missing>")
    } else {
      x <- x %>% explicit_na(label = "NA")
      .ref_group <- .ref_group %>% explicit_na(label = "NA")
    }

    if ("NA" %in% levels(x)) levels(.ref_group) <- c(levels(.ref_group), "NA")
    checkmate::assert_factor(x, levels = levels(.ref_group), min.levels = 2)

    y$pval_counts <- character()
    if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
      tab <- rbind(table(x), table(.ref_group))
      res <- suppressWarnings(stats::chisq.test(tab))
      y$pval_counts <- res$p.value
    }
  }

  y
}

#' @describeIn analyze_variables Method for `character` class. This makes an automatic
#'   conversion to factor (with a warning) and then forwards to the method for factors.
#'
#' @note
#' * Automatic conversion of character to factor does not guarantee that the table
#'   can be generated correctly. In particular for sparse tables this very likely can fail.
#'   It is therefore better to always pre-process the dataset such that factors are manually
#'   created from character variables before passing the dataset to [rtables::build_table()].
#'
#' @method s_summary character
#'
#' @examples
#' # `s_summary.character`
#'
#' ## Basic usage:
#' s_summary(c("a", "a", "b", "c", "a"), verbose = FALSE)
#' s_summary(c("a", "a", "b", "c", "a", ""), .var = "x", na_rm = FALSE, verbose = FALSE)
#'
#' @export
s_summary.character <- function(x, denom = c("n", "N_col", "N_row"), ...) {
  args_list <- list(...)
  na_rm <- args_list[["na_rm"]] %||% TRUE
  verbose <- args_list[["verbose"]] %||% TRUE

  if (na_rm) {
    y <- as_factor_keep_attributes(x, verbose = verbose)
  } else {
    y <- as_factor_keep_attributes(x, verbose = verbose, na_level = "NA")
  }

  s_summary(x = y, denom = denom, ...)
}

#' @describeIn analyze_variables Method for `logical` class.
#'
#' @return
#'   * If `x` is of class `logical`, returns a `list` with named `numeric` items:
#'     * `n`: The [length()] of `x` (possibly after removing `NA`s).
#'     * `count`: Count of `TRUE` in `x`.
#'     * `count_fraction`: Count and proportion of `TRUE` in `x` relative to the denominator, or `NA` if the
#'       denominator is zero. Note that `NA`s in `x` are never counted or leading to `NA` here.
#'
#' @method s_summary logical
#'
#' @examples
#' # `s_summary.logical`
#'
#' ## Basic usage:
#' s_summary(c(TRUE, FALSE, TRUE, TRUE))
#'
#' # Empty factor returns zero-filled items.
#' s_summary(as.logical(c()))
#'
#' ## Management of NA values.
#' x <- c(NA, TRUE, FALSE)
#' s_summary(x, na_rm = TRUE)
#' s_summary(x, na_rm = FALSE)
#'
#' ## Different denominators.
#' x <- c(TRUE, FALSE, TRUE, TRUE)
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
#' @export
s_summary.logical <- function(x, denom = c("n", "N_col", "N_row"), ...) {
  checkmate::assert_logical(x)
  args_list <- list(...)
  .N_row <- args_list[[".N_row"]] # nolint
  .N_col <- args_list[[".N_col"]] # nolint
  na_rm <- args_list[["na_rm"]] %||% TRUE
  compare_with_ref_group <- args_list[["compare_with_ref_group"]]

  if (na_rm) {
    x <- x[!is.na(x)]
  }

  y <- list()
  y$n <- c("n" = length(x))
  denom <- match.arg(denom) %>%
    switch(
      n = length(x),
      N_row = .N_row,
      N_col = .N_col
    )
  y$count <- c("count" = sum(x, na.rm = TRUE))
  y$count_fraction <- c(y$count, "fraction" = ifelse(denom > 0, y$count / denom, 0))
  y$count_fraction_fixed_dp <- y$count_fraction
  y$fraction <- c("num" = unname(y$count), "denom" = denom)
  y$n_blq <- c("n_blq" = 0L)


  if (isTRUE(compare_with_ref_group)) {
    .ref_group <- args_list[[".ref_group"]]
    .in_ref_col <- args_list[[".in_ref_col"]]
    checkmate::assert_flag(.in_ref_col)

    if (na_rm) {
      x <- stats::na.omit(x)
      .ref_group <- stats::na.omit(.ref_group)
    } else {
      x[is.na(x)] <- FALSE
      .ref_group[is.na(.ref_group)] <- FALSE
    }

    y$pval_counts <- character()
    if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
      x <- factor(x, levels = c(TRUE, FALSE))
      .ref_group <- factor(.ref_group, levels = c(TRUE, FALSE))
      tbl <- rbind(table(x), table(.ref_group))
      y$pval_counts <- suppressWarnings(prop_chisq(tbl))
    }
  }

  y
}

#' @describeIn analyze_variables Formatted analysis function which is used as `afun` in `analyze_vars()` and
#'   `compare_vars()` and as `cfun` in `summarize_colvars()`.
#'
#' @param compare_with_ref_group (`flag`)\cr whether comparison statistics should be analyzed instead of summary
#'   statistics (`compare_with_ref_group = TRUE` adds `pval` statistic comparing
#'   against reference group).
#'
#' @return
#' * `a_summary()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @note
#' * To use for comparison (with additional p-value statistic), parameter
#'   `compare_with_ref_group` must be set to `TRUE`.
#' * Ensure that either all `NA` values are converted to an explicit `NA` level or all `NA` values are left as is.
#'
#' @examples
#' a_summary(factor(c("a", "a", "b", "c", "a")), .N_row = 10, .N_col = 10)
#' a_summary(
#'   factor(c("a", "a", "b", "c", "a")),
#'   .ref_group = factor(c("a", "a", "b", "c")), compare_with_ref_group = TRUE, .in_ref_col = TRUE
#' )
#'
#' a_summary(c("A", "B", "A", "C"), .var = "x", .N_col = 10, .N_row = 10, verbose = FALSE)
#' a_summary(
#'   c("A", "B", "A", "C"),
#'   .ref_group = c("B", "A", "C"), .var = "x", compare_with_ref_group = TRUE, verbose = FALSE,
#'   .in_ref_col = FALSE
#' )
#'
#' a_summary(c(TRUE, FALSE, FALSE, TRUE, TRUE), .N_row = 10, .N_col = 10)
#' a_summary(
#'   c(TRUE, FALSE, FALSE, TRUE, TRUE),
#'   .ref_group = c(TRUE, FALSE), .in_ref_col = TRUE, compare_with_ref_group = TRUE,
#'   .in_ref_col = FALSE
#' )
#'
#' a_summary(rnorm(10), .N_col = 10, .N_row = 20, .var = "bla")
#' a_summary(rnorm(10, 5, 1),
#'   .ref_group = rnorm(20, -5, 1), .var = "bla", compare_with_ref_group = TRUE,
#'   .in_ref_col = FALSE
#' )
#'
#' @export
a_summary <- function(x,
                      ...,
                      .stats = NULL,
                      .stat_names = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL) {
  dots_extra_args <- list(...)

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$default_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Correction of the pval indication if it is numeric or counts
  type <- ifelse(is.numeric(x), "numeric", "counts") # counts is "categorical"
  .stats <- .correct_num_or_counts_pval(type, .stats)

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- retrieve_extra_afun_params(
    names(dots_extra_args$.additional_fun_parameters)
  )
  dots_extra_args$.additional_fun_parameters <- NULL # After extraction we do not need them anymore

  # Check if compare_with_ref_group is TRUE but no ref col is set
  if (isTRUE(dots_extra_args$compare_with_ref_group) &&
    all(
      length(dots_extra_args[[".ref_group"]]) == 0, # only used for testing
      length(extra_afun_params[[".ref_group"]]) == 0
    )
  ) {
    stop(
      "For comparison (compare_with_ref_group = TRUE), the reference group must be specified.",
      "\nSee ref_group in split_cols_by()."
    )
  }

  # Main statistical functions application
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_summary,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      x = list(x),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in with stats defaults if needed
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  .stats <- c(
    get_stats(met_grp,
      stats_in = .stats,
      add_pval = dots_extra_args$compare_with_ref_group %||% FALSE
    ),
    names(custom_stat_functions) # Additional stats from custom functions
  )

  x_stats <- x_stats[.stats]

  is_char <- is.character(x) || is.factor(x)
  if (is_char) {
    levels_per_stats <- lapply(x_stats, names)
  } else {
    levels_per_stats <- names(x_stats) %>%
      as.list() %>%
      setNames(names(x_stats))
  }

  # Fill in formats/indents/labels with custom input and defaults
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)
  lbls <- get_labels_from_stats(.stats, .labels, levels_per_stats)

  if (is_char) {
    # Keep pval_counts stat if present from comparisons and empty
    if ("pval_counts" %in% names(x_stats) && length(x_stats[["pval_counts"]]) == 0) {
      x_stats[["pval_counts"]] <- list(NULL) %>% setNames("pval_counts")
    }

    # Unlist stats
    x_stats <- x_stats %>%
      .unlist_keep_nulls() %>%
      setNames(names(.formats))
  }

  # Check for custom labels from control_analyze_vars
  .labels <- if ("control" %in% names(dots_extra_args)) {
    labels_use_control(lbls, dots_extra_args[["control"]], .labels)
  } else {
    lbls
  }

  # Auto format handling
  .formats <- apply_auto_formatting(
    .formats,
    x_stats,
    extra_afun_params$.df_row,
    extra_afun_params$.var
  )

  # Get and check statistical names from defaults
  .stat_names <- get_stat_names(x_stats, .stat_names) # note is x_stats

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn analyze_variables Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @param ... additional arguments passed to `s_summary()`, including:
#'   * `denom`: (`string`) See parameter description below.
#'   * `.N_row`: (`numeric(1)`) Row-wise N (row group count) for the group of observations being analyzed (i.e. with no
#'     column-based subsetting).
#'   * `.N_col`: (`numeric(1)`) Column-wise N (column count) for the full column being tabulated within.
#'   * `verbose`: (`flag`) Whether additional warnings and messages should be printed. Mainly used to print out
#'     information about factor casting. Defaults to `TRUE`. Used for `character`/`factor` variables only.
#' @param compare_with_ref_group (logical)\cr whether to compare the variable with a reference group.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @return
#' * `analyze_vars()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_summary()` to the table layout.
#'
#' @examples
#' ## Fabricated dataset.
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   PARAMCD = rep("lab", 6 * 3),
#'   AVISIT  = rep(paste0("V", 1:3), 6),
#'   ARM     = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL    = c(9:1, rep(NA, 9))
#' )
#'
#' # `analyze_vars()` in `rtables` pipelines
#' ## Default output within a `rtables` pipeline.
#' l <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   analyze_vars(vars = "AVAL")
#'
#' build_table(l, df = dta_test)
#'
#' ## Select and format statistics output.
#' l <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   analyze_vars(
#'     vars = "AVAL",
#'     .stats = c("n", "mean_sd", "quantiles"),
#'     .formats = c("mean_sd" = "xx.x, xx.x"),
#'     .labels = c(n = "n", mean_sd = "Mean, SD", quantiles = c("Q1 - Q3"))
#'   )
#'
#' build_table(l, df = dta_test)
#'
#' ## Use arguments interpreted by `s_summary`.
#' l <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   analyze_vars(vars = "AVAL", na_rm = FALSE)
#'
#' build_table(l, df = dta_test)
#'
#' ## Handle `NA` levels first when summarizing factors.
#' dta_test$AVISIT <- NA_character_
#' dta_test <- df_explicit_na(dta_test)
#' l <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   analyze_vars(vars = "AVISIT", na_rm = FALSE)
#'
#' build_table(l, df = dta_test)
#'
#' # auto format
#' dt <- data.frame("VAR" = c(0.001, 0.2, 0.0011000, 3, 4))
#' basic_table() %>%
#'   analyze_vars(
#'     vars = "VAR",
#'     .stats = c("n", "mean", "mean_sd", "range"),
#'     .formats = c("mean_sd" = "auto", "range" = "auto")
#'   ) %>%
#'   build_table(dt)
#'
#' @export
#' @order 2
analyze_vars <- function(lyt,
                         vars,
                         var_labels = vars,
                         na_str = default_na_str(),
                         nested = TRUE,
                         show_labels = "default",
                         table_names = vars,
                         section_div = NA_character_,
                         ...,
                         na_rm = TRUE,
                         compare_with_ref_group = FALSE,
                         .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                         .stat_names = NULL,
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  # Depending on main functions
  extra_args <- list(
    "na_rm" = na_rm,
    "compare_with_ref_group" = compare_with_ref_group,
    ...
  )

  # Needed defaults
  if (!is.null(.stats)) extra_args[[".stats"]] <- .stats
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Adding all additional information from layout to analysis functions (see ?rtables::additional_fun_params)
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_summary) <- c(
    formals(a_summary),
    extra_args[[".additional_fun_parameters"]]
  )

  # Main {rtables} structural call
  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_summary,
    na_str = na_str,
    inclNAs = !na_rm,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}
