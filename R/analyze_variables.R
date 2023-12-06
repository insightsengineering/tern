#' Control Function for Descriptive Statistics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters for summaries of descriptive statistics. Typically used internally to specify
#' details for [s_summary()]. This function family is mainly used by [analyze_vars()].
#'
#' @inheritParams argument_convention
#' @param quantiles (`numeric`)\cr of length two to specify the quantiles to calculate.
#' @param quantile_type (`numeric`)\cr between 1 and 9 selecting quantile algorithms to be used.
#'   Default is set to 2 as this matches the default quantile algorithm in SAS `proc univariate` set by `QNTLDEF=5`.
#'   This differs from R's default. See more about `type` in [stats::quantile()].
#' @param test_mean (`numeric`)\cr to test against the mean under the null hypothesis when calculating p-value.
#'
#' @note Deprecation cycle started for `control_summarize_vars` as it is going to renamed into
#'   `control_analyze_vars`. Intention is to reflect better the core underlying `rtables`
#'   functions; in this case [analyze_vars()] wraps [rtables::analyze()].
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export control_analyze_vars control_summarize_vars
#' @aliases control_summarize_vars
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

control_summarize_vars <- control_analyze_vars


#' Analyze Variables
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [analyze_vars()] generates a summary of one or more variables, using the S3 generic function
#' [s_summary()] to calculate a list of summary statistics. A list of all available statistics for numeric
#' variables can be viewed by running `get_stats("analyze_vars_numeric")` and for non-numeric variables by running
#' `get_stats("analyze_vars_counts")`. Use the `.stats` parameter to specify the statistics to include in your output
#' summary table.
#'
#' @details
#' **Automatic digit formatting:** The number of digits to display can be automatically determined from the analyzed
#' variable(s) (`vars`) for certain statistics by setting the statistic format to `"auto"` in `.formats`.
#' This utilizes the [format_auto()] formatting function. Note that only data for the current row & variable (for all
#' columns) will be considered (`.df_row[[.var]]`, see [`rtables::additional_fun_params`]) and not the whole dataset.
#'
#' @note
#' * Deprecation cycle started for `summarize_vars` which has been renamed to `analyze_vars`. This renaming is intended
#'   to better reflect its core underlying `rtables` functions - in this case [rtables::analyze()].
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("analyze_vars_numeric")` to see
#'   statistics available for numeric variables, and `get_stats("analyze_vars_counts")` for statistics available
#'   for non-numeric variables.
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
s_summary <- function(x,
                      na.rm = TRUE, # nolint
                      denom,
                      .N_row, # nolint
                      .N_col, # nolint
                      .var,
                      ...) {
  checkmate::assert_flag(na.rm)
  UseMethod("s_summary", x)
}

#' @describeIn analyze_variables Method for `numeric` class.
#'
#' @param control (`list`)\cr parameters for descriptive statistics details, specified by using
#'   the helper function [control_analyze_vars()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for mean and median.
#'   * `quantiles` (`numeric`)\cr vector of length two to specify the quantiles.
#'   * `quantile_type` (`numeric`)\cr between 1 and 9 selecting quantile algorithms to be used.
#'     See more about `type` in [stats::quantile()].
#'   * `test_mean` (`numeric`)\cr value to test against the mean under the null hypothesis when calculating p-value.
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
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' x <- c(NA_real_, 1, 2)
#' s_summary(x, stats = NULL)
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
s_summary.numeric <- function(x,
                              na.rm = TRUE, # nolint
                              denom,
                              .N_row, # nolint
                              .N_col, # nolint
                              .var,
                              control = control_analyze_vars(),
                              ...) {
  checkmate::assert_numeric(x)

  if (na.rm) {
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

  mean_pval <- stat_mean_pval(x, test_mean = control$test_mean, na.rm = FALSE, n_min = 2)
  y$mean_pval <- formatters::with_label(mean_pval, paste("Mean", f_pval(control$test_mean)))

  y$median <- c("median" = stats::median(x, na.rm = FALSE))

  y$mad <- c("mad" = stats::median(x - y$median, na.rm = FALSE))

  median_ci <- stat_median_ci(x, conf_level = control$conf_level, na.rm = FALSE, gg_helper = FALSE)
  y$median_ci <- formatters::with_label(median_ci, paste("Median", f_conf_level(control$conf_level)))

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

  y
}

#' @describeIn analyze_variables Method for `factor` class.
#'
#' @param denom (`string`)\cr choice of denominator for factor proportions. Options are:
#'   * `n`: number of values in this row and column intersection.
#'   * `N_row`: total number of values in this row across columns.
#'   * `N_col`: total number of values in this column across rows.
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
#'   set `na.rm = FALSE` and missing values will be displayed as an `NA` level. Alternatively, an explicit
#'   factor level can be defined for `NA` values during pre-processing via [df_explicit_na()] - the
#'   default `na_level` (`"<Missing>"`) will also be excluded when `na.rm` is set to `TRUE`.
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
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' ## Different denominators.
#' x <- factor(c("a", "a", "b", "c", "a"))
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
#' @export
s_summary.factor <- function(x,
                             na.rm = TRUE, # nolint
                             denom = c("n", "N_row", "N_col"),
                             .N_row, # nolint
                             .N_col, # nolint
                             ...) {
  assert_valid_factor(x)
  denom <- match.arg(denom)

  if (na.rm) {
    x <- x[!is.na(x)] %>% fct_discard("<Missing>")
  } else {
    x <- x %>% explicit_na(label = "NA")
  }

  y <- list()

  y$n <- length(x)

  y$count <- as.list(table(x, useNA = "ifany"))
  dn <- switch(denom,
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

  y$n_blq <- sum(grepl("BLQ|LTR|<[1-9]|<PCLLOQ", x))

  y
}

#' @describeIn analyze_variables Method for `character` class. This makes an automatic
#'   conversion to factor (with a warning) and then forwards to the method for factors.
#'
#' @param verbose (`logical`)\cr Defaults to `TRUE`, which prints out warnings and messages. It is mainly used
#'   to print out information about factor casting.
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
#' s_summary(c("a", "a", "b", "c", "a"), .var = "x", verbose = FALSE)
#' s_summary(c("a", "a", "b", "c", "a", ""), .var = "x", na.rm = FALSE, verbose = FALSE)
#'
#' @export
s_summary.character <- function(x,
                                na.rm = TRUE, # nolint
                                denom = c("n", "N_row", "N_col"),
                                .N_row, # nolint
                                .N_col, # nolint
                                .var,
                                verbose = TRUE,
                                ...) {
  if (na.rm) {
    y <- as_factor_keep_attributes(x, verbose = verbose)
  } else {
    y <- as_factor_keep_attributes(x, verbose = verbose, na_level = "NA")
  }

  s_summary(
    x = y,
    na.rm = na.rm,
    denom = denom,
    .N_row = .N_row,
    .N_col = .N_col,
    ...
  )
}

#' @describeIn analyze_variables Method for `logical` class.
#'
#' @param denom (`string`)\cr choice of denominator for proportion. Options are:
#'   * `n`: number of values in this row and column intersection.
#'   * `N_row`: total number of values in this row across columns.
#'   * `N_col`: total number of values in this column across rows.
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
#' s_summary(x, na.rm = TRUE)
#' s_summary(x, na.rm = FALSE)
#'
#' ## Different denominators.
#' x <- c(TRUE, FALSE, TRUE, TRUE)
#' s_summary(x, denom = "N_row", .N_row = 10L)
#' s_summary(x, denom = "N_col", .N_col = 20L)
#'
#' @export
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
  dn <- switch(denom,
    n = length(x),
    N_row = .N_row,
    N_col = .N_col
  )
  y$count <- count
  y$count_fraction <- c(count, ifelse(dn > 0, count / dn, 0))
  y$n_blq <- 0L
  y
}

#' @describeIn analyze_variables Formatted analysis function which is used as `afun` in `analyze_vars()` and
#'   `compare_vars()` and as `cfun` in `summarize_colvars()`.
#'
#' @param compare (`logical`)\cr Whether comparison statistics should be analyzed instead of summary statistics
#'   (`compare = TRUE` adds `pval` statistic comparing against reference group).
#'
#' @return
#' * `a_summary()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @note
#' * To use for comparison (with additional p-value statistic), parameter `compare` must be set to `TRUE`.
#' * Ensure that either all `NA` values are converted to an explicit `NA` level or all `NA` values are left as is.
#'
#' @examples
#' a_summary(factor(c("a", "a", "b", "c", "a")), .N_row = 10, .N_col = 10)
#' a_summary(
#'   factor(c("a", "a", "b", "c", "a")),
#'   .ref_group = factor(c("a", "a", "b", "c")), compare = TRUE
#' )
#'
#' a_summary(c("A", "B", "A", "C"), .var = "x", .N_col = 10, .N_row = 10, verbose = FALSE)
#' a_summary(
#'   c("A", "B", "A", "C"),
#'   .ref_group = c("B", "A", "C"), .var = "x", compare = TRUE, verbose = FALSE
#' )
#'
#' a_summary(c(TRUE, FALSE, FALSE, TRUE, TRUE), .N_row = 10, .N_col = 10)
#' a_summary(
#'   c(TRUE, FALSE, FALSE, TRUE, TRUE),
#'   .ref_group = c(TRUE, FALSE), .in_ref_col = TRUE, compare = TRUE
#' )
#'
#' a_summary(rnorm(10), .N_col = 10, .N_row = 20, .var = "bla")
#' a_summary(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .var = "bla", compare = TRUE)
#'
#' @export
a_summary <- function(x,
                      .N_col, # nolint
                      .N_row, # nolint
                      .var = NULL,
                      .df_row = NULL,
                      .ref_group = NULL,
                      .in_ref_col = FALSE,
                      compare = FALSE,
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL,
                      na.rm = TRUE, # nolint
                      na_level = lifecycle::deprecated(),
                      na_str = default_na_str(),
                      ...) {
  extra_args <- list(...)
  if (lifecycle::is_present(na_level)) {
    lifecycle::deprecate_warn("0.9.1", "a_summary(na_level)", "a_summary(na_str)")
    na_str <- na_level
  }

  if (is.numeric(x)) {
    type <- "numeric"
    if (!is.null(.stats) && any(grepl("^pval", .stats))) {
      .stats[grepl("^pval", .stats)] <- "pval" # tmp fix xxx
    }
  } else {
    type <- "counts"
    if (!is.null(.stats) && any(grepl("^pval", .stats))) {
      .stats[grepl("^pval", .stats)] <- "pval_counts" # tmp fix xxx
    }
  }

  # If one col has NA vals, must add NA row to other cols (using placeholder lvl `fill-na-level`)
  if (any(is.na(.df_row[[.var]])) && !any(is.na(x)) && !na.rm) levels(x) <- c(levels(x), "fill-na-level")

  x_stats <- if (!compare) {
    s_summary(x = x, .N_col = .N_col, .N_row = .N_row, na.rm = na.rm, ...)
  } else {
    s_compare(
      x = x, .N_col = .N_col, .N_row = .N_row, na.rm = na.rm, .ref_group = .ref_group, .in_ref_col = .in_ref_col, ...
    )
  }

  # Fill in with formatting defaults if needed
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  .stats <- get_stats(met_grp, stats_in = .stats, add_pval = compare)
  .formats <- get_formats_from_stats(.stats, .formats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  lbls <- get_labels_from_stats(.stats, .labels)
  # Check for custom labels from control_analyze_vars
  .labels <- if ("control" %in% names(extra_args)) {
    lbls %>% labels_use_control(extra_args[["control"]], .labels)
  } else {
    lbls
  }

  if ("count_fraction_fixed_dp" %in% .stats) x_stats[["count_fraction_fixed_dp"]] <- x_stats[["count_fraction"]]
  x_stats <- x_stats[.stats]

  if (is.factor(x) || is.character(x)) {
    # Ungroup statistics with values for each level of x
    x_ungrp <- ungroup_stats(x_stats, .formats, .labels, .indent_mods)
    x_stats <- x_ungrp[["x"]]
    .formats <- x_ungrp[[".formats"]]
    .labels <- gsub("fill-na-level", "NA", x_ungrp[[".labels"]])
    .indent_mods <- x_ungrp[[".indent_mods"]]
  }

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = na_str
  )
}

#' Constructor Function for [analyze_vars()] and [summarize_colvars()]
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Constructor function which creates a combined formatted analysis function.
#'
#' @inheritParams argument_convention
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @return Combined formatted analysis function for use in [analyze_vars()].
#'
#' @note This function has been deprecated in favor of direct implementation of `a_summary()`.
#'
#' @seealso [analyze_vars()]
#'
#' @export
create_afun_summary <- function(.stats, .formats, .labels, .indent_mods) {
  lifecycle::deprecate_warn(
    "0.8.5.9010",
    "create_afun_summary()",
    details = "Please use a_summary() directly instead."
  )
  function(x,
           .ref_group,
           .in_ref_col,
           ...,
           .var) {
    a_summary(x,
      .stats = .stats,
      .formats = .formats,
      .labels = .labels,
      .indent_mods = .indent_mods,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      .var = .var, ...
    )
  }
}

#' @describeIn analyze_variables Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @param ... arguments passed to `s_summary()`.
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
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
#'   analyze_vars(vars = "AVAL", na.rm = FALSE)
#'
#' build_table(l, df = dta_test)
#'
#' ## Handle `NA` levels first when summarizing factors.
#' dta_test$AVISIT <- NA_character_
#' dta_test <- df_explicit_na(dta_test)
#' l <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   analyze_vars(vars = "AVISIT", na.rm = FALSE)
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
#' @export analyze_vars summarize_vars
#' @order 2
analyze_vars <- function(lyt,
                         vars,
                         var_labels = vars,
                         na_level = lifecycle::deprecated(),
                         na_str = default_na_str(),
                         nested = TRUE,
                         ...,
                         na.rm = TRUE, # nolint
                         show_labels = "default",
                         table_names = vars,
                         section_div = NA_character_,
                         .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  if (lifecycle::is_present(na_level)) {
    lifecycle::deprecate_warn("0.9.1", "analyze_vars(na_level)", "analyze_vars(na_str)")
    na_str <- na_level
  }

  extra_args <- list(.stats = .stats, na.rm = na.rm, na_str = na_str, ...)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_summary,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    inclNAs = TRUE,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}
#' @describeIn analyze_variables `r lifecycle::badge("deprecated")` Use `analyze_vars` instead.
summarize_vars <- function(...) {
  lifecycle::deprecate_warn(when = "0.8.5.9010", "summarize_vars()", "analyze_vars()")
  analyze_vars(...)
}
