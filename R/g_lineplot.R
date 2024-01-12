#' Line plot with the optional table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Line plot with the optional table.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param alt_counts_df (`data.frame` or `NULL`)\cr data set that will be used (only)
#'   to counts objects in groups for stratification.
#' @param variables (named `character` vector) of variable names in `df` data set. Details are:
#'   * `x` (`character`)\cr name of x-axis variable.
#'   * `y` (`character`)\cr name of y-axis variable.
#'   * `group_var` (`character`)\cr name of grouping variable (or strata), i.e. treatment arm.
#'     Can be `NA` to indicate lack of groups.
#'   * `subject_var` (`character`)\cr name of subject variable. Only applies if `group_var` is
#'      not NULL.
#'   * `paramcd` (`character`)\cr name of the variable for parameter's code. Used for y-axis label and plot's subtitle.
#'     Can be `NA` if `paramcd` is not to be added to the y-axis label or subtitle.
#'   * `y_unit` (`character`)\cr name of variable with units of `y`. Used for y-axis label and plot's subtitle.
#'     Can be `NA` if y unit is not to be added to the y-axis label or subtitle.
#' @param mid (`character` or `NULL`)\cr names of the statistics that will be plotted as midpoints.
#'   All the statistics indicated in `mid` variable must be present in the object returned by `sfun`,
#'   and be of a `double` or `numeric` type vector of length one.
#' @param interval (`character` or `NULL`)\cr names of the statistics that will be plotted as intervals.
#'   All the statistics indicated in `interval` variable must be present in the object returned by `sfun`,
#'   and be of a `double` or `numeric` type vector of length two. Set `interval = NULL` if intervals should not be
#'   added to the plot.
#' @param whiskers (`character`)\cr names of the interval whiskers that will be plotted. Names must match names
#'   of the list element `interval` that will be returned by `sfun` (e.g. `mean_ci_lwr` element of
#'   `sfun(x)[["mean_ci"]]`). It is possible to specify one whisker only, or to suppress all whiskers by setting
#'   `interval = NULL`.
#' @param table (`character` or `NULL`)\cr names of the statistics that will be displayed in the table below the plot.
#'   All the statistics indicated in `table` variable must be present in the object returned by `sfun`.
#' @param sfun (`closure`)\cr the function to compute the values of required statistics. It must return a named `list`
#'   with atomic vectors. The names of the `list` elements refer to the names of the statistics and are used by `mid`,
#'   `interval`, `table`. It must be able to accept as input a vector with data for which statistics are computed.
#' @param ... optional arguments to `sfun`.
#' @param mid_type (`character`)\cr controls the type of the `mid` plot, it can be point (`p`), line (`l`),
#'   or point and line (`pl`).
#' @param mid_point_size (`integer` or `double`)\cr controls the font size of the point for `mid` plot.
#' @param position (`character` or `call`)\cr geom element position adjustment, either as a string, or the result of
#'   a call to a position adjustment function.
#' @param legend_title (`character` string)\cr legend title.
#' @param legend_position (`character`)\cr the position of the plot legend (`none`, `left`, `right`, `bottom`, `top`,
#'   or two-element numeric vector).
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to control styling of the plot.
#' @param x_lab (`character`)\cr x-axis label. If equal to `NULL`, then no label will be added.
#' @param y_lab (`character`)\cr y-axis label. If equal to `NULL`, then no label will be added.
#' @param y_lab_add_paramcd (`logical`)\cr should `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` be added to the
#'   y-axis label `y_lab`?
#' @param y_lab_add_unit (`logical`)\cr should y unit, i.e. `unique(df[[variables["y_unit"]]])` be added to the y-axis
#'   label `y_lab`?
#' @param title (`character`)\cr plot title.
#' @param subtitle (`character`)\cr plot subtitle.
#' @param subtitle_add_paramcd (`logical`)\cr should `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` be added to
#'   the plot's subtitle `subtitle`?
#' @param subtitle_add_unit (`logical`)\cr should y unit, i.e. `unique(df[[variables["y_unit"]]])` be added to the
#'   plot's subtitle `subtitle`?
#' @param caption (`character`)\cr optional caption below the plot.
#' @param table_format (named `character` or `NULL`)\cr format patterns for descriptive statistics used in the
#'   (optional) table appended to the plot. It is passed directly to the `h_format_row` function through the `format`
#'   parameter. Names of `table_format` must match the names of statistics returned by `sfun` function.
#' @param table_labels (named `character` or `NULL`)\cr labels for descriptive statistics used in the (optional) table
#'   appended to the plot. Names of `table_labels` must match the names of statistics returned by `sfun` function.
#' @param table_font_size (`integer` or `double`)\cr controls the font size of values in the table.
#' @param newpage (`logical`)\cr should plot be drawn on new page?
#' @param col (`character`)\cr colors.
#'
#' @return A `ggplot` line plot (and statistics table if applicable).
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tern_ex_adsl
#' adlb <- tern_ex_adlb %>% dplyr::filter(ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
#' adlb$AVISIT <- droplevels(adlb$AVISIT)
#' adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))
#'
#' # Mean with CI
#' g_lineplot(adlb, adsl, subtitle = "Laboratory Test:")
#'
#' # Mean with CI, no stratification with group_var
#' g_lineplot(adlb, variables = control_lineplot_vars(group_var = NA))
#'
#' # Mean, upper whisker of CI, no group_var(strata) counts N
#' g_lineplot(
#'   adlb,
#'   whiskers = "mean_ci_upr",
#'   title = "Plot of Mean and Upper 95% Confidence Limit by Visit"
#' )
#'
#' # Median with CI
#' g_lineplot(
#'   adlb,
#'   adsl,
#'   mid = "median",
#'   interval = "median_ci",
#'   whiskers = c("median_ci_lwr", "median_ci_upr"),
#'   title = "Plot of Median and 95% Confidence Limits by Visit"
#' )
#'
#' # Mean, +/- SD
#' g_lineplot(adlb, adsl,
#'   interval = "mean_sdi",
#'   whiskers = c("mean_sdi_lwr", "mean_sdi_upr"),
#'   title = "Plot of Median +/- SD by Visit"
#' )
#'
#' # Mean with CI plot with stats table
#' g_lineplot(adlb, adsl, table = c("n", "mean", "mean_ci"))
#'
#' # Mean with CI, table and customized confidence level
#' g_lineplot(
#'   adlb,
#'   adsl,
#'   table = c("n", "mean", "mean_ci"),
#'   control = control_analyze_vars(conf_level = 0.80),
#'   title = "Plot of Mean and 80% Confidence Limits by Visit"
#' )
#'
#' # Mean with CI, table, filtered data
#' adlb_f <- dplyr::filter(adlb, ARMCD != "ARM A" | AVISIT == "BASELINE")
#' g_lineplot(adlb_f, table = c("n", "mean"))
#'
#' @export
g_lineplot <- function(df,
                       alt_counts_df = NULL,
                       variables = control_lineplot_vars(),
                       mid = "mean",
                       interval = "mean_ci",
                       whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                       table = NULL,
                       sfun = tern::s_summary,
                       ...,
                       mid_type = "pl",
                       mid_point_size = 2,
                       position = ggplot2::position_dodge(width = 0.4),
                       legend_title = NULL,
                       legend_position = "bottom",
                       ggtheme = nestcolor::theme_nest(),
                       x_lab = obj_label(df[[variables[["x"]]]]),
                       y_lab = NULL,
                       y_lab_add_paramcd = TRUE,
                       y_lab_add_unit = TRUE,
                       title = "Plot of Mean and 95% Confidence Limits by Visit",
                       subtitle = "",
                       subtitle_add_paramcd = TRUE,
                       subtitle_add_unit = TRUE,
                       caption = NULL,
                       table_format = summary_formats(),
                       table_labels = summary_labels(),
                       table_font_size = 3,
                       newpage = TRUE,
                       col = NULL) {
  checkmate::assert_character(variables, any.missing = TRUE)
  checkmate::assert_character(mid, null.ok = TRUE)
  checkmate::assert_character(interval, null.ok = TRUE)
  checkmate::assert_character(col, null.ok = TRUE)

  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(subtitle, null.ok = TRUE)

  if (is.character(interval)) {
    checkmate::assert_vector(whiskers, min.len = 0, max.len = 2)
  }

  if (length(whiskers) == 1) {
    checkmate::assert_character(mid)
  }

  if (is.character(mid)) {
    checkmate::assert_scalar(mid_type)
    checkmate::assert_subset(mid_type, c("pl", "p", "l"))
  }

  x <- variables[["x"]]
  y <- variables[["y"]]
  paramcd <- variables["paramcd"] # NA if paramcd == NA or it is not in variables
  y_unit <- variables["y_unit"] # NA if y_unit == NA or it is not in variables
  if (is.na(variables["group_var"])) {
    group_var <- NULL # NULL if group_var == NA or it is not in variables
  } else {
    group_var <- variables[["group_var"]]
    subject_var <- variables[["subject_var"]]
  }
  checkmate::assert_flag(y_lab_add_paramcd, null.ok = TRUE)
  checkmate::assert_flag(subtitle_add_paramcd, null.ok = TRUE)
  if ((!is.null(y_lab) && y_lab_add_paramcd) || (!is.null(subtitle) && subtitle_add_paramcd)) {
    checkmate::assert_false(is.na(paramcd))
    checkmate::assert_scalar(unique(df[[paramcd]]))
  }

  checkmate::assert_flag(y_lab_add_unit, null.ok = TRUE)
  checkmate::assert_flag(subtitle_add_unit, null.ok = TRUE)
  if ((!is.null(y_lab) && y_lab_add_unit) || (!is.null(subtitle) && subtitle_add_unit)) {
    checkmate::assert_false(is.na(y_unit))
    checkmate::assert_scalar(unique(df[[y_unit]]))
  }

  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    checkmate::assert_set_equal(unique(alt_counts_df[[group_var]]), unique(df[[group_var]]))
  }

  ####################################### |
  # ---- Compute required statistics ----
  ####################################### |
  if (!is.null(group_var)) {
    df_grp <- tidyr::expand(df, .data[[group_var]], .data[[x]]) # expand based on levels of factors
  } else {
    df_grp <- tidyr::expand(df, NULL, .data[[x]])
  }
  df_grp <- df_grp %>%
    dplyr::full_join(y = df[, c(group_var, x, y)], by = c(group_var, x), multiple = "all") %>%
    dplyr::group_by_at(c(group_var, x))

  df_stats <- df_grp %>%
    dplyr::summarise(
      data.frame(t(do.call(c, unname(sfun(.data[[y]], ...)[c(mid, interval)])))),
      .groups = "drop"
    )

  df_stats <- df_stats[!is.na(df_stats[[mid]]), ]

  # add number of objects N in group_var (strata)
  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    strata_N <- paste0(group_var, "_N") # nolint

    df_N <- stats::aggregate(eval(parse(text = subject_var)) ~ eval(parse(text = group_var)), data = alt_counts_df, FUN = function(x) length(unique(x))) # nolint
    colnames(df_N) <- c(group_var, "N") # nolint
    df_N[[strata_N]] <- paste0(df_N[[group_var]], " (N = ", df_N$N, ")") # nolint

    # strata_N should not be in clonames(df_stats)
    checkmate::assert_disjunct(strata_N, colnames(df_stats))

    df_stats <- merge(x = df_stats, y = df_N[, c(group_var, strata_N)], by = group_var)
  } else if (!is.null(group_var)) {
    strata_N <- group_var # nolint
  } else {
    strata_N <- NULL # nolint
  }

  ############################################### |
  # ---- Prepare certain plot's properties. ----
  ############################################### |
  # legend title
  if (is.null(legend_title) && !is.null(group_var) && legend_position != "none") {
    legend_title <- attr(df[[group_var]], "label")
  }

  # y label
  if (!is.null(y_lab)) {
    if (y_lab_add_paramcd) {
      y_lab <- paste(y_lab, unique(df[[paramcd]]))
    }

    if (y_lab_add_unit) {
      y_lab <- paste0(y_lab, " (", unique(df[[y_unit]]), ")")
    }

    y_lab <- trimws(y_lab)
  }

  # subtitle
  if (!is.null(subtitle)) {
    if (subtitle_add_paramcd) {
      subtitle <- paste(subtitle, unique(df[[paramcd]]))
    }

    if (subtitle_add_unit) {
      subtitle <- paste0(subtitle, " (", unique(df[[y_unit]]), ")")
    }

    subtitle <- trimws(subtitle)
  }

  ############################### |
  # ---- Build plot object. ----
  ############################### |
  p <- ggplot2::ggplot(
    data = df_stats,
    mapping = ggplot2::aes(
      x = .data[[x]], y = .data[[mid]],
      color = if (is.null(strata_N)) NULL else .data[[strata_N]],
      shape = if (is.null(strata_N)) NULL else .data[[strata_N]],
      lty = if (is.null(strata_N)) NULL else .data[[strata_N]],
      group = if (is.null(strata_N)) NULL else .data[[strata_N]]
    )
  )

  if (!is.null(mid)) {
    # points
    if (grepl("p", mid_type, fixed = TRUE)) {
      p <- p + ggplot2::geom_point(position = position, size = mid_point_size, na.rm = TRUE)
    }

    # lines
    # further conditions in if are to ensure that not all of the groups consist of only one observation
    if (grepl("l", mid_type, fixed = TRUE) && !is.null(group_var) &&
      !all(dplyr::summarise(df_grp, count_n = dplyr::n())[["count_n"]] == 1L)) { # nolint
      p <- p + ggplot2::geom_line(position = position, na.rm = TRUE)
    }
  }

  # interval
  if (!is.null(interval)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[whiskers[1]]], ymax = .data[[whiskers[max(1, length(whiskers))]]]),
        width = 0.45,
        position = position
      )

    if (length(whiskers) == 1) { # lwr or upr only; mid is then required
      # workaround as geom_errorbar does not provide single-direction whiskers
      p <- p +
        ggplot2::geom_linerange(
          data = df_stats[!is.na(df_stats[[whiskers]]), ], # as na.rm =TRUE does not suppress warnings
          ggplot2::aes(ymin = .data[[mid]], ymax = .data[[whiskers]]),
          position = position,
          na.rm = TRUE,
          show.legend = FALSE
        )
    }
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = legend_title,
      lty = legend_title,
      shape = legend_title,
      x = x_lab,
      y = y_lab
    )

  if (!is.null(col)) {
    p <- p +
      ggplot2::scale_color_manual(values = col)
  }

  if (!is.null(ggtheme)) {
    p <- p + ggtheme
  } else {
    p <- p +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.key.width = grid::unit(1, "cm"),
        legend.position = legend_position,
        legend.direction = ifelse(
          legend_position %in% c("top", "bottom"),
          "horizontal",
          "vertical"
        )
      )
  }

  ############################################################# |
  # ---- Optionally, add table to the bottom of the plot. ----
  ############################################################# |
  if (!is.null(table)) {
    df_stats_table <- df_grp %>%
      dplyr::summarise(
        h_format_row(
          x = sfun(.data[[y]], ...)[table],
          format = table_format,
          labels = table_labels
        ),
        .groups = "drop"
      )

    stats_lev <- rev(setdiff(colnames(df_stats_table), c(group_var, x)))

    df_stats_table <- df_stats_table %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(c(group_var, x)),
        names_to = "stat",
        values_to = "value",
        names_ptypes = list(stat = factor(levels = stats_lev))
      )

    tbl <- ggplot2::ggplot(
      df_stats_table,
      ggplot2::aes(x = .data[[x]], y = .data[["stat"]], label = .data[["value"]])
    ) +
      ggplot2::geom_text(size = table_font_size) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5)),
        strip.text = ggplot2::element_text(hjust = 0),
        strip.text.x = ggplot2::element_text(margin = ggplot2::margin(1.5, 0, 1.5, 0, "pt")),
        strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
        legend.position = "none"
      )

    if (!is.null(group_var)) {
      tbl <- tbl + ggplot2::facet_wrap(facets = group_var, ncol = 1)
    }

    # align plot and table
    cowplot::plot_grid(p, tbl, ncol = 1, align = "v", axis = "tblr")
  } else {
    p
  }
}

#' Helper function to get the right formatting in the optional table in `g_lineplot`.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (named `list`)\cr list of numerical values to be formatted and optionally labeled.
#'   Elements of `x` must be `numeric` vectors.
#' @param format (named `character` or `NULL`)\cr format patterns for `x`. Names of the `format` must
#'   match the names of `x`. This parameter is passed directly to the `rtables::format_rcell`
#'   function through the `format` parameter.
#' @param labels (named `character` or `NULL`)\cr optional labels for `x`. Names of the `labels` must
#'   match the names of `x`. When a label is not specified for an element of `x`,
#'   then this function tries to use `label` or `names` (in this order) attribute of that element
#'   (depending on which one exists and it is not `NULL` or `NA` or `NaN`). If none of these attributes
#'   are attached to a given element of `x`, then the label is automatically generated.
#'
#' @return A single row `data.frame` object.
#'
#' @examples
#' mean_ci <- c(48, 51)
#' x <- list(mean = 50, mean_ci = mean_ci)
#' format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
#' labels <- c(mean = "My Mean")
#' h_format_row(x, format, labels)
#'
#' attr(mean_ci, "label") <- "Mean 95% CI"
#' x <- list(mean = 50, mean_ci = mean_ci)
#' h_format_row(x, format, labels)
#'
#' @export
h_format_row <- function(x, format, labels = NULL) {
  # cell: one row, one column data.frame
  format_cell <- function(x, format, label = NULL) {
    fc <- format_rcell(x = x, format = unlist(format))
    if (is.na(fc)) {
      fc <- "NA"
    }
    x_label <- attr(x, "label")
    if (!is.null(label) && !is.na(label)) {
      names(fc) <- label
    } else if (!is.null(x_label) && !is.na(x_label)) {
      names(fc) <- x_label
    } else if (length(x) == length(fc)) {
      names(fc) <- names(x)
    }
    as.data.frame(t(fc))
  }

  row <- do.call(
    cbind,
    lapply(
      names(x), function(xn) format_cell(x[[xn]], format = format[xn], label = labels[xn])
    )
  )

  row
}

#' Control Function for `g_lineplot` Function
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Default values for `variables` parameter in `g_lineplot` function.
#' A variable's default value can be overwritten for any variable.
#'
#' @param x (`character`)\cr x variable name.
#' @param y (`character`)\cr y variable name.
#' @param group_var (`character` or `NA`)\cr group variable name.
#' @param strata (`character` or `NA`)\cr deprecated - group variable name.
#' @param subject_var (`character` or `NA`)\cr subject variable name.
#' @param cohort_id (`character` or `NA`)\cr deprecated - subject variable name.
#' @param paramcd (`character` or `NA`)\cr `paramcd` variable name.
#' @param y_unit (`character` or `NA`)\cr `y_unit` variable name.
#'
#' @return A named character vector of variable names.
#'
#' @examples
#' control_lineplot_vars()
#' control_lineplot_vars(group_var = NA)
#'
#' @export
control_lineplot_vars <- function(x = "AVISIT", y = "AVAL", group_var = "ARM", paramcd = "PARAMCD", y_unit = "AVALU",
                                  subject_var = "USUBJID", strata = lifecycle::deprecated(),
                                  cohort_id = lifecycle::deprecated()) {
  if (lifecycle::is_present(strata)) {
    lifecycle::deprecate_warn("0.9.2", "control_lineplot_vars(strata)", "control_lineplot_vars(group_var)")
    group_var <- strata
  }

  if (lifecycle::is_present(cohort_id)) {
    lifecycle::deprecate_warn("0.9.2", "control_lineplot_vars(cohort_id)", "control_lineplot_vars(subject_id)")
    subject_id <- cohort_id
  }

  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(group_var, na.ok = TRUE)
  checkmate::assert_string(subject_var, na.ok = TRUE)
  checkmate::assert_string(paramcd, na.ok = TRUE)
  checkmate::assert_string(y_unit, na.ok = TRUE)

  variables <- c(x = x, y = y, group_var = group_var, paramcd = paramcd, y_unit = y_unit, subject_var = subject_var)
  return(variables)
}
