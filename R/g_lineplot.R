#' Line plot with the optional table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Line plot with the optional table
#'
#' @param df (`data frame`) \cr data set containing all analysis variables.
#' @param alt_counts_df (`data frame` or `NULL`) \cr
#'  data set that will be used (only) to counts objects in strata.
#' @param variables (named `character` vector) of variable names in `df` data set. Details are: \cr
#' * `x`: x-axis variable.
#' * `y`: y-axis variable.
#' * `strata`: grouping variable, i.e. treatment arm. Can be `NA` to indicate lack of groups.
#' * `paramcd`: the variable name for parameter's code. Used for y-axis label and plot's subtitle.
#' Can be `NA` if paramcd is not to be added to the y-axis label or subtitle.
#' * `y_unit`: variable with units of `y`. Used for y-axis label and plot's subtitle.
#' Can be `NA` if y unit is not to be added to the y-axis label or subtitle.
#' @param mid (`character` or `NULL`) \cr
#' names of the statistics that will be plotted as midpoints.
#' All the statistics indicated in `mid` variable must be present in the object returned by `sfun`,
#' and be of a `double` or `numeric` type vector of length one.
#' @param interval (`character` or `NULL`) \cr
#' names of the statistics that will be plotted as intervals.
#' All the statistics indicated in `interval` variable must be present in the object returned by `sfun`,
#' and be of a `double` or `numeric` type vector of length two.
#' @param whiskers (`character`) \cr
#' names of the interval whiskers that will be plotted. Must match the `names` attribute of the
#' `interval` element in the list returned by `sfun`.
#' It is possible to specify one whisker only, lower or upper.
#' @param table (`character` or `NULL`) \cr
#' names of the statistics that will be displayed in the table below the plot.
#' All the statistics indicated in `table` variable must be present in the object returned by `sfun`.
#' @param sfun (`closure`) \cr the function to compute the values of required statistics.
#' It must return a named `list` with atomic vectors.
#' The names of the `list` elements refer to the names of the statistics
#' and are used by `mid`, `interval`, `table`.
#' It must be able to accept as input a vector with data for which statistics are computed.
#' @param ... optional arguments to `sfun`.
#' @param mid_type (`character` scalar) \cr
#' controls the type of the `mid` plot, it can be point (`p`), line (`l`), or point and line (`pl`).
#' @param mid_point_size (`integer` or `double`) \cr
#' controls the font size of the point for `mid` plot.
#' @param position (`character` or `call`) \cr
#' geom element position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param legend_title (`character` string) \cr legend title.
#' @param legend_position (`character`) \cr
#' the position of the plot legend (`none`, `left`, `right`, `bottom`, `top`, or two-element numeric vector).
#' @param ggtheme (`theme`) \cr
#' a graphical theme as provided by `ggplot2` to control outlook of the plot.
#' @param y_lab (`character` scalar) \cr y-axis label.
#' If it equals to `NULL`, then no label will be added.
#' @param y_lab_add_paramcd (`logical` scalar) \cr
#' should paramcd, i.e. `unique(df[[variables["paramcd"]]])` be added to the y-axis label `y_lab`?
#' @param y_lab_add_unit (`logical` scalar) \cr
#' should y unit, i.e. `unique(df[[variables["y_unit"]]])` be added to the y-axis label `y_lab`?
#' @param title (`character` scalar) \cr plot title.
#' @param subtitle (`character` scalar) \cr plot subtitle.
#' @param subtitle_add_paramcd (`logical` scalar) \cr
#' should paramcd, i.e. `unique(df[[variables["paramcd"]]])` be added to the plot's subtitle `subtitle`?
#' @param subtitle_add_unit (`logical` scalar) \cr
#' should y unit, i.e. `unique(df[[variables["y_unit"]]])` be added to the plot's subtitle `subtitle`?
#' @param caption (`character` scalar) \cr optional caption below the plot.
#' @param table_format (named `character` or `NULL`) \cr
#' format patterns for descriptive statistics used in the (optional) table appended to the plot.
#' It is passed directly to the `h_format_row` function through the `format` parameter.
#' Names of `table_format` must match the names of statistics returned by `sfun` function.
#' @param table_labels (named `character` or `NULL`) \cr
#' labels for descriptive statistics used in the (optional) table appended to the plot.
#' Names of `table_labels` must match the names of statistics returned by `sfun` function.
#' @param table_font_size (`integer` or `double`) \cr
#' controls the font size of values in the table.
#' @param newpage (`logical` scalar) \cr should plot be drawn on new page?
#'
#' @author Wojciech Wojciak wojciech.wojciak@contractors.roche.com
#'
#' @return \code{ggplot}
#'
#' @export
#'
#' @examples
#' adsl <- scda::synthetic_cdisc_data("latest")$adsl
#' adlb <- scda::synthetic_cdisc_data("latest")$adlb
#' adlb <- dplyr::filter(adlb, ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
#' adlb$AVISIT <- droplevels(adlb$AVISIT)
#' adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))
#'
#' # Mean with CI
#' g_lineplot(adlb, adsl, subtitle = "Laboratory Test:")
#'
#' # Mean with CI, no stratification
#' g_lineplot(adlb, variables = control_lineplot_vars(strata = NA))
#'
#' # Mean, upper whisker of CI, no strata counts N
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
#'   control = control_summarize_vars(conf_level = 0.80),
#'   title = "Plot of Mean and 80% Confidence Limits by Visit"
#' )
#'
#' # Mean with CI, table, filtered data
#' adlb_f <- dplyr::filter(adlb, ARMCD != "ARM A" | AVISIT == "BASELINE")
#' g_lineplot(adlb_f, table = c("n", "mean"))
g_lineplot <- function(df, # nolint
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
                       ggtheme = NULL,
                       y_lab = NULL,
                       y_lab_add_paramcd = TRUE,
                       y_lab_add_unit = TRUE,
                       title = "Plot of Mean and 95% Confidence Limits by Visit",
                       subtitle = "",
                       subtitle_add_paramcd = TRUE,
                       subtitle_add_unit = TRUE,
                       caption = NULL,
                       table_format = tern::summary_formats(),
                       table_labels = tern::summary_labels(),
                       table_font_size = 3,
                       newpage = TRUE) {
  assertthat::assert_that(is.character(variables) || is.na(variables))
  assertthat::assert_that(is.character(mid) || is.null(mid))
  assertthat::assert_that(is.character(interval) || is.null(interval))
  assertthat::assert_that(ifelse(is.character(interval), length(whiskers) <= 2, TRUE))
  assertthat::assert_that(ifelse(length(whiskers) == 1, is.character(mid), TRUE))
  assertthat::assert_that(assertthat::is.string(title) || is.null(title))
  assertthat::assert_that(assertthat::is.string(subtitle) || is.null(subtitle))
  assertthat::assert_that(
    ifelse(
      is.character(mid),
      (length(mid_type) == 1) && mid_type %in% c("pl", "p", "l"),
      TRUE
    )
  )

  x <- variables[["x"]]
  y <- variables[["y"]]
  paramcd <- variables["paramcd"] # NA if paramcd == NA or it is not in variables # nolint
  y_unit <- variables["y_unit"] # NA if y_unit == NA or it is not in variables # nolint
  strata <- if (!is.na(variables["strata"])) variables[["strata"]] # NULL if strata == NA or it is not in variables # nolint

  assertthat::assert_that(
    ifelse(
      (!is.null(y_lab) && y_lab_add_paramcd) || (!is.null(subtitle) && subtitle_add_paramcd),
      !is.na(paramcd),
      TRUE
    )
  )
  assertthat::assert_that(
    ifelse(
      (!is.null(y_lab) && y_lab_add_unit) || (!is.null(subtitle) && subtitle_add_unit),
      !is.na(y_unit),
      TRUE
    )
  )
  assertthat::assert_that(ifelse(!is.na(paramcd), length(unique(df[[paramcd]])) == 1, TRUE))
  assertthat::assert_that(ifelse(!is.na(y_unit), length(unique(df[[y_unit]])) == 1, TRUE))
  assertthat::assert_that(
    ifelse(
      !is.null(strata) && !is.null(alt_counts_df),
      all(unique(alt_counts_df[[strata]]) %in% unique(df[[strata]])),
      TRUE
    )
  )

  ##################################################
  ## Compute required statistics.
  ##################################################
  if (!is.null(strata)) {
    df_grp <- tidyr::expand(df, .data[[strata]], .data[[x]]) # expand based on levels of factors
  } else {
    df_grp <- tidyr::expand(df, NULL, .data[[x]])
  }
  df_grp <- df_grp %>%
    dplyr::full_join(y = df[, c(strata, x, y)], by = c(strata, x)) %>%
    dplyr::group_by_at(c(strata, x))

  df_stats <- df_grp %>%
    dplyr::summarise(
      data.frame(t(do.call(c, unname(sfun(.data[[y]], ...)[c(mid, interval)])))),
      .groups = "drop"
    )

  df_stats <- df_stats %>% dplyr::filter(!is.na(mean))

  # add number of objects N in strata
  if (!is.null(strata) && !is.null(alt_counts_df)) {
    strata_N <- paste0(strata, "_N") # nolint

    df_N <- as.data.frame(table(alt_counts_df[[strata]], exclude = c(NA, NaN, Inf))) # nolint
    colnames(df_N) <- c(strata, "N") # nolint
    df_N[[strata_N]] <- paste0(df_N[[strata]], " (N = ", df_N$N, ")") # nolint

    assertthat::assert_that(!(strata_N %in% colnames(df_stats)))
    df_stats <- merge(x = df_stats, y = df_N[, c(strata, strata_N)], by = strata)
  } else if (!is.null(strata)) {
    strata_N <- strata # nolint
  } else {
    strata_N <- NULL # nolint
  }

  ##################################################
  ## Prepare certain plot's properties.
  ##################################################
  # legend title
  if (is.null(legend_title) && !is.null(strata) && legend_position != "none") {
    legend_title <- attr(df[[strata]], "label")
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

  ##################################################
  ## Build plot object.
  ##################################################
  p <- ggplot2::ggplot(
    data = df_stats,
    mapping = ggplot2::aes_string(
      x = x, y = mid, color = strata_N, shape = strata_N, lty = strata_N, group = strata_N
    )
  )

  if (!is.null(mid)) {
    # points
    if (grepl("p", mid_type, fixed = TRUE)) {
      p <- p + ggplot2::geom_point(position = position, size = mid_point_size, na.rm = TRUE)
    }

    # lines
    # further conditions in if are to ensure that not all of the groups consist of only one observation
    if (grepl("l", mid_type, fixed = TRUE) &&
      !is.null(strata) &&
      !all(dplyr::summarise(df_grp, count_n = dplyr::n())[["count_n"]] == 1L)) {
      p <- p + ggplot2::geom_line(position = position, na.rm = TRUE)
    }
  }

  # interval
  if (!is.null(interval)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes_string(ymin = whiskers[1], ymax = whiskers[max(1, length(whiskers))]),
        width = 0.45,
        position = position
      )

    if (length(whiskers) == 1) { # lwr or upr only; mid is then required
      # workaround as geom_errorbar does not provide single-direction whiskers
      p <- p +
        ggplot2::geom_linerange(
          data = df_stats[!is.na(df_stats[[whiskers]]), ], # as na.rm =TRUE does not suppress warnings
          ggplot2::aes_string(ymin = mid, ymax = whiskers),
          position = position,
          na.rm = TRUE,
          show.legend = FALSE
        )
    }
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(c(0.25, .25))) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = legend_title,
      lty = legend_title,
      shape = legend_title,
      x = attr(df[[x]], "label"),
      y = y_lab
    )

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

  ##################################################
  ## Optionally, add table to the bottom of the plot.
  ##################################################
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

    stats_lev <- rev(setdiff(colnames(df_stats_table), c(strata, x)))

    df_stats_table <- df_stats_table %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(c(strata, x)),
        names_to = "stat",
        values_to = "value",
        names_ptypes = list(stat = factor(levels = stats_lev))
      )

    tbl <- ggplot2::ggplot(df_stats_table, ggplot2::aes_string(x = x, y = "stat", label = "value")) +
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

    if (!is.null(strata)) {
      tbl <- tbl + ggplot2::facet_wrap(facets = strata, ncol = 1)
    }

    # align plot and table
    cowplot::plot_grid(p, tbl, ncol = 1)
  } else {
    p
  }
}

#' Helper function to get the right formatting in the optional table in g_lineplot.
#'
#'
#' @param x (named `list`) \cr list of numerical values to be formatted and optionally labeled.
#' Elements of `x` must be `numeric` vectors.
#' @param format (named `character` or `NULL`) \cr
#' format patterns for `x`. Names of the `format` must match the names of `x`.
#' This parameter is passed directly to the `rtables::format_rcell`
#' function through the `format` parameter.
#' @param labels (named `character` or `NULL`) \cr
#' optional labels for `x`. Names of the `labels` must match the names of `x`.
#' When a label is not specified for an element of `x`,
#' then this function tries to use `label` or `names` (in this order) attribute of that element
#' (depending on which one exists and it is not `NULL` or `NA` or `NaN`).
#' If none of these attributes are attached to a given element of `x`,
#' then the label is automatically generated.
#'
#' @author Wojciech Wojciak wojciech.wojciak@contractors.roche.com
#'
#' @return 1-row \code{data.frame} object
#'
#' @examples
#'
#' mean_ci <- c(48, 51)
#' x <- list(mean = 50, mean_ci = mean_ci)
#' format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
#' labels <- c(mean = "My Mean")
#' tern::h_format_row(x, format, labels)
#'
#' attr(mean_ci, "label") <- "Mean 95% CI"
#' x <- list(mean = 50, mean_ci = mean_ci)
#' tern::h_format_row(x, format, labels)
#'
#' @keywords internal
h_format_row <- function(x, format, labels = NULL) {

  # cell: one row, one column data.frame
  format_cell <- function(x, format, label = NULL) {
    fc <- format_rcell(x = x, format = format)
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

#' Control Function for g_lineplot Function
#'
#'
#' Default values for `variables` parameter in `g_lineplot` function.
#' A variable's default value can be overwritten for any variable.
#'
#' @param x (`character` scalar) x variable name. \cr
#' @param y (`character` scalar) y variable name. \cr
#' @param strata (`character` scalar or `NA`) strata variable name. \cr
#' @param paramcd (`character` scalar or `NA`) paramcd variable name. \cr
#' @param y_unit (`character` scalar or `NA`) y_unit variable name. \cr
#'
#' @return A named character vector with names of variables.
#'
#' @author Wojciech Wojciak wojciech.wojciak@contractors.roche.com
#'
#' @export
#'
#' @examples
#' control_lineplot_vars()
#' control_lineplot_vars(strata = NA)
control_lineplot_vars <- function(x = "AVISIT", y = "AVAL", strata = "ARM", paramcd = "PARAMCD", y_unit = "AVALU") {
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(is.character(y))
  assertthat::assert_that(is.character(strata) || is.na(strata))
  assertthat::assert_that(is.character(paramcd) || is.na(paramcd))
  assertthat::assert_that(is.character(y_unit) || is.na(y_unit))

  variables <- c(x = x, y = y, strata = strata, paramcd = paramcd, y_unit = y_unit)
  return(variables)
}
