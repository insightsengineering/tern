#' Line plot with the optional table
#'
#'
#' @param df (`data frame` or `tibble`) \cr data set containing all analysis variables.
#' @param alt_counts_df (`data frame` or `tibble` or `NULL`) \cr
#'  data set that will be used (only) to counts objects in strata.
#' @param variables (named `character` vector) of variable names in `df` data set. Details are: \cr
#' * `x`: x-axis variable.
#' * `y`: y-axis variable.
#' * `strata`: grouping variable, i.e. treatment arm. Can be `NULL`.
#' * `ylab`: y-axis label variable. Can be `NULL`.
#' * `y_unit`: variable with units of `y`, units will added to the y-axis label. Can be `NULL`.
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
#' @param ... \cr
#' optional arguments to `sfun`.
#' @param mid_type (`character` scalar) \cr
#' controls the type of the `mid` plot, it can be point (`p`), line (`l`), or point and line (`pl`).
#' @param mid_point_size (`integer` or `double`) \cr
#' controls the font size of the point for `mid` plot.
#' @param position \cr
#' geom element position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param legend_title (`character` string) \cr legend title.
#' @param legend_position (`character`) \cr
#' the position of the plot legend (`none`, `left`, `right`, `bottom`, `top`, or two-element numeric vector).
#' @param ggtheme (`theme`) \cr
#' a graphical theme as provided by `ggplot2` to control outlook of the plot.
#' @param title (`character` scalar) \cr plot title.
#' @param subtitle (`character` scalar) \cr plot subtitle.
#' @param caption (`character` scalar) \cr optional caption below the plot.
#' @param table_format (named `character` or `NULL`) \cr
#' format patterns for descriptive statistics used in the (optional) table appended to the plot.
#' It is passed directly to the \code{\link[tern]{h_format_row}} function through the `format` parameter.
#' Names of `table_format` must match the names of statistics returned by `sfun` function.
#' @param table_labels (named `character` or `NULL`) \cr
#' labels for descriptive statistics used in the (optional) table appended to the plot.
#' Names of `table_labels` must match the names of statistics returned by `sfun` function.
#' @param table_font_size (`integer` or `double`) \cr
#' controls the font size of values in the table.
#' @param newpage (`logical` scalar) \cr should plot be drawn on new page?
#'
#' @import ggplot2
#' @importFrom dplyr group_by_at summarise all_of full_join
#' @importFrom grid grid.newpage unit.pmax
#' @importFrom gridExtra grid.arrange
#'
#' @author Wojciech Wojciak wojciech.wojciak@contractors.roche.com
#'
#' @return \code{ggplot} or \code{gtable} object,
#' depending on whether or not a table is appended to the plot.
#'
#' @export
#'
#' @examples
#'
#' adlb <- scda::synthetic_cdisc_data("latest")$adlb
#' adlb <- dplyr::filter(adlb, ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
#'
#' adsl <- scda::synthetic_cdisc_data("latest")$adsl
#'
#' # Mean with CI
#' g_lineplot(adlb, adsl, subtitle = "Laboratory Test: ALT")
#'
#' # Mean with CI, no stratification
#' g_lineplot(adlb, variables = c(x = "AVISIT", y = "AVAL", y_lab = "PARAMCD", y_unit = "AVALU"))
#'
#' # Mean, upper whisker of CI, no strata counts N
#' g_lineplot(adlb, whiskers = "mean_ci_upr",
#'   title = "Plot of Mean and Upper 95% Confidence Limit by Visit"
#' )
#'
#' # Median with CI
#' g_lineplot(adlb, adsl, mid = "median", interval = "median_ci",
#'   whiskers = c("median_ci_lwr", "median_ci_upr"),
#'   title = "Plot of Median and 95% Confidence Limits by Visit"
#' )
#'
#' # Mean, +/- SD
#' g_lineplot(adlb, adsl, interval = "mean_sdi", whiskers = c("mean_sdi_lwr", "mean_sdi_upr"),
#'   title = "Plot of Median +/- SD by Visit")
#'
#' # Mean with CI plot with stats table
#' g_lineplot(adlb, adsl, table = c("n", "mean", "mean_ci"))
#'
#' # Mean with CI, table and customized confidence level
#' g_lineplot(
#'   adlb, adsl, table = c("n", "mean", "mean_ci"), control = list(conf_level = 0.80),
#'   title = "Plot of Mean and 80% Confidence Limits by Visit"
#' )
#'
#' # Mean with CI, table, filtered data
#' adlb_f <- dplyr::filter(adlb, ARMCD != "ARM A" | AVISIT == "BASELINE")
#' g_lineplot(adlb_f, table = c("n", "mean"))
#'
#'
g_lineplot <- function(df, # nolint
                       alt_counts_df = NULL,
                       variables = c(
                         x = "AVISIT",
                         y = "AVAL",
                         strata = "ARM",
                         ylab = "PARAMCD",
                         y_unit = "AVALU"
                       ),
                       mid = "mean",
                       interval = "mean_ci",
                       whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                       table = NULL,
                       sfun = tern::s_summary,
                       ...,
                       mid_type = "pl",
                       mid_point_size = 2,
                       position = position_dodge(width = 0.4),
                       legend_title = NULL,
                       legend_position = "bottom",
                       ggtheme = NULL,
                       title = "Plot of Mean and 95% Confidence Limits by Visit",
                       subtitle = NULL,
                       caption = NULL,
                       table_format = tern::summary_formats(),
                       table_labels = tern::summary_labels(),
                       table_font_size = 3,
                       newpage = TRUE) {

  assert_that(is.character(mid) || is.null(mid))
  assert_that(is.character(interval) || is.null(interval))
  assert_that(ifelse(is.character(interval), length(whiskers) <= 2, TRUE))
  assert_that(ifelse(length(whiskers) == 1, is.character(mid), TRUE))
  assert_that(is.string(title) || is.null(title))
  assert_that(is.string(subtitle) || is.null(subtitle))
  assert_that(
    ifelse(
      is.character(mid),
      (length(mid_type) == 1) && mid_type %in% c("pl", "p", "l"),
      TRUE
    )
  )
  assert_that(
    is.character(variables),
    all(c("x", "y") %in% names(variables))
  )

  # variables_default <- c( # nolint
  #   x = "AVISIT", # nolint
  #   y = "AVAL", # nolint
  #   strata = "ARM", # nolint
  #   ylab = "PARAMCD", # nolint
  #   y_unit = "AVALU" # nolint
  # ) # nolint
  # variables <- c(variables, variables_default[setdiff(names(variables_default), names(variables))]) # nolint

  x <- variables[["x"]]
  y <- variables[["y"]]
  strata <- if (!is.na(variables["strata"])) variables[["strata"]] # NULL if no strata in variables
  ylab <- if (!is.na(variables["ylab"])) variables[["ylab"]] # NULL if no ylab in variables
  y_unit <- if (!is.na(variables["y_unit"])) variables[["y_unit"]] # NULL if no y_unit in variables

  ##################################################
  ## Compute required statistics.
  ##################################################
  # some partial example of base R solution
  # (does not work when c(mid, interval) is of length one, or strata is NULL)
  # df_stats <- aggregate( # nolint
  #   x = df[[y]], # nolint
  #   by = df[, c(strata, x)], # nolint
  #   FUN = function(val) do.call(c, unname(sfun(val, ...)[c(mid, interval)])) # nolint
  # ) # nolint
  # assert_that(!(any(c(strata, x) %in% colnames(df_stats$x)))) # nolint
  # df_stats <- cbind(df_stats[, c(strata, x)], df_stats$x) # nolint

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

  # add number of objects N in strata
  if (!is.null(strata) && !is.null(alt_counts_df)) {

    strata_N <- paste0(strata, "_N") # nolint

    df_N <- as.data.frame(table(alt_counts_df[[strata]], exclude = c(NA, NaN, Inf))) # nolint
    colnames(df_N) <- c(strata, "N")
    df_N[[strata_N]] <- paste0(df_N[[strata]], " (N = ", df_N$N, ")")

    assert_that(!(strata_N %in% colnames(df_stats)))
    df_stats <- merge(x = df_stats, y = df_N[, c(strata, strata_N)], by = strata)

  } else if (!is.null(strata)) {
    strata_N <- strata # nolint
  } else {
    strata_N <- NULL # nolint
  }

  ##################################################
  ## Prepare certain plot's properties.
  ##################################################
  # y label
  if (!is.null(ylab)) {
    ylab <- unique(df[[ylab]])
    stopifnot("ylab must be in df and df[[ylab]] must be of the same value" = length(ylab) == 1) # nolint
  }

  if (!is.null(y_unit)) {
    y_unit <- unique(df[[y_unit]])
    stopifnot("y_unit must be in df and df[[y_unit]] must be of the same value" = length(y_unit) == 1) # nolint
    ylab <- paste0(ylab, " (", y_unit, ")")
  }

  # legend title
  if (is.null(legend_title) && !is.null(strata) && legend_position != "none") {
    legend_title <- attr(df[[strata]], "label")
  }

  ##################################################
  ## Build plot object.
  ##################################################
  p <- ggplot(
    data = df_stats,
    mapping = aes_string(
      x = x, y = mid, color = strata_N, shape = strata_N, lty = strata_N, group = strata_N
    ))

  if (!is.null(mid)) {
    # points
    if (grepl("p", mid_type, fixed = TRUE)) {
      p <- p + geom_point(position = position, size = mid_point_size, na.rm = TRUE)
    }

    # lines
    if (grepl("l", mid_type, fixed = TRUE)) {
      # for line graphs, the data points must be grouped so that it knows which points to connect.
      m <- if (is.null(strata)) aes(group = 1)
      p <- p + geom_line(mapping = m, position = position, na.rm = TRUE)
    }
  }

  # interval
  if (!is.null(interval)) {

    p <- p +
      geom_errorbar(
        aes_string(ymin = whiskers[1], ymax = whiskers[max(1, length(whiskers))]),
        width = 0.45,
        position = position
      )

    if (length(whiskers) == 1) { # lwr or upr only; mid is then required
      # workaround as geom_errorbar does not provide single-direction whiskers
      p <- p +
        geom_linerange(
          data = df_stats[!is.na(df_stats[[whiskers]]), ], # as na.rm =TRUE does not suppress warnings
          aes_string(ymin = mid, ymax = whiskers),
          position = position,
          na.rm = TRUE,
          show.legend = FALSE
        )
    }
  }

  p <- p +
    scale_y_continuous(labels = scales::comma, expand = expansion(c(0.25, .25))) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = legend_title,
      lty = legend_title,
      shape = legend_title,
      x = attr(df[[x]], "label"),
      y = ylab
    )

  if (!is.null(ggtheme)) {
    p <- p + ggtheme
  } else {
    p <- p +
      theme_bw() +
      theme(
        legend.key.width = unit(1, "cm"),
        legend.position = legend_position,
        legend.direction = ifelse(
          legend_position %in% c("top", "bottom"),
          "horizontal",
          "vertical")
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

    tbl <- ggplot(df_stats_table, aes_string(x = x, y = "stat", label = "value")) +
      geom_text(size = table_font_size) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 5)),
        strip.text = element_text(hjust = 0),
        strip.text.x = element_text(margin = margin(1.5, 0, 1.5, 0, "pt")),
        strip.background = element_rect(fill = "grey95", color = NA),
        legend.position = "none"
      )

    if (!is.null(strata)) {
      tbl <- tbl + facet_wrap(facets = strata, ncol = 1)
    }

    # align plot and table
    p_grob <- ggplotGrob(p)
    tbl_grob <- ggplotGrob(tbl)
    maxWidth <- grid::unit.pmax(p_grob$widths, tbl_grob$widths) # nolint
    p_grob$widths <- maxWidth
    tbl_grob$widths <- maxWidth

    if (newpage) {
      grid::grid.newpage()
    }

    gridExtra::grid.arrange(p_grob, tbl_grob, ncol = 1, heights = c(3, 1))

  }

  else {
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
#' This parameter is passed directly to the \code{\link[rtables]{format_rcell}}
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
#' @export
#'
#' @examples
#'
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
h_format_row <- function(x, format, labels = NULL) {

  # cell: one row, one column data.frame
  format_cell <- function(x, format, label = NULL) {
    fc <- rtables::format_rcell(x = x, format = format)
    if (is.na(fc))
      fc <- "NA"
    x_label <- attr(x, "label")
    if (!is.null(label) && !is.na(label))
      names(fc) <- label
    else if (!is.null(x_label) && !is.na(x_label))
      names(fc) <- x_label
    else if (length(x) == length(fc))
      names(fc) <- names(x)
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
