# Line plot with optional table

**\[stable\]**

Line plot with optional table.

## Usage

``` r
g_lineplot(
  df,
  alt_counts_df = NULL,
  variables = control_lineplot_vars(),
  mid = "mean",
  interval = "mean_ci",
  whiskers = c("mean_ci_lwr", "mean_ci_upr"),
  table = NULL,
  sfun = s_summary,
  ...,
  mid_type = "pl",
  mid_point_size = 2,
  position = ggplot2::position_dodge(width = 0.4),
  legend_title = NULL,
  legend_position = "bottom",
  ggtheme = nestcolor::theme_nest(),
  xticks = NULL,
  xlim = NULL,
  ylim = NULL,
  x_lab = obj_label(df[[variables[["x"]]]]),
  y_lab = NULL,
  y_lab_add_paramcd = TRUE,
  y_lab_add_unit = TRUE,
  title = "Plot of Mean and 95% Confidence Limits by Visit",
  subtitle = "",
  subtitle_add_paramcd = TRUE,
  subtitle_add_unit = TRUE,
  caption = NULL,
  table_format = NULL,
  table_labels = NULL,
  table_font_size = 3,
  errorbar_width = 0.45,
  newpage = lifecycle::deprecated(),
  col = NULL,
  linetype = NULL,
  rel_height_plot = 0.5,
  as_list = FALSE
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- alt_counts_df:

  (`data.frame` or `NULL`)  
  data set that will be used (only) to counts objects in groups for
  stratification.

- variables:

  (named `character`) vector of variable names in `df` which should
  include:

  - `x` (`string`)  
    name of x-axis variable.

  - `y` (`string`)  
    name of y-axis variable.

  - `group_var` (`string` or `NULL`)  
    name of grouping variable (or strata), i.e. treatment arm. Can be
    `NA` to indicate lack of groups.

  - `subject_var` (`string` or `NULL`)  
    name of subject variable. Only applies if `group_var` is not NULL.

  - `paramcd` (`string` or `NA`)  
    name of the variable for parameter's code. Used for y-axis label and
    plot's subtitle. Can be `NA` if `paramcd` is not to be added to the
    y-axis label or subtitle.

  - `y_unit` (`string` or `NA`)  
    name of variable with units of `y`. Used for y-axis label and plot's
    subtitle. Can be `NA` if y unit is not to be added to the y-axis
    label or subtitle.

  - `facet_var` (`string` or `NA`)  
    name of the secondary grouping variable used for plot faceting, i.e.
    treatment arm. Can be `NA` to indicate lack of groups.

- mid:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as midpoints. All the
  statistics indicated in `mid` variable must be present in the object
  returned by `sfun`, and be of a `double` or `numeric` type vector of
  length one.

- interval:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as intervals. All the
  statistics indicated in `interval` variable must be present in the
  object returned by `sfun`, and be of a `double` or `numeric` type
  vector of length two. Set `interval = NULL` if intervals should not be
  added to the plot.

- whiskers:

  (`character`)  
  names of the interval whiskers that will be plotted. Names must match
  names of the list element `interval` that will be returned by `sfun`
  (e.g. `mean_ci_lwr` element of `sfun(x)[["mean_ci"]]`). It is possible
  to specify one whisker only, or to suppress all whiskers by setting
  `interval = NULL`.

- table:

  (`character` or `NULL`)  
  names of the statistics that will be displayed in the table below the
  plot. All the statistics indicated in `table` variable must be present
  in the object returned by `sfun`.

- sfun:

  (`function`)  
  the function to compute the values of required statistics. It must
  return a named `list` with atomic vectors. The names of the `list`
  elements refer to the names of the statistics and are used by `mid`,
  `interval`, `table`. It must be able to accept as input a vector with
  data for which statistics are computed.

- ...:

  optional arguments to `sfun`.

- mid_type:

  (`string`)  
  controls the type of the `mid` plot, it can be point (`"p"`), line
  (`"l"`), or point and line (`"pl"`).

- mid_point_size:

  (`numeric(1)`)  
  font size of the `mid` plot points.

- position:

  (`character` or `call`)  
  geom element position adjustment, either as a string, or the result of
  a call to a position adjustment function.

- legend_title:

  (`string`)  
  legend title.

- legend_position:

  (`string`)  
  the position of the plot legend (`"none"`, `"left"`, `"right"`,
  `"bottom"`, `"top"`, or a two-element numeric vector).

- ggtheme:

  (`theme`)  
  a graphical theme as provided by `ggplot2` to control styling of the
  plot.

- xticks:

  (`numeric` or `NULL`)  
  numeric vector of tick positions or a single number with spacing
  between ticks on the x-axis, for use when `variables$x` is numeric. If
  `NULL` (default),
  [`labeling::extended()`](https://rdrr.io/pkg/labeling/man/extended.html)
  is used to determine optimal tick positions on the x-axis. If
  `variables$x` is not numeric, this argument is ignored.

- xlim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the x-axis, respectively.
  If `NULL` (default), the default scale range is used.

- ylim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the y-axis, respectively.
  If `NULL` (default), the default scale range is used.

- x_lab:

  (`string` or `NULL`)  
  x-axis label. If `NULL` then no label will be added.

- y_lab:

  (`string` or `NULL`)  
  y-axis label. If `NULL` then no label will be added.

- y_lab_add_paramcd:

  (`flag`)  
  whether `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` should be
  added to the y-axis label (`y_lab`).

- y_lab_add_unit:

  (`flag`)  
  whether y-axis unit, i.e. `unique(df[[variables["y_unit"]]])` should
  be added to the y-axis label (`y_lab`).

- title:

  (`string`)  
  plot title.

- subtitle:

  (`string`)  
  plot subtitle.

- subtitle_add_paramcd:

  (`flag`)  
  whether `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` should be
  added to the plot's subtitle (`subtitle`).

- subtitle_add_unit:

  (`flag`)  
  whether the y-axis unit, i.e. `unique(df[[variables["y_unit"]]])`
  should be added to the plot's subtitle (`subtitle`).

- caption:

  (`string`)  
  optional caption below the plot.

- table_format:

  (named `vector` or `NULL`)  
  custom formats for descriptive statistics used instead of defaults in
  the (optional) table appended to the plot. It is passed directly to
  the `h_format_row` function through the `format` parameter. Names of
  `table_format` must match the names of statistics returned by `sfun`
  function. Can be a character vector with values from
  [`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  or custom format functions.

- table_labels:

  (named `character` or `NULL`)  
  labels for descriptive statistics used in the (optional) table
  appended to the plot. Names of `table_labels` must match the names of
  statistics returned by `sfun` function.

- table_font_size:

  (`numeric(1)`)  
  font size of the text in the table.

- errorbar_width:

  (`numeric(1)`)  
  width of the error bars.

- newpage:

  **\[deprecated\]** not used.

- col:

  (`character`)  
  color(s). See
  [`?ggplot2::aes_colour_fill_alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)
  for example values.

- linetype:

  (`character`)  
  line type(s). See
  [`?ggplot2::aes_linetype_size_shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)
  for example values.

- rel_height_plot:

  (`proportion`)  
  proportion of total figure height to allocate to the line plot.
  Relative height of annotation table is then `1 - rel_height_plot`. If
  `table = NULL`, this parameter is ignored.

- as_list:

  (`flag`)  
  whether the two `ggplot` objects should be returned as a list when
  `table` is not `NULL`. If `TRUE`, a named list with two elements,
  `plot` and `table`, will be returned. If `FALSE` (default) the
  annotation table is printed below the plot via
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

## Value

A `ggplot` line plot (and statistics table if applicable).

## Examples

``` r
adsl <- tern_ex_adsl
adlb <- tern_ex_adlb %>% dplyr::filter(ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
adlb$AVISIT <- droplevels(adlb$AVISIT)
adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))

# Mean with CI
g_lineplot(adlb, adsl, subtitle = "Laboratory Test:")


# Mean with CI, no stratification with group_var
g_lineplot(adlb, variables = control_lineplot_vars(group_var = NA))


# Mean, upper whisker of CI, no group_var(strata) counts N
g_lineplot(
  adlb,
  whiskers = "mean_ci_upr",
  title = "Plot of Mean and Upper 95% Confidence Limit by Visit"
)


# Median with CI
g_lineplot(
  adlb,
  adsl,
  mid = "median",
  interval = "median_ci",
  whiskers = c("median_ci_lwr", "median_ci_upr"),
  title = "Plot of Median and 95% Confidence Limits by Visit"
)


# Mean, +/- SD
g_lineplot(adlb, adsl,
  interval = "mean_sdi",
  whiskers = c("mean_sdi_lwr", "mean_sdi_upr"),
  title = "Plot of Median +/- SD by Visit"
)


# Mean with CI plot with stats table
g_lineplot(adlb, adsl, table = c("n", "mean", "mean_ci"))


# Mean with CI, table and customized confidence level
g_lineplot(
  adlb,
  adsl,
  table = c("n", "mean", "mean_ci"),
  control = control_analyze_vars(conf_level = 0.80),
  title = "Plot of Mean and 80% Confidence Limits by Visit"
)


# Mean with CI, table with customized formats/labels
g_lineplot(
  adlb,
  adsl,
  table = c("n", "mean", "mean_ci"),
  table_format = list(
    mean = function(x, ...) {
      ifelse(x < 20, round_fmt(x, digits = 3), round_fmt(x, digits = 2))
    },
    mean_ci = "(xx.xxx, xx.xxx)"
  ),
  table_labels = list(
    mean = "mean",
    mean_ci = "95% CI"
  )
)


# Mean with CI, table, filtered data
adlb_f <- dplyr::filter(adlb, ARMCD != "ARM A" | AVISIT == "BASELINE")
g_lineplot(adlb_f, table = c("n", "mean"))

```
