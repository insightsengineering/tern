# Kaplan-Meier plot

**\[stable\]**

From a survival model, a graphic is rendered along with tabulated
annotation including the number of patient at risk at given time and the
median survival per group.

## Usage

``` r
g_km(
  df,
  variables,
  control_surv = control_surv_timepoint(),
  col = NULL,
  lty = NULL,
  lwd = 0.5,
  censor_show = TRUE,
  pch = 3,
  size = 2,
  max_time = NULL,
  xticks = NULL,
  xlab = "Days",
  yval = c("Survival", "Failure"),
  ylab = paste(yval, "Probability"),
  ylim = NULL,
  title = NULL,
  footnotes = NULL,
  font_size = 10,
  ci_ribbon = FALSE,
  annot_at_risk = TRUE,
  annot_at_risk_title = TRUE,
  annot_surv_med = TRUE,
  annot_coxph = FALSE,
  annot_stats = NULL,
  annot_stats_vlines = FALSE,
  control_coxph_pw = control_coxph(),
  ref_group_coxph = NULL,
  control_annot_surv_med = control_surv_med_annot(),
  control_annot_coxph = control_coxph_annot(),
  legend_pos = NULL,
  rel_height_plot = 0.75,
  ggtheme = NULL,
  as_list = FALSE,
  draw = lifecycle::deprecated(),
  newpage = lifecycle::deprecated(),
  gp = lifecycle::deprecated(),
  vp = lifecycle::deprecated(),
  name = lifecycle::deprecated(),
  annot_coxph_ref_lbls = lifecycle::deprecated(),
  position_coxph = lifecycle::deprecated(),
  position_surv_med = lifecycle::deprecated(),
  width_annots = lifecycle::deprecated()
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- variables:

  (named `list`)  
  variable names. Details are:

  - `tte` (`numeric`)  
    variable indicating time-to-event duration values.

  - `is_event` (`logical`)  
    event variable. `TRUE` if event, `FALSE` if time to event is
    censored.

  - `arm` (`factor`)  
    the treatment group variable.

  - `strata` (`character` or `NULL`)  
    variable names indicating stratification factors.

- control_surv:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_surv_timepoint()`](https://insightsengineering.github.io/tern/reference/control_surv_timepoint.md).
  Some possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level of the interval for survival rate.

  - `conf_type` (`string`)  
    `"plain"` (default), `"log"`, `"log-log"` for confidence interval
    type, see more in
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
    Note that the option "none" is no longer supported.

- col:

  (`character`)  
  lines colors. Length of a vector should be equal to number of strata
  from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- lty:

  (`numeric`)  
  line type. If a vector is given, its length should be equal to the
  number of strata from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- lwd:

  (`numeric`)  
  line width. If a vector is given, its length should be equal to the
  number of strata from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- censor_show:

  (`flag`)  
  whether to show censored observations.

- pch:

  (`string`)  
  name of symbol or character to use as point symbol to indicate
  censored cases.

- size:

  (`numeric(1)`)  
  size of censored point symbols.

- max_time:

  (`numeric(1)`)  
  maximum value to show on x-axis. Only data values less than or up to
  this threshold value will be plotted (defaults to `NULL`).

- xticks:

  (`numeric` or `NULL`)  
  numeric vector of tick positions or a single number with spacing
  between ticks on the x-axis. If `NULL` (default),
  [`labeling::extended()`](https://rdrr.io/pkg/labeling/man/extended.html)
  is used to determine optimal tick positions on the x-axis.

- xlab:

  (`string`)  
  x-axis label.

- yval:

  (`string`)  
  type of plot, to be plotted on the y-axis. Options are `Survival`
  (default) and `Failure` probability.

- ylab:

  (`string`)  
  y-axis label.

- ylim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the y-axis, respectively.
  If `NULL` (default), the default scale range is used.

- title:

  (`string`)  
  plot title.

- footnotes:

  (`string`)  
  plot footnotes.

- font_size:

  (`numeric(1)`)  
  font size to use for all text.

- ci_ribbon:

  (`flag`)  
  whether the confidence interval should be drawn around the
  Kaplan-Meier curve.

- annot_at_risk:

  (`flag`)  
  compute and add the annotation table reporting the number of patient
  at risk matching the main grid of the Kaplan-Meier curve.

- annot_at_risk_title:

  (`flag`)  
  whether the "Patients at Risk" title should be added above the
  `annot_at_risk` table. Has no effect if `annot_at_risk` is `FALSE`.
  Defaults to `TRUE`.

- annot_surv_med:

  (`flag`)  
  compute and add the annotation table on the Kaplan-Meier curve
  estimating the median survival time per group.

- annot_coxph:

  (`flag`)  
  whether to add the annotation table from a
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
  model.

- annot_stats:

  (`string` or `NULL`)  
  statistics annotations to add to the plot. Options are `median`
  (median survival follow-up time) and `min` (minimum survival follow-up
  time).

- annot_stats_vlines:

  (`flag`)  
  add vertical lines corresponding to each of the statistics specified
  by `annot_stats`. If `annot_stats` is `NULL` no lines will be added.

- control_coxph_pw:

  (`list`)  
  parameters for comparison details, specified using the helper function
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).
  Some possible parameter options are:

  - `pval_method` (`string`)  
    p-value method for testing hazard ratio = 1. Default method is
    `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.

  - `ties` (`string`)  
    method for tie handling. Default is `"efron"`, can also be set to
    `"breslow"` or `"exact"`. See more in
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)

  - `conf_level` (`proportion`)  
    confidence level of the interval for HR.

- ref_group_coxph:

  (`string` or `NULL`)  
  level of arm variable to use as reference group in calculations for
  `annot_coxph` table. If `NULL` (default), uses the first level of the
  arm variable.

- control_annot_surv_med:

  (`list`)  
  parameters to control the position and size of the annotation table
  added to the plot when `annot_surv_med = TRUE`, specified using the
  [`control_surv_med_annot()`](https://insightsengineering.github.io/tern/reference/control_annot.md)
  function. Parameter options are: `x`, `y`, `w`, `h`, and `fill`. See
  [`control_surv_med_annot()`](https://insightsengineering.github.io/tern/reference/control_annot.md)
  for details.

- control_annot_coxph:

  (`list`)  
  parameters to control the position and size of the annotation table
  added to the plot when `annot_coxph = TRUE`, specified using the
  [`control_coxph_annot()`](https://insightsengineering.github.io/tern/reference/control_annot.md)
  function. Parameter options are: `x`, `y`, `w`, `h`, `fill`, and
  `ref_lbls`. See
  [`control_coxph_annot()`](https://insightsengineering.github.io/tern/reference/control_annot.md)
  for details.

- legend_pos:

  (`numeric(2)` or `NULL`)  
  vector containing x- and y-coordinates, respectively, for the legend
  position relative to the KM plot area. If `NULL` (default), the legend
  is positioned in the bottom right corner of the plot, or the middle
  right of the plot if needed to prevent overlapping.

- rel_height_plot:

  (`proportion`)  
  proportion of total figure height to allocate to the Kaplan-Meier
  plot. Relative height of patients at risk table is then
  `1 - rel_height_plot`. If `annot_at_risk = FALSE` or `as_list = TRUE`,
  this parameter is ignored.

- ggtheme:

  (`theme`)  
  a graphical theme as provided by `ggplot2` to format the Kaplan-Meier
  plot.

- as_list:

  (`flag`)  
  whether the two `ggplot` objects should be returned as a list when
  `annot_at_risk = TRUE`. If `TRUE`, a named list with two elements,
  `plot` and `table`, will be returned. If `FALSE` (default) the
  patients at risk table is printed below the plot via
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

- draw:

  **\[deprecated\]** This function no longer generates `grob` objects.

- newpage:

  **\[deprecated\]** This function no longer generates `grob` objects.

- gp:

  **\[deprecated\]** This function no longer generates `grob` objects.

- vp:

  **\[deprecated\]** This function no longer generates `grob` objects.

- name:

  **\[deprecated\]** This function no longer generates `grob` objects.

- annot_coxph_ref_lbls:

  **\[deprecated\]** Please use the `ref_lbls` element of
  `control_annot_coxph` instead.

- position_coxph:

  **\[deprecated\]** Please use the `x` and `y` elements of
  `control_annot_coxph` instead.

- position_surv_med:

  **\[deprecated\]** Please use the `x` and `y` elements of
  `control_annot_surv_med` instead.

- width_annots:

  **\[deprecated\]** Please use the `w` element of
  `control_annot_surv_med` (for `surv_med`) and `control_annot_coxph`
  (for `coxph`)."

## Value

A `ggplot` Kaplan-Meier plot and (optionally) summary table.

## Examples

``` r
library(dplyr)

df <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(is_event = CNSR == 0)
variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

# Basic examples
g_km(df = df, variables = variables)

g_km(df = df, variables = variables, yval = "Failure")


# Examples with customization parameters applied
g_km(
  df = df,
  variables = variables,
  control_surv = control_surv_timepoint(conf_level = 0.9),
  col = c("grey25", "grey50", "grey75"),
  annot_at_risk_title = FALSE,
  lty = 1:3,
  font_size = 8
)

g_km(
  df = df,
  variables = variables,
  annot_stats = c("min", "median"),
  annot_stats_vlines = TRUE,
  max_time = 3000,
  ggtheme = ggplot2::theme_minimal()
)


# Example with pairwise Cox-PH analysis annotation table, adjusted annotation tables
g_km(
  df = df, variables = variables,
  annot_coxph = TRUE,
  control_coxph = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99),
  control_annot_coxph = control_coxph_annot(x = 0.26, w = 0.35),
  control_annot_surv_med = control_surv_med_annot(x = 0.8, y = 0.9, w = 0.35)
)

```
