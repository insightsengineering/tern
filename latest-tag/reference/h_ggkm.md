# Helper function to create a KM plot

**\[deprecated\]**

Draw the Kaplan-Meier plot using `ggplot2`.

## Usage

``` r
h_ggkm(
  data,
  xticks = NULL,
  yval = "Survival",
  censor_show,
  xlab,
  ylab,
  ylim = NULL,
  title,
  footnotes = NULL,
  max_time = NULL,
  lwd = 1,
  lty = NULL,
  pch = 3,
  size = 2,
  col = NULL,
  ci_ribbon = FALSE,
  ggtheme = nestcolor::theme_nest()
)
```

## Arguments

- data:

  (`data.frame`)  
  survival data as pre-processed by `h_data_plot`.

- xticks:

  (`numeric` or `NULL`)  
  numeric vector of tick positions or a single number with spacing
  between ticks on the x-axis. If `NULL` (default),
  [`labeling::extended()`](https://rdrr.io/pkg/labeling/man/extended.html)
  is used to determine optimal tick positions on the x-axis.

- yval:

  (`string`)  
  type of plot, to be plotted on the y-axis. Options are `Survival`
  (default) and `Failure` probability.

- censor_show:

  (`flag`)  
  whether to show censored observations.

- xlab:

  (`string`)  
  x-axis label.

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

- max_time:

  (`numeric(1)`)  
  maximum value to show on x-axis. Only data values less than or up to
  this threshold value will be plotted (defaults to `NULL`).

- lwd:

  (`numeric`)  
  line width. If a vector is given, its length should be equal to the
  number of strata from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- lty:

  (`numeric`)  
  line type. If a vector is given, its length should be equal to the
  number of strata from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- pch:

  (`string`)  
  name of symbol or character to use as point symbol to indicate
  censored cases.

- size:

  (`numeric(1)`)  
  size of censored point symbols.

- col:

  (`character`)  
  lines colors. Length of a vector should be equal to number of strata
  from
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- ci_ribbon:

  (`flag`)  
  whether the confidence interval should be drawn around the
  Kaplan-Meier curve.

- ggtheme:

  (`theme`)  
  a graphical theme as provided by `ggplot2` to format the Kaplan-Meier
  plot.

## Value

A `ggplot` object.

## Examples

``` r
# \donttest{
library(dplyr)
library(survival)

fit_km <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
data_plot <- h_data_plot(fit_km = fit_km)
xticks <- h_xticks(data = data_plot)
gg <- h_ggkm(
  data = data_plot,
  censor_show = TRUE,
  xticks = xticks,
  xlab = "Days",
  yval = "Survival",
  ylab = "Survival Probability",
  title = "Survival"
)
gg

# }
```
