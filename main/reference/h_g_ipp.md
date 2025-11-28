# Helper function to create simple line plot over time

**\[stable\]**

Function that generates a simple line plot displaying parameter trends
over time.

## Usage

``` r
h_g_ipp(
  df,
  xvar,
  yvar,
  xlab,
  ylab,
  id_var,
  title = "Individual Patient Plots",
  subtitle = "",
  caption = NULL,
  add_baseline_hline = FALSE,
  yvar_baseline = "BASE",
  ggtheme = nestcolor::theme_nest(),
  col = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- xvar:

  (`string`)  
  time point variable to be plotted on x-axis.

- yvar:

  (`string`)  
  continuous analysis variable to be plotted on y-axis.

- xlab:

  (`string`)  
  plot label for x-axis.

- ylab:

  (`string`)  
  plot label for y-axis.

- id_var:

  (`string`)  
  variable used as patient identifier.

- title:

  (`string`)  
  title for plot.

- subtitle:

  (`string`)  
  subtitle for plot.

- caption:

  (`string`)  
  optional caption below the plot.

- add_baseline_hline:

  (`flag`)  
  adds horizontal line at baseline y-value on plot when `TRUE`.

- yvar_baseline:

  (`string`)  
  variable with baseline values only. Ignored when `add_baseline_hline`
  is `FALSE`.

- ggtheme:

  (`theme`)  
  optional graphical theme function as provided by `ggplot2` to control
  outlook of plot. Use
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  to tweak the display.

- col:

  (`character`)  
  line colors.

## Value

A `ggplot` line plot.

## See also

[`g_ipp()`](https://insightsengineering.github.io/tern/reference/g_ipp.md)
which uses this function.

## Examples

``` r
library(dplyr)

# Select a small sample of data to plot.
adlb <- tern_ex_adlb %>%
  filter(PARAMCD == "ALT", !(AVISIT %in% c("SCREENING", "BASELINE"))) %>%
  slice(1:36)

p <- h_g_ipp(
  df = adlb,
  xvar = "AVISIT",
  yvar = "AVAL",
  xlab = "Visit",
  id_var = "USUBJID",
  ylab = "SGOT/ALT (U/L)",
  add_baseline_hline = TRUE
)
p

```
