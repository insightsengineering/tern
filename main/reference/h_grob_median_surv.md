# Helper function to create survival estimation grobs

**\[deprecated\]**

The survival fit is transformed in a grob containing a table with groups
in rows characterized by N, median and 95% confidence interval.

## Usage

``` r
h_grob_median_surv(
  fit_km,
  armval = "All",
  x = 0.9,
  y = 0.9,
  width = grid::unit(0.3, "npc"),
  ttheme = gridExtra::ttheme_default()
)
```

## Arguments

- fit_km:

  (`survfit`)  
  result of
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- armval:

  (`string`)  
  used as strata name when treatment arm variable only has one level.
  Default is `"All"`.

- x:

  (`proportion`)  
  a value between 0 and 1 specifying x-location.

- y:

  (`proportion`)  
  a value between 0 and 1 specifying y-location.

- width:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width (as a unit) to use when printing the grob.

- ttheme:

  (`list`)  
  see
  [`gridExtra::ttheme_default()`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html).

## Value

A `grob` of a table containing statistics `N`, `Median`, and `XX% CI`
(`XX` taken from `fit_km`).

## Examples

``` r
# \donttest{
library(dplyr)
library(survival)
library(grid)

grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
  h_grob_median_surv() %>%
  grid::grid.draw()
#> Warning: `h_grob_median_surv()` was deprecated in tern 0.9.4.
#> ℹ `g_km` now generates `ggplot` objects. This function is no longer used within
#>   `tern`.

# }
```
