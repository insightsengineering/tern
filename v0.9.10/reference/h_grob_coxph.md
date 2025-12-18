# Helper function to create Cox-PH grobs

**\[deprecated\]**

Grob of `rtable` output from
[`h_tbl_coxph_pairwise()`](https://insightsengineering.github.io/tern/reference/h_tbl_coxph_pairwise.md)

## Usage

``` r
h_grob_coxph(
  ...,
  x = 0,
  y = 0,
  width = grid::unit(0.4, "npc"),
  ttheme = gridExtra::ttheme_default(padding = grid::unit(c(1, 0.5), "lines"), core =
    list(bg_params = list(fill = c("grey95", "grey90"), alpha = 0.5)))
)
```

## Arguments

- ...:

  arguments to pass to
  [`h_tbl_coxph_pairwise()`](https://insightsengineering.github.io/tern/reference/h_tbl_coxph_pairwise.md).

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

A `grob` of a table containing statistics `HR`, `XX% CI` (`XX` taken
from `control_coxph_pw`), and `p-value (log-rank)`.

## Examples

``` r
# \donttest{
library(dplyr)
library(survival)
library(grid)

grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
data <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(is_event = CNSR == 0)
tbl_grob <- h_grob_coxph(
  df = data,
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARMCD"),
  control_coxph_pw = control_coxph(conf_level = 0.9), x = 0.5, y = 0.5
)
#> Warning: `h_grob_coxph()` was deprecated in tern 0.9.4.
#> ℹ `g_km` now generates `ggplot` objects. This function is no longer used within
#>   `tern`.
grid::grid.draw(tbl_grob)

# }
```
