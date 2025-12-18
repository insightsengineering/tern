# Draw `grob`

**\[deprecated\]**

Draw grob on device page.

## Usage

``` r
draw_grob(grob, newpage = TRUE, vp = NULL)
```

## Arguments

- grob:

  (`grob`)  
  grid object.

- newpage:

  (`flag`)  
  draw on a new page.

- vp:

  (`viewport` or `NULL`)  
  a [`viewport()`](https://rdrr.io/r/grid/viewport.html) object (or
  `NULL`).

## Value

A `grob`.

## Examples

``` r
library(dplyr)
library(grid)

# \donttest{
rect <- rectGrob(width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"))
rect %>% draw_grob(vp = grid::viewport(angle = 45))


num <- lapply(1:10, textGrob)
num %>%
  arrange_grobs(grobs = .) %>%
  draw_grob()
#> Warning: `stack_grobs()` was deprecated in tern 0.9.4.
#> ℹ `tern` plotting functions no longer generate `grob` objects.
#> ℹ The deprecated feature was likely used in the tern package.
#>   Please report the issue at
#>   <https://github.com/insightsengineering/tern/issues>.
showViewport()

# }
```
