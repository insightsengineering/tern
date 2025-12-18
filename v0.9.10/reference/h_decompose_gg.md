# `ggplot` decomposition

**\[deprecated\]**

The elements composing the `ggplot` are extracted and organized in a
`list`.

## Usage

``` r
h_decompose_gg(gg)
```

## Arguments

- gg:

  (`ggplot`)  
  a graphic to decompose.

## Value

A named `list` with elements:

- `panel`: The panel.

- `yaxis`: The y-axis.

- `xaxis`: The x-axis.

- `xlab`: The x-axis label.

- `ylab`: The y-axis label.

- `guide`: The legend.

## Examples

``` r
# \donttest{
library(dplyr)
library(survival)
library(grid)

fit_km <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
data_plot <- h_data_plot(fit_km = fit_km)
xticks <- h_xticks(data = data_plot)
gg <- h_ggkm(
  data = data_plot,
  yval = "Survival",
  censor_show = TRUE,
  xticks = xticks, xlab = "Days", ylab = "Survival Probability",
  title = "tt",
  footnotes = "ff"
)
#> Warning: `h_ggkm()` was deprecated in tern 0.9.4.
#> ℹ `g_km` now generates `ggplot` objects. This function is no longer used within
#>   `tern`.

g_el <- h_decompose_gg(gg)
#> Warning: `h_decompose_gg()` was deprecated in tern 0.9.4.
#> ℹ `g_km` now generates `ggplot` objects. This function is no longer used within
#>   `tern`.
grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "red", fill = "gray85", lwd = 5))
grid::grid.draw(g_el$panel)


grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "royalblue", fill = "gray85", lwd = 5))
grid::grid.draw(with(g_el, cbind(ylab, yaxis)))

# }
```
