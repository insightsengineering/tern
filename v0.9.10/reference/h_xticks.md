# Helper function to calculate x-tick positions

**\[stable\]**

Calculate the positions of ticks on the x-axis. However, if `xticks`
already exists it is kept as is. It is based on the same function
`ggplot2` relies on, and is required in the graphic and the
patient-at-risk annotation table.

## Usage

``` r
h_xticks(data, xticks = NULL, max_time = NULL)
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

- max_time:

  (`numeric(1)`)  
  maximum value to show on x-axis. Only data values less than or up to
  this threshold value will be plotted (defaults to `NULL`).

## Value

A vector of positions to use for x-axis ticks on a `ggplot` object.

## Examples

``` r
library(dplyr)
library(survival)

data <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
  h_data_plot()

h_xticks(data)
#> [1]    0 1000 2000 3000 4000 5000
h_xticks(data, xticks = seq(0, 3000, 500))
#> [1]    0  500 1000 1500 2000 2500 3000
h_xticks(data, xticks = 500)
#>  [1]    0  500 1000 1500 2000 2500 3000 3500 4000 4500
h_xticks(data, xticks = 500, max_time = 6000)
#>  [1]    0  500 1000 1500 2000 2500 3000 3500 4000 4500 5000 5500 6000
h_xticks(data, xticks = c(0, 500), max_time = 300)
#> [1]   0 500
h_xticks(data, xticks = 500, max_time = 300)
#>  [1]    0  500 1000 1500 2000 2500 3000 3500 4000 4500
```
