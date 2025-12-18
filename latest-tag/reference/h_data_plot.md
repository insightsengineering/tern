# Helper function to tidy survival fit data

**\[stable\]**

Convert the survival fit data into a data frame designed for plotting
within `g_km`.

This starts from the
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
result, and then:

- Post-processes the `strata` column into a factor.

- Extends each stratum by an additional first row with time 0 and
  probability 1 so that downstream plot lines start at those
  coordinates.

- Adds a `censor` column.

- Filters the rows before `max_time`.

## Usage

``` r
h_data_plot(fit_km, armval = "All", max_time = NULL)
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

- max_time:

  (`numeric(1)`)  
  maximum value to show on x-axis. Only data values less than or up to
  this threshold value will be plotted (defaults to `NULL`).

## Value

A `tibble` with columns `time`, `n.risk`, `n.event`, `n.censor`,
`estimate`, `std.error`, `conf.high`, `conf.low`, `strata`, and
`censor`.

## Examples

``` r
library(dplyr)
library(survival)

# Test with multiple arms
tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
  h_data_plot()
#> # A tibble: 203 × 10
#>     time n.risk n.event n.censor estimate std.error conf.high conf.low strata
#>    <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <fct> 
#>  1   0       70       0        0    1        0          1        1     ARM A 
#>  2  10.4     69       1        0    0.986    0.0146     1        0.958 ARM A 
#>  3  20.5     68       1        0    0.971    0.0208     1        0.932 ARM A 
#>  4  21.5     67       1        0    0.957    0.0257     1        0.910 ARM A 
#>  5  25.3     66       0        1    0.957    0.0257     1        0.910 ARM A 
#>  6  48.1     65       1        0    0.942    0.0300     0.999    0.888 ARM A 
#>  7  66.6     64       0        1    0.942    0.0300     0.999    0.888 ARM A 
#>  8  73.9     63       0        1    0.942    0.0300     0.999    0.888 ARM A 
#>  9  78.9     62       0        1    0.942    0.0300     0.999    0.888 ARM A 
#> 10  90.3     61       1        0    0.926    0.0342     0.991    0.866 ARM A 
#> # ℹ 193 more rows
#> # ℹ 1 more variable: censor <dbl>

# Test with single arm
tern_ex_adtte %>%
  filter(PARAMCD == "OS", ARMCD == "ARM B") %>%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
  h_data_plot(armval = "ARM B")
#> # A tibble: 74 × 10
#>      time n.risk n.event n.censor estimate std.error conf.high conf.low strata
#>     <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
#>  1   0        74       0        0    1        0          1        1     ARM B 
#>  2   2.17     73       1        0    0.986    0.0138     1        0.960 ARM B 
#>  3  16.9      72       1        0    0.973    0.0196     1        0.936 ARM B 
#>  4  22.6      71       1        0    0.959    0.0242     1        0.914 ARM B 
#>  5  26.3      70       1        0    0.945    0.0282     0.999    0.894 ARM B 
#>  6  57.5      69       1        0    0.932    0.0317     0.991    0.875 ARM B 
#>  7  80.0      68       1        0    0.918    0.0350     0.983    0.857 ARM B 
#>  8  81.6      67       1        0    0.904    0.0381     0.974    0.839 ARM B 
#>  9  88.6      66       1        0    0.890    0.0411     0.965    0.822 ARM B 
#> 10 102.       65       1        0    0.877    0.0439     0.955    0.804 ARM B 
#> # ℹ 64 more rows
#> # ℹ 1 more variable: censor <dbl>
```
