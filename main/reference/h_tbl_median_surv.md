# Helper function for survival estimations

**\[stable\]**

Transform a survival fit to a table with groups in rows characterized by
N, median and confidence interval.

## Usage

``` r
h_tbl_median_surv(fit_km, armval = "All", digits = 4)
```

## Arguments

- fit_km:

  (`survfit`)\
  result of
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- armval:

  (`string`)\
  used as strata name when treatment arm variable only has one level.
  Default is `"All"`.

- digits:

  (`integer(1)`)\
  number of significant digits for median and CI values. Defaults to
  `4`.

## Value

A summary table with statistics `N`, `Median`, and `XX% CI` (`XX` taken
from `fit_km`).

## Examples

``` r
library(dplyr)
library(survival)

adtte <- tern_ex_adtte |> filter(PARAMCD == "OS")
fit <- survfit(
  formula = Surv(AVAL, 1 - CNSR) ~ ARMCD,
  data = adtte
)
h_tbl_median_surv(fit_km = fit)
#>        N Median        95% CI
#> ARM A 69  974.6   (687, 1625)
#> ARM B 73  727.8 (555.8, 1156)
#> ARM C 58  632.3   (393, 1001)
h_tbl_median_surv(fit_km = fit, digits = 2)
#>        N Median      95% CI
#> ARM A 69    970 (690, 1600)
#> ARM B 73    730 (560, 1200)
#> ARM C 58    630 (390, 1000)
```
