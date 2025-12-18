# Muffled `car::Anova`

Applied on survival models,
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) signal that the
`strata` terms is dropped from the model formula when present, this
function deliberately muffles this message.

## Usage

``` r
muffled_car_anova(mod, test_statistic)
```

## Arguments

- mod:

  (`coxph`)  
  Cox regression model fitted by
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

- test_statistic:

  (`string`)  
  the method used for estimation of p.values; `wald` (default) or
  `likelihood`.

## Value

The output of [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html),
with convergence message muffled.
