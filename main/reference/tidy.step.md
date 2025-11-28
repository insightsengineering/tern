# Custom tidy method for STEP results

**\[stable\]**

Tidy the STEP results into a `tibble` format ready for plotting.

## Usage

``` r
# S3 method for class 'step'
tidy(x, ...)
```

## Arguments

- x:

  (`matrix`)  
  results from
  [`fit_survival_step()`](https://insightsengineering.github.io/tern/reference/fit_survival_step.md).

- ...:

  not used.

## Value

A `tibble` with one row per STEP subgroup. The estimates and CIs are on
the HR or OR scale, respectively. Additional attributes carry metadata
also used for plotting.

## See also

[`g_step()`](https://insightsengineering.github.io/tern/reference/g_step.md)
which consumes the result from this function.

## Examples

``` r
library(survival)
lung$sex <- factor(lung$sex)
vars <- list(
  time = "time",
  event = "status",
  arm = "sex",
  biomarker = "age"
)
step_matrix <- fit_survival_step(
  variables = vars,
  data = lung,
  control = c(control_coxph(), control_step(num_points = 10, degree = 2))
)
broom::tidy(step_matrix)
#> # A tibble: 10 × 12
#>    `Percentile Center` `Percentile Lower` `Percentile Upper` `Interval Center`
#>  *               <dbl>              <dbl>              <dbl>             <dbl>
#>  1              0.0909             0                   0.341              50  
#>  2              0.182              0                   0.432              54  
#>  3              0.273              0.0227              0.523              57  
#>  4              0.364              0.114               0.614              59.5
#>  5              0.455              0.205               0.705              62  
#>  6              0.545              0.295               0.795              64  
#>  7              0.636              0.386               0.886              67  
#>  8              0.727              0.477               0.977              69  
#>  9              0.818              0.568               1                  71  
#> 10              0.909              0.659               1                  74  
#> # ℹ 8 more variables: `Interval Lower` <dbl>, `Interval Upper` <dbl>, n <dbl>,
#> #   events <dbl>, `Hazard Ratio` <dbl>, se <dbl>, ci_lower <dbl>,
#> #   ci_upper <dbl>
```
