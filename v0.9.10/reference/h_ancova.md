# Helper function to return results of a linear model

**\[stable\]**

## Usage

``` r
h_ancova(
  .var,
  .df_row,
  variables,
  interaction_item = NULL,
  weights_emmeans = NULL
)
```

## Arguments

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .df_row:

  (`data.frame`)  
  data set that includes all the variables that are called in `.var` and
  `variables`.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables, with expected elements:

  - `arm` (`string`)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of `arm`
    variable is taken as the reference group.

  - `covariates` (`character`)  
    a vector that can contain single variable names (such as `"X1"`),
    and/or interaction terms indicated by `"X1 * X2"`.

- interaction_item:

  (`string` or `NULL`)  
  name of the variable that should have interactions with arm. if the
  interaction is not needed, the default option is `NULL`.

- weights_emmeans:

  (`string` or `NULL`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)

## Value

The summary of a linear model.

## Examples

``` r
h_ancova(
  .var = "Sepal.Length",
  .df_row = iris,
  variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
)
#>  Species    emmean     SE  df lower.CL upper.CL
#>  setosa       6.15 0.3370 143     5.49     6.82
#>  versicolor   5.72 0.0668 143     5.59     5.85
#>  virginica    5.41 0.1490 143     5.11     5.70
#> 
#> Confidence level used: 0.95 
```
