# Helper functions for Poisson models

**\[experimental\]**

Helper functions that returns the results of
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) when Poisson or
Quasi-Poisson distributions are needed (see `family` parameter), or
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) for
Negative Binomial distributions. Link function for the GLM is `log`.

## Usage

``` r
h_glm_count(.var, .df_row, variables, distribution, weights)

h_glm_poisson(.var, .df_row, variables, weights)

h_glm_quasipoisson(.var, .df_row, variables, weights)

h_glm_negbin(.var, .df_row, variables, weights)
```

## Arguments

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .df_row:

  (`data.frame`)  
  dataset that includes all the variables that are called in `.var` and
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

  - `offset` (`numeric`)  
    a numeric vector or scalar adding an offset.

- distribution:

  (`character`)  
  a character value specifying the distribution used in the regression
  (Poisson, Quasi-Poisson, negative binomial).

- weights:

  (`character`)  
  a character vector specifying weights used in averaging predictions.
  Number of weights must equal the number of levels included in the
  covariates. Weights option passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

## Value

- `h_glm_count()` returns the results of the selected model.

&nbsp;

- `h_glm_poisson()` returns the results of a Poisson model.

&nbsp;

- `h_glm_quasipoisson()` returns the results of a Quasi-Poisson model.

&nbsp;

- `h_glm_negbin()` returns the results of a negative binomial model.

## Functions

- `h_glm_count()`: Helper function to return the results of the selected
  model (Poisson, Quasi-Poisson, negative binomial).

- `h_glm_poisson()`: Helper function to return results of a Poisson
  model.

- `h_glm_quasipoisson()`: Helper function to return results of a
  Quasi-Poisson model.

- `h_glm_negbin()`: Helper function to return results of a negative
  binomial model.

## See also

[summarize_glm_count](https://insightsengineering.github.io/tern/reference/summarize_glm_count.md)
