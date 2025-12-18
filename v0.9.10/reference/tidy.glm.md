# Custom tidy method for binomial GLM results

**\[stable\]**

Helper method (for
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html))
to prepare a data frame from a `glm` object with `binomial` family.

## Usage

``` r
# S3 method for class 'glm'
tidy(x, conf_level = 0.95, at = NULL, ...)
```

## Arguments

- x:

  (`glm`)  
  logistic regression model fitted by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with "binomial"
  family.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- at:

  (`numeric` or `NULL`)  
  optional values for the interaction variable. Otherwise the median is
  used.

- ...:

  additional arguments for the lower level functions.

## Value

A `data.frame` containing the tidied model.

## See also

[h_logistic_regression](https://insightsengineering.github.io/tern/reference/h_logistic_regression.md)
for relevant helper functions.

## Examples

``` r
library(dplyr)
library(broom)

adrs_f <- tern_ex_adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
  mutate(
    Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
    RACE = factor(RACE),
    SEX = factor(SEX)
  )
formatters::var_labels(adrs_f) <- c(formatters::var_labels(tern_ex_adrs), Response = "Response")
mod1 <- fit_logistic(
  data = adrs_f,
  variables = list(
    response = "Response",
    arm = "ARMCD",
    covariates = c("AGE", "RACE")
  )
)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
mod2 <- fit_logistic(
  data = adrs_f,
  variables = list(
    response = "Response",
    arm = "ARMCD",
    covariates = c("AGE", "RACE"),
    interaction = "AGE"
  )
)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

df <- tidy(mod1, conf_level = 0.99)
df2 <- tidy(mod2, conf_level = 0.99)
```
