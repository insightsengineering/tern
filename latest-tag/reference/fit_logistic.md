# Fit for logistic regression

**\[stable\]**

Fit a (conditional) logistic regression model.

## Usage

``` r
fit_logistic(
  data,
  variables = list(response = "Response", arm = "ARMCD", covariates = NULL, interaction =
    NULL, strata = NULL),
  response_definition = "response"
)
```

## Arguments

- data:

  (`data.frame`)  
  the data frame on which the model was fit.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- response_definition:

  (`string`)  
  the definition of what an event is in terms of `response`. This will
  be used when fitting the (conditional) logistic regression model on
  the left hand side of the formula.

## Value

A fitted logistic regression model.

## Model Specification

The `variables` list needs to include the following elements:

- `arm`: Treatment arm variable name.

- `response`: The response arm variable name. Usually this is a 0/1
  variable.

- `covariates`: This is either `NULL` (no covariates) or a character
  vector of covariate variable names.

- `interaction`: This is either `NULL` (no interaction) or a string of a
  single covariate variable name already included in `covariates`. Then
  the interaction with the treatment arm is included in the model.

## Examples

``` r
library(dplyr)

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
```
