# Helper functions for multivariate logistic regression

**\[stable\]**

Helper functions used in calculations for logistic regression.

## Usage

``` r
h_get_interaction_vars(fit_glm)

h_interaction_coef_name(
  interaction_vars,
  first_var_with_level,
  second_var_with_level
)

h_or_cat_interaction(
  odds_ratio_var,
  interaction_var,
  fit_glm,
  conf_level = 0.95
)

h_or_cont_interaction(
  odds_ratio_var,
  interaction_var,
  fit_glm,
  at = NULL,
  conf_level = 0.95
)

h_or_interaction(
  odds_ratio_var,
  interaction_var,
  fit_glm,
  at = NULL,
  conf_level = 0.95
)

h_simple_term_labels(terms, table)

h_interaction_term_labels(terms1, terms2, table, any = FALSE)

h_glm_simple_term_extract(x, fit_glm)

h_glm_interaction_extract(x, fit_glm)

h_glm_inter_term_extract(odds_ratio_var, interaction_var, fit_glm, ...)

h_logistic_simple_terms(x, fit_glm, conf_level = 0.95)

h_logistic_inter_terms(x, fit_glm, conf_level = 0.95, at = NULL)
```

## Arguments

- fit_glm:

  (`glm`)  
  logistic regression model fitted by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with "binomial"
  family. Limited functionality is also available for conditional
  logistic regression models fitted by
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html),
  currently this is used only by
  [`extract_rsp_biomarkers()`](https://insightsengineering.github.io/tern/reference/extract_rsp_biomarkers.md).

- interaction_vars:

  (`character(2)`)  
  interaction variable names.

- first_var_with_level:

  (`character(2)`)  
  the first variable name with the interaction level.

- second_var_with_level:

  (`character(2)`)  
  the second variable name with the interaction level.

- odds_ratio_var:

  (`string`)  
  the odds ratio variable.

- interaction_var:

  (`string`)  
  the interaction variable.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- at:

  (`numeric` or `NULL`)  
  optional values for the interaction variable. Otherwise the median is
  used.

- terms:

  (`character`)  
  simple terms.

- table:

  (`table`)  
  table containing numbers for terms.

- terms1:

  (`character`)  
  terms for first dimension (rows).

- terms2:

  (`character`)  
  terms for second dimension (rows).

- any:

  (`flag`)  
  whether any of `term1` and `term2` can be fulfilled to count the
  number of patients. In that case they can only be scalar (strings).

- x:

  (`character`)  
  a variable or interaction term in `fit_glm` (depending on the helper
  function used).

- ...:

  additional arguments for the lower level functions.

## Value

Vector of names of interaction variables.

Name of coefficient.

Odds ratio.

Odds ratio.

Odds ratio.

Term labels containing numbers of patients.

Term labels containing numbers of patients.

Tabulated main effect results from a logistic regression model.

Tabulated interaction term results from a logistic regression model.

A `data.frame` of tabulated interaction term results from a logistic
regression model.

Tabulated statistics for the given variable(s) from the logistic
regression model.

Tabulated statistics for the given variable(s) from the logistic
regression model.

## Functions

- `h_get_interaction_vars()`: Helper function to extract interaction
  variable names from a fitted model assuming only one interaction term.

- `h_interaction_coef_name()`: Helper function to get the right
  coefficient name from the interaction variable names and the given
  levels. The main value here is that the order of first and second
  variable is checked in the `interaction_vars` input.

- `h_or_cat_interaction()`: Helper function to calculate the odds ratio
  estimates for the case when both the odds ratio and the interaction
  variable are categorical.

- `h_or_cont_interaction()`: Helper function to calculate the odds ratio
  estimates for the case when either the odds ratio or the interaction
  variable is continuous.

- `h_or_interaction()`: Helper function to calculate the odds ratio
  estimates in case of an interaction. This is a wrapper for
  `h_or_cont_interaction()` and `h_or_cat_interaction()`.

- `h_simple_term_labels()`: Helper function to construct term labels
  from simple terms and the table of numbers of patients.

- `h_interaction_term_labels()`: Helper function to construct term
  labels from interaction terms and the table of numbers of patients.

- `h_glm_simple_term_extract()`: Helper function to tabulate the main
  effect results of a (conditional) logistic regression model.

- `h_glm_interaction_extract()`: Helper function to tabulate the
  interaction term results of a logistic regression model.

- `h_glm_inter_term_extract()`: Helper function to tabulate the
  interaction results of a logistic regression model. This basically is
  a wrapper for `h_or_interaction()` and `h_glm_simple_term_extract()`
  which puts the results in the right data frame format.

- `h_logistic_simple_terms()`: Helper function to tabulate the results
  including odds ratios and confidence intervals of simple terms.

- `h_logistic_inter_terms()`: Helper function to tabulate the results
  including odds ratios and confidence intervals of interaction terms.

## Note

We don't provide a function for the case when both variables are
continuous because this does not arise in this table, as the treatment
arm variable will always be involved and categorical.

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

h_glm_simple_term_extract("AGE", mod1)
#>   variable variable_label term term_label interaction interaction_label
#> 1      AGE            Age  AGE        Age                              
#>   reference reference_label  estimate  std_error df     pvalue
#> 1                           0.1698216 0.09524116  1 0.07457501
#>   is_variable_summary is_term_summary
#> 1               FALSE            TRUE
h_glm_simple_term_extract("ARMCD", mod1)
#>   variable   variable_label  term              term_label interaction
#> 1    ARMCD Planned Arm Code ARM A Reference ARM A, n = 64            
#> 2    ARMCD Planned Arm Code ARM B           ARM B, n = 68            
#> 3    ARMCD Planned Arm Code ARM C           ARM C, n = 52            
#>   interaction_label reference reference_label  estimate std_error df    pvalue
#> 1                                                                  2 0.3004308
#> 2                                             -1.774769  1.144405  1 0.1209443
#> 3                                               17.1922  3626.588  1 0.9962176
#>   is_variable_summary is_term_summary
#> 1                TRUE           FALSE
#> 2               FALSE            TRUE
#> 3               FALSE            TRUE

h_glm_interaction_extract("ARMCD:AGE", mod2)
#>    variable                        variable_label  term              term_label
#> 1 ARMCD:AGE Interaction of Planned Arm Code * Age ARM A Reference ARM A, n = 64
#> 2 ARMCD:AGE Interaction of Planned Arm Code * Age ARM B           ARM B, n = 68
#> 3 ARMCD:AGE Interaction of Planned Arm Code * Age ARM C           ARM C, n = 52
#>   interaction interaction_label reference reference_label   estimate std_error
#> 1                                                                             
#> 2                                                          0.3081205 0.2062392
#> 3                                                         0.02948826  548.5923
#>   df    pvalue is_variable_summary is_term_summary
#> 1  2 0.3275837                TRUE           FALSE
#> 2  1 0.1351767               FALSE            TRUE
#> 3  1 0.9999571               FALSE            TRUE

h_glm_inter_term_extract("AGE", "ARMCD", mod2)
#>   variable variable_label term term_label interaction interaction_label
#> 1      AGE            Age  AGE        Age                              
#> 2      AGE            Age  AGE        Age       ARMCD  Planned Arm Code
#> 3      AGE            Age  AGE        Age       ARMCD  Planned Arm Code
#> 4      AGE            Age  AGE        Age       ARMCD  Planned Arm Code
#>   reference reference_label    estimate std_error odds_ratio       lcl      ucl
#> 1                           -0.03873898 0.1514322         NA        NA       NA
#> 2     ARM A           ARM A          NA        NA  0.9620018 0.7149514 1.294420
#> 3     ARM B           ARM B          NA        NA  1.3091545 1.0021802 1.710157
#> 4     ARM C           ARM C          NA        NA  0.9907919 0.0000000      Inf
#>   df   pvalue is_variable_summary is_term_summary is_reference_summary
#> 1  1 0.798092               FALSE            TRUE                FALSE
#> 2 NA       NA               FALSE           FALSE                 TRUE
#> 3 NA       NA               FALSE           FALSE                 TRUE
#> 4 NA       NA               FALSE           FALSE                 TRUE

h_logistic_simple_terms("AGE", mod1)
#>   variable variable_label term term_label interaction interaction_label
#> 1      AGE            Age  AGE        Age                              
#>   reference reference_label  estimate  std_error df     pvalue
#> 1                           0.1698216 0.09524116  1 0.07457501
#>   is_variable_summary is_term_summary odds_ratio       lcl      ucl
#> 1               FALSE            TRUE   1.185093 0.9832935 1.428308
#>                     ci
#> 1 0.9832935, 1.4283084

h_logistic_inter_terms(c("RACE", "AGE", "ARMCD", "AGE:ARMCD"), mod2)
#>        variable                        variable_label                      term
#> 1          RACE                                  Race                     ASIAN
#> 2          RACE                                  Race BLACK OR AFRICAN AMERICAN
#> 3          RACE                                  Race                     WHITE
#> 13        ARMCD                      Planned Arm Code                     ARM A
#> 23        ARMCD                      Planned Arm Code                     ARM B
#> ARM B     ARMCD                      Planned Arm Code                     ARM B
#> 33        ARMCD                      Planned Arm Code                     ARM C
#> ARM C     ARMCD                      Planned Arm Code                     ARM C
#> 11          AGE                                   Age                       AGE
#> 21          AGE                                   Age                       AGE
#> 31          AGE                                   Age                       AGE
#> 4           AGE                                   Age                       AGE
#> 12    AGE:ARMCD Interaction of Planned Arm Code * Age                     ARM A
#> 22    AGE:ARMCD Interaction of Planned Arm Code * Age                     ARM B
#> 32    AGE:ARMCD Interaction of Planned Arm Code * Age                     ARM C
#>                              term_label interaction interaction_label reference
#> 1              Reference ASIAN, n = 110                                        
#> 2     BLACK OR AFRICAN AMERICAN, n = 40                                        
#> 3                         WHITE, n = 34                                        
#> 13              Reference ARM A, n = 64                                        
#> 23                        ARM B, n = 68                                        
#> ARM B                     ARM B, n = 68         AGE               Age        35
#> 33                        ARM C, n = 52                                        
#> ARM C                     ARM C, n = 52         AGE               Age        35
#> 11                                  Age                                        
#> 21                                  Age       ARMCD  Planned Arm Code     ARM A
#> 31                                  Age       ARMCD  Planned Arm Code     ARM B
#> 4                                   Age       ARMCD  Planned Arm Code     ARM C
#> 12              Reference ARM A, n = 64                                        
#> 22                        ARM B, n = 68                                        
#> 32                        ARM C, n = 52                                        
#>       reference_label    estimate std_error df     pvalue odds_ratio        lcl
#> 1                                            2  0.9361139                      
#> 2                        18.15018  3944.701  1  0.9963288   76299564          0
#> 3                      -0.3727152  1.025808  1  0.7163522  0.6888614 0.09224926
#> 13                                           2   0.253914         NA         NA
#> 23                      -11.52715   6.96188  1 0.09777185         NA         NA
#> ARM B              35          NA        NA NA         NA  0.4757148 0.03361147
#> 33                       16.33327  20278.39  1  0.9993573         NA         NA
#> ARM C              35          NA        NA NA         NA   34808099          0
#> 11                    -0.03873898 0.1514322  1   0.798092         NA         NA
#> 21              ARM A          NA        NA NA         NA  0.9620018  0.7149514
#> 31              ARM B          NA        NA NA         NA   1.309155    1.00218
#> 4               ARM C          NA        NA NA         NA  0.9907919          0
#> 12                                          NA         NA         NA         NA
#> 22                      0.3081205 0.2062392  1  0.1351767         NA         NA
#> 32                     0.02948826  548.5923  1  0.9999571         NA         NA
#>            ucl is_variable_summary is_term_summary is_reference_summary
#> 1                             TRUE           FALSE                FALSE
#> 2          Inf               FALSE            TRUE                FALSE
#> 3     5.143998               FALSE            TRUE                FALSE
#> 13          NA                TRUE           FALSE                FALSE
#> 23          NA               FALSE            TRUE                FALSE
#> ARM B 6.732956               FALSE           FALSE                 TRUE
#> 33          NA               FALSE            TRUE                FALSE
#> ARM C      Inf               FALSE           FALSE                 TRUE
#> 11          NA               FALSE            TRUE                FALSE
#> 21     1.29442               FALSE           FALSE                 TRUE
#> 31    1.710157               FALSE           FALSE                 TRUE
#> 4          Inf               FALSE           FALSE                 TRUE
#> 12          NA                TRUE           FALSE                FALSE
#> 22          NA               FALSE            TRUE                FALSE
#> 32          NA               FALSE            TRUE                FALSE
#>                           ci
#> 1                           
#> 2                     0, Inf
#> 3     0.09224926, 5.14399810
#> 13                    NA, NA
#> 23                    NA, NA
#> ARM B 0.03361147, 6.73295611
#> 33                    NA, NA
#> ARM C                 0, Inf
#> 11                    NA, NA
#> 21      0.7149514, 1.2944200
#> 31        1.002180, 1.710157
#> 4                     0, Inf
#> 12                    NA, NA
#> 22                    NA, NA
#> 32                    NA, NA
```
