# Multivariate logistic regression table

**\[stable\]**

Layout-creating function which summarizes a logistic variable regression
for binary outcome with categorical/continuous covariates in model
statement. For each covariate category (if categorical) or specified
values (if continuous), present degrees of freedom, regression parameter
estimate and standard error (SE) relative to reference group or
category. Report odds ratios for each covariate category or specified
values and corresponding Wald confidence intervals as default but allow
user to specify other confidence levels. Report p-value for Wald
chi-square test of the null hypothesis that covariate has no effect on
response in model containing all specified covariates. Allow option to
include one two-way interaction and present similar output for each
interaction degree of freedom.

## Usage

``` r
summarize_logistic(
  lyt,
  conf_level,
  drop_and_remove_str = "",
  .indent_mods = NULL
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- drop_and_remove_str:

  (`string`)  
  string to be dropped and removed.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

## Value

A layout object suitable for passing to further layouting functions, or
to
[`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
Adding this function to an `rtable` layout will add a logistic
regression variable summary to the table layout.

## Note

For the formula, the variable names need to be standard `data.frame`
column names without special characters.

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

# flagging empty strings with "_"
df <- df_explicit_na(df, na_level = "_")
df2 <- df_explicit_na(df2, na_level = "_")

result1 <- basic_table() %>%
  summarize_logistic(
    conf_level = 0.95,
    drop_and_remove_str = "_"
  ) %>%
  build_table(df = df)
result1
#>                                       Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 95% CI     p-value
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Planned Arm Code                              2                                                                                 0.3004 
#>   Reference ARM A, n = 64                                                                                                              
#>   ARM B, n = 68                               1                  -1.775             1.144           0.17       (<0.01, 3.23)    0.1209 
#>   ARM C, n = 52                               1                  17.192            3626.588       >999.99     (0.00, >999.99)   0.9962 
#> Age                                                                                                                                    
#>   Age                                         1                  0.170              0.095           1.19       (0.93, 1.51)     0.0746 
#> Race                                          2                                                                                 0.7967 
#>   Reference ASIAN, n = 110                                                                                                             
#>   BLACK OR AFRICAN AMERICAN, n = 40           1                  17.923            4001.705       >999.99     (0.00, >999.99)   0.9964 
#>   WHITE, n = 34                               1                  -0.656             0.974           0.52       (0.04, 6.37)     0.5002 

result2 <- basic_table() %>%
  summarize_logistic(
    conf_level = 0.95,
    drop_and_remove_str = "_"
  ) %>%
  build_table(df = df2)
result2
#>                                         Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 95% CI     p-value
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Race                                            2                                                                                 0.9361 
#>   Reference ASIAN, n = 110                                                                                                               
#>   BLACK OR AFRICAN AMERICAN, n = 40             1                  18.150            3944.701       >999.99     (0.00, >999.99)   0.9963 
#>   WHITE, n = 34                                 1                  -0.373             1.026           0.69       (0.05, 9.68)     0.7164 
#> Planned Arm Code                                2                                                                                 0.2539 
#>   Reference ARM A, n = 64                                                                                                                
#>   ARM B, n = 68                                 1                 -11.527             6.962                                       0.0978 
#>     Age                                                                                                                                  
#>       35                                                                                              0.48       (0.01, 15.48)           
#>   ARM C, n = 52                                 1                  16.333           20278.387                                     0.9994 
#>     Age                                                                                                                                  
#>       35                                                                                            >999.99     (0.00, >999.99)          
#> Age                                                                                                                                      
#>   Age                                           1                  -0.039             0.151                                       0.7981 
#>     Planned Arm Code                                                                                                                     
#>       ARM A                                                                                           0.96       (0.65, 1.42)            
#>       ARM B                                                                                           1.31       (0.92, 1.86)            
#>       ARM C                                                                                           0.99      (0.00, >999.99)          
#> Interaction of Planned Arm Code * Age           2                                                                                 0.3276 
#>   Reference ARM A, n = 64                                                                                                                
#>   ARM B, n = 68                                 1                  0.308              0.206                                       0.1352 
#>   ARM C, n = 52                                 1                  0.029             548.592                                      1.0000 
```
