# Prepare response data estimates for multiple biomarkers in a single data frame

**\[stable\]**

Prepares estimates for number of responses, patients and overall
response rate, as well as odds ratio estimates, confidence intervals and
p-values, for multiple biomarkers across population subgroups in a
single data frame. `variables` corresponds to the names of variables
found in `data`, passed as a named list and requires elements `rsp` and
`biomarkers` (vector of continuous biomarker variables) and optionally
`covariates`, `subgroups` and `strata`. `groups_lists` optionally
specifies groupings for `subgroups` variables.

## Usage

``` r
extract_rsp_biomarkers(
  variables,
  data,
  groups_lists = list(),
  control = control_logistic(),
  label_all = "All Patients"
)
```

## Arguments

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- control:

  (named `list`)  
  controls for the response definition and the confidence level produced
  by
  [`control_logistic()`](https://insightsengineering.github.io/tern/reference/control_logistic.md).

- label_all:

  (`string`)  
  label for the total population analysis.

## Value

A `data.frame` with columns `biomarker`, `biomarker_label`, `n_tot`,
`n_rsp`, `prop`, `or`, `lcl`, `ucl`, `conf_level`, `pval`, `pval_label`,
`subgroup`, `var`, `var_label`, and `row_type`.

## Note

You can also specify a continuous variable in `rsp` and then use the
`response_definition` control to convert that internally to a logical
variable reflecting binary response.

## See also

[`h_logistic_mult_cont_df()`](https://insightsengineering.github.io/tern/reference/h_response_biomarkers_subgroups.md)
which is used internally.

## Examples

``` r
library(dplyr)
library(forcats)

adrs <- tern_ex_adrs
adrs_labels <- formatters::var_labels(adrs)

adrs_f <- adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  mutate(rsp = AVALC == "CR")

# Typical analysis of two continuous biomarkers `BMRKR1` and `AGE`,
# in logistic regression models with one covariate `RACE`. The subgroups
# are defined by the levels of `BMRKR2`.
df <- extract_rsp_biomarkers(
  variables = list(
    rsp = "rsp",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX",
    subgroups = "BMRKR2"
  ),
  data = adrs_f
)
df
#>   biomarker              biomarker_label n_tot n_rsp      prop        or
#> 1    BMRKR1 Continuous Level Biomarker 1   200   164 0.8200000 0.9755036
#> 2       AGE                          Age   200   164 0.8200000 0.9952416
#> 3    BMRKR1 Continuous Level Biomarker 1    70    53 0.7571429 1.1524547
#> 4       AGE                          Age    70    53 0.7571429 0.9261012
#> 5    BMRKR1 Continuous Level Biomarker 1    68    58 0.8529412 0.8773122
#> 6       AGE                          Age    68    58 0.8529412 0.9867104
#> 7    BMRKR1 Continuous Level Biomarker 1    62    53 0.8548387 0.8792921
#> 8       AGE                          Age    62    53 0.8548387 1.0630262
#>         lcl      ucl conf_level      pval     pval_label     subgroup    var
#> 1 0.8804862 1.080775       0.95 0.6352602 p-value (Wald) All Patients    ALL
#> 2 0.9462617 1.046757       0.95 0.8530389 p-value (Wald) All Patients    ALL
#> 3 0.9462127 1.403650       0.95 0.1584187 p-value (Wald)          LOW BMRKR2
#> 4 0.8487519 1.010500       0.95 0.0844837 p-value (Wald)          LOW BMRKR2
#> 5 0.7277189 1.057657       0.95 0.1699778 p-value (Wald)       MEDIUM BMRKR2
#> 6 0.8798911 1.106498       0.95 0.8189816 p-value (Wald)       MEDIUM BMRKR2
#> 7 0.7189748 1.075357       0.95 0.2103709 p-value (Wald)         HIGH BMRKR2
#> 8 0.9595973 1.177603       0.95 0.2418840 p-value (Wald)         HIGH BMRKR2
#>                      var_label row_type
#> 1                 All Patients  content
#> 2                 All Patients  content
#> 3 Continuous Level Biomarker 2 analysis
#> 4 Continuous Level Biomarker 2 analysis
#> 5 Continuous Level Biomarker 2 analysis
#> 6 Continuous Level Biomarker 2 analysis
#> 7 Continuous Level Biomarker 2 analysis
#> 8 Continuous Level Biomarker 2 analysis

# Here we group the levels of `BMRKR2` manually, and we add a stratification
# variable `STRATA1`. We also here use a continuous variable `EOSDY`
# which is then binarized internally (response is defined as this variable
# being larger than 750).
df_grouped <- extract_rsp_biomarkers(
  variables = list(
    rsp = "EOSDY",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX",
    subgroups = "BMRKR2",
    strata = "STRATA1"
  ),
  data = adrs_f,
  groups_lists = list(
    BMRKR2 = list(
      "low" = "LOW",
      "low/medium" = c("LOW", "MEDIUM"),
      "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
    )
  ),
  control = control_logistic(
    response_definition = "I(response > 750)"
  )
)
df_grouped
#>   biomarker              biomarker_label n_tot n_rsp prop or lcl ucl conf_level
#> 1    BMRKR1 Continuous Level Biomarker 1   200     0    0 NA  NA  NA       0.95
#> 2       AGE                          Age   200     0    0 NA  NA  NA       0.95
#> 3    BMRKR1 Continuous Level Biomarker 1    70     0    0 NA  NA  NA       0.95
#> 4       AGE                          Age    70     0    0 NA  NA  NA       0.95
#> 5    BMRKR1 Continuous Level Biomarker 1   138     0    0 NA  NA  NA       0.95
#> 6       AGE                          Age   138     0    0 NA  NA  NA       0.95
#> 7    BMRKR1 Continuous Level Biomarker 1   200     0    0 NA  NA  NA       0.95
#> 8       AGE                          Age   200     0    0 NA  NA  NA       0.95
#>   pval     pval_label        subgroup    var                    var_label
#> 1   NA p-value (Wald)    All Patients    ALL                 All Patients
#> 2   NA p-value (Wald)    All Patients    ALL                 All Patients
#> 3   NA p-value (Wald)             low BMRKR2 Continuous Level Biomarker 2
#> 4   NA p-value (Wald)             low BMRKR2 Continuous Level Biomarker 2
#> 5   NA p-value (Wald)      low/medium BMRKR2 Continuous Level Biomarker 2
#> 6   NA p-value (Wald)      low/medium BMRKR2 Continuous Level Biomarker 2
#> 7   NA p-value (Wald) low/medium/high BMRKR2 Continuous Level Biomarker 2
#> 8   NA p-value (Wald) low/medium/high BMRKR2 Continuous Level Biomarker 2
#>   row_type
#> 1  content
#> 2  content
#> 3 analysis
#> 4 analysis
#> 5 analysis
#> 6 analysis
#> 7 analysis
#> 8 analysis
```
