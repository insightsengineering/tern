# Helper functions for tabulating biomarker effects on binary response by subgroup

**\[stable\]**

Helper functions which are documented here separately to not confuse the
user when reading about the user-facing functions.

## Usage

``` r
h_rsp_to_logistic_variables(variables, biomarker)

h_logistic_mult_cont_df(variables, data, control = control_logistic())
```

## Arguments

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- biomarker:

  (`string`)  
  the name of the biomarker variable.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- control:

  (named `list`)  
  controls for the response definition and the confidence level produced
  by
  [`control_logistic()`](https://insightsengineering.github.io/tern/reference/control_logistic.md).

## Value

- `h_rsp_to_logistic_variables()` returns a named `list` of elements
  `response`, `arm`, `covariates`, and `strata`.

&nbsp;

- `h_logistic_mult_cont_df()` returns a `data.frame` containing
  estimates and statistics for the selected biomarkers.

## Functions

- `h_rsp_to_logistic_variables()`: helps with converting the "response"
  function variable list to the "logistic regression" variable list. The
  reason is that currently there is an inconsistency between the
  variable names accepted by
  [`extract_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/extract_rsp_subgroups.md)
  and
  [`fit_logistic()`](https://insightsengineering.github.io/tern/reference/fit_logistic.md).

- `h_logistic_mult_cont_df()`: prepares estimates for number of
  responses, patients and overall response rate, as well as odds ratio
  estimates, confidence intervals and p-values, for multiple biomarkers
  in a given single data set. `variables` corresponds to names of
  variables found in `data`, passed as a named list and requires
  elements `rsp` and `biomarkers` (vector of continuous biomarker
  variables) and optionally `covariates` and `strata`.

## Examples

``` r
library(dplyr)
library(forcats)

adrs <- tern_ex_adrs
adrs_labels <- formatters::var_labels(adrs)

adrs_f <- adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  mutate(rsp = AVALC == "CR")
formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")

# This is how the variable list is converted internally.
h_rsp_to_logistic_variables(
  variables = list(
    rsp = "RSP",
    covariates = c("A", "B"),
    strata = "D"
  ),
  biomarker = "AGE"
)
#> $response
#> [1] "RSP"
#> 
#> $arm
#> [1] "AGE"
#> 
#> $covariates
#> [1] "A" "B"
#> 
#> $strata
#> [1] "D"
#> 

# For a single population, estimate separately the effects
# of two biomarkers.
df <- h_logistic_mult_cont_df(
  variables = list(
    rsp = "rsp",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX"
  ),
  data = adrs_f
)
df
#>   biomarker              biomarker_label n_tot n_rsp prop        or       lcl
#> 1    BMRKR1 Continuous Level Biomarker 1   200   164 0.82 0.9755036 0.8804862
#> 2       AGE                          Age   200   164 0.82 0.9952416 0.9462617
#>        ucl conf_level      pval     pval_label
#> 1 1.080775       0.95 0.6352602 p-value (Wald)
#> 2 1.046757       0.95 0.8530389 p-value (Wald)

# If the data set is empty, still the corresponding rows with missings are returned.
h_coxreg_mult_cont_df(
  variables = list(
    rsp = "rsp",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX",
    strata = "STRATA1"
  ),
  data = adrs_f[NULL, ]
)
#>   biomarker              biomarker_label n_tot n_tot_events median hr lcl ucl
#> 1    BMRKR1 Continuous Level Biomarker 1     0            0     NA NA  NA  NA
#> 2       AGE                          Age     0            0     NA NA  NA  NA
#>   conf_level pval     pval_label
#> 1       0.95   NA p-value (Wald)
#> 2       0.95   NA p-value (Wald)
```
