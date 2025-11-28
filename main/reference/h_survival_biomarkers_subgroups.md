# Helper functions for tabulating biomarker effects on survival by subgroup

**\[stable\]**

Helper functions which are documented here separately to not confuse the
user when reading about the user-facing functions.

## Usage

``` r
h_surv_to_coxreg_variables(variables, biomarker)

h_coxreg_mult_cont_df(variables, data, control = control_coxreg())
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

  (`list`)  
  a list of parameters as returned by the helper function
  [`control_coxreg()`](https://insightsengineering.github.io/tern/reference/control_coxreg.md).

## Value

- `h_surv_to_coxreg_variables()` returns a named `list` of elements
  `time`, `event`, `arm`, `covariates`, and `strata`.

&nbsp;

- `h_coxreg_mult_cont_df()` returns a `data.frame` containing estimates
  and statistics for the selected biomarkers.

## Functions

- `h_surv_to_coxreg_variables()`: Helps with converting the "survival"
  function variable list to the "Cox regression" variable list. The
  reason is that currently there is an inconsistency between the
  variable names accepted by
  [`extract_survival_subgroups()`](https://insightsengineering.github.io/tern/reference/extract_survival_subgroups.md)
  and
  [`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md).

- `h_coxreg_mult_cont_df()`: Prepares estimates for number of events,
  patients and median survival times, as well as hazard ratio estimates,
  confidence intervals and p-values, for multiple biomarkers in a given
  single data set. `variables` corresponds to names of variables found
  in `data`, passed as a named list and requires elements `tte`,
  `is_event`, `biomarkers` (vector of continuous biomarker variables)
  and optionally `subgroups` and `strata`.

## Examples

``` r
library(dplyr)
library(forcats)

adtte <- tern_ex_adtte

# Save variable labels before data processing steps.
adtte_labels <- formatters::var_labels(adtte, fill = FALSE)

adtte_f <- adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVALU = as.character(AVALU),
    is_event = CNSR == 0
  )
labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
formatters::var_labels(adtte_f)[names(labels)] <- labels

# This is how the variable list is converted internally.
h_surv_to_coxreg_variables(
  variables = list(
    tte = "AVAL",
    is_event = "EVNT",
    covariates = c("A", "B"),
    strata = "D"
  ),
  biomarker = "AGE"
)
#> $time
#> [1] "AVAL"
#> 
#> $event
#> [1] "EVNT"
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
df <- h_coxreg_mult_cont_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX",
    strata = c("STRATA1", "STRATA2")
  ),
  data = adtte_f
)
df
#>   biomarker              biomarker_label n_tot n_tot_events   median       hr
#> 1    BMRKR1 Continuous Level Biomarker 1   200          141 753.5176 1.000189
#> 2       AGE                          Age   200          141 753.5176 1.008267
#>         lcl      ucl conf_level      pval     pval_label
#> 1 0.9511092 1.051802       0.95 0.9941244 p-value (Wald)
#> 2 0.9845155 1.032591       0.95 0.4984743 p-value (Wald)

# If the data set is empty, still the corresponding rows with missings are returned.
h_coxreg_mult_cont_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "REGION1",
    strata = c("STRATA1", "STRATA2")
  ),
  data = adtte_f[NULL, ]
)
#>   biomarker              biomarker_label n_tot n_tot_events median hr lcl ucl
#> 1    BMRKR1 Continuous Level Biomarker 1     0            0     NA NA  NA  NA
#> 2       AGE                          Age     0            0     NA NA  NA  NA
#>   conf_level pval     pval_label
#> 1       0.95   NA p-value (Wald)
#> 2       0.95   NA p-value (Wald)
```
