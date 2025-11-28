# Prepare survival data for population subgroups in data frames

**\[stable\]**

Prepares estimates of median survival times and treatment hazard ratios
for population subgroups in data frames. Simple wrapper for
[`h_survtime_subgroups_df()`](https://insightsengineering.github.io/tern/reference/h_survival_duration_subgroups.md)
and
[`h_coxph_subgroups_df()`](https://insightsengineering.github.io/tern/reference/h_survival_duration_subgroups.md).
Result is a `list` of two `data.frame`s: `survtime` and `hr`.
`variables` corresponds to the names of variables found in `data`,
passed as a named `list` and requires elements `tte`, `is_event`, `arm`
and optionally `subgroups` and `strata`. `groups_lists` optionally
specifies groupings for `subgroups` variables.

## Usage

``` r
extract_survival_subgroups(
  variables,
  data,
  groups_lists = list(),
  control = control_coxph(),
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

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).
  Some possible parameter options are:

  - `pval_method` (`string`)  
    p-value method for testing the null hypothesis that hazard ratio
    = 1. Default method is `"log-rank"` which comes from
    [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html),
    can also be set to `"wald"` or `"likelihood"` (from
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)).

  - `ties` (`string`)  
    specifying the method for tie handling. Default is `"efron"`, can
    also be set to `"breslow"` or `"exact"`. See more in
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

  - `conf_level` (`proportion`)  
    confidence level of the interval for HR.

- label_all:

  (`string`)  
  label for the total population analysis.

## Value

A named `list` of two elements:

- `survtime`: A `data.frame` containing columns `arm`, `n`, `n_events`,
  `median`, `subgroup`, `var`, `var_label`, and `row_type`.

- `hr`: A `data.frame` containing columns `arm`, `n_tot`,
  `n_tot_events`, `hr`, `lcl`, `ucl`, `conf_level`, `pval`,
  `pval_label`, `subgroup`, `var`, `var_label`, and `row_type`.

## See also

[survival_duration_subgroups](https://insightsengineering.github.io/tern/reference/survival_duration_subgroups.md)
