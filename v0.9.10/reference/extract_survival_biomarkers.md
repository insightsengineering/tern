# Prepare survival data estimates for multiple biomarkers in a single data frame

**\[stable\]**

Prepares estimates for number of events, patients and median survival
times, as well as hazard ratio estimates, confidence intervals and
p-values, for multiple biomarkers across population subgroups in a
single data frame. `variables` corresponds to the names of variables
found in `data`, passed as a named `list` and requires elements `tte`,
`is_event`, `biomarkers` (vector of continuous biomarker variables), and
optionally `subgroups` and `strata`. `groups_lists` optionally specifies
groupings for `subgroups` variables.

## Usage

``` r
extract_survival_biomarkers(
  variables,
  data,
  groups_lists = list(),
  control = control_coxreg(),
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
  a list of parameters as returned by the helper function
  [`control_coxreg()`](https://insightsengineering.github.io/tern/reference/control_coxreg.md).

- label_all:

  (`string`)  
  label for the total population analysis.

## Value

A `data.frame` with columns `biomarker`, `biomarker_label`, `n_tot`,
`n_tot_events`, `median`, `hr`, `lcl`, `ucl`, `conf_level`, `pval`,
`pval_label`, `subgroup`, `var`, `var_label`, and `row_type`.

## See also

[`h_coxreg_mult_cont_df()`](https://insightsengineering.github.io/tern/reference/h_survival_biomarkers_subgroups.md)
which is used internally,
[`tabulate_survival_biomarkers()`](https://insightsengineering.github.io/tern/reference/survival_biomarkers_subgroups.md).
