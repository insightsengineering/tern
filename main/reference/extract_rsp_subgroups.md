# Prepare response data for population subgroups in data frames

**\[stable\]**

Prepares response rates and odds ratios for population subgroups in data
frames. Simple wrapper for
[`h_odds_ratio_subgroups_df()`](https://insightsengineering.github.io/tern/reference/h_response_subgroups.md)
and
[`h_proportion_subgroups_df()`](https://insightsengineering.github.io/tern/reference/h_response_subgroups.md).
Result is a list of two `data.frames`: `prop` and `or`. `variables`
corresponds to the names of variables found in `data`, passed as a named
`list` and requires elements `rsp`, `arm` and optionally `subgroups` and
`strata`. `groups_lists` optionally specifies groupings for `subgroups`
variables.

## Usage

``` r
extract_rsp_subgroups(
  variables,
  data,
  groups_lists = list(),
  conf_level = 0.95,
  method = NULL,
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

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string` or `NULL`)  
  specifies the test used to calculate the p-value for the difference
  between two proportions. For options, see
  [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md).
  Default is `NULL` so no test is performed.

- label_all:

  (`string`)  
  label for the total population analysis.

## Value

A named list of two elements:

- `prop`: A `data.frame` containing columns `arm`, `n`, `n_rsp`, `prop`,
  `subgroup`, `var`, `var_label`, and `row_type`.

- `or`: A `data.frame` containing columns `arm`, `n_tot`, `or`, `lcl`,
  `ucl`, `conf_level`, `subgroup`, `var`, `var_label`, and `row_type`.

## See also

[response_subgroups](https://insightsengineering.github.io/tern/reference/response_subgroups.md)
