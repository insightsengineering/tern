# Labels for column variables in binary response by subgroup table

**\[stable\]**

Internal function to check variables included in
[`tabulate_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/response_subgroups.md)
and create column labels.

## Usage

``` r
d_rsp_subgroups_colvars(vars, conf_level = NULL, method = NULL)
```

## Arguments

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string` or `NULL`)  
  specifies the test used to calculate the p-value for the difference
  between two proportions. For options, see
  [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md).
  Default is `NULL` so no test is performed.

## Value

A `list` of variables to tabulate and their labels.
