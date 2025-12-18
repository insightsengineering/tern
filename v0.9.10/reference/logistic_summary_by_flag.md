# Logistic regression summary table

**\[stable\]**

Constructor for content functions to be used in
[`summarize_logistic()`](https://insightsengineering.github.io/tern/reference/summarize_logistic.md)
to summarize logistic regression results. This function is a wrapper for
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

## Usage

``` r
logistic_summary_by_flag(
  flag_var,
  na_str = default_na_str(),
  .indent_mods = NULL
)
```

## Arguments

- flag_var:

  (`string`)  
  variable name identifying which row should be used in this content
  function.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

## Value

A content function.
