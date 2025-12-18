# Content row function to add `alt_counts_df` row total to labels

This takes the label of the latest row split level and adds the row
total from `alt_counts_df` in parentheses. This function differs from
[`c_label_n()`](https://insightsengineering.github.io/tern/reference/c_label_n.md)
by taking row counts from `alt_counts_df` rather than `df`, and is used
by
[`add_rowcounts()`](https://insightsengineering.github.io/tern/reference/add_rowcounts.md)
when `alt_counts` is set to `TRUE`.

## Usage

``` r
c_label_n_alt(df, labelstr, .alt_df_row)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

## Value

A list with formatted
[`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html)
with the row count value and the correct label.

## See also

[`c_label_n()`](https://insightsengineering.github.io/tern/reference/c_label_n.md)
which performs the same function but retrieves row counts from `df`
instead of `alt_counts_df`.
