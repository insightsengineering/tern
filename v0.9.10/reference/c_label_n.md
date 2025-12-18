# Content row function to add row total to labels

This takes the label of the latest row split level and adds the row
total from `df` in parentheses. This function differs from
[`c_label_n_alt()`](https://insightsengineering.github.io/tern/reference/c_label_n_alt.md)
by taking row counts from `df` rather than `alt_counts_df`, and is used
by
[`add_rowcounts()`](https://insightsengineering.github.io/tern/reference/add_rowcounts.md)
when `alt_counts` is set to `FALSE`.

## Usage

``` r
c_label_n(df, labelstr, .N_row)
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

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

## Value

A list with formatted
[`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html)
with the row count value and the correct label.

## Note

It is important here to not use `df` but rather `.N_row` in the
implementation, because the former is already split by columns and will
refer to the first column of the data only.

## See also

[`c_label_n_alt()`](https://insightsengineering.github.io/tern/reference/c_label_n_alt.md)
which performs the same function but retrieves row counts from
`alt_counts_df` instead of `df`.
