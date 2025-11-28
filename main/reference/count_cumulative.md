# Cumulative counts of numeric variable by thresholds

**\[stable\]**

The analyze function `count_cumulative()` creates a layout element to
calculate cumulative counts of values in a numeric variable that are
less than, less or equal to, greater than, or greater or equal to
user-specified threshold values.

This function analyzes numeric variable `vars` against the threshold
values supplied to the `thresholds` argument as a numeric vector.
Whether counts should include the threshold values, and whether to count
values lower or higher than the threshold values can be set via the
`include_eq` and `lower_tail` parameters, respectively.

## Usage

``` r
count_cumulative(
  lyt,
  vars,
  thresholds,
  lower_tail = TRUE,
  include_eq = TRUE,
  var_labels = vars,
  show_labels = "visible",
  na_str = default_na_str(),
  nested = TRUE,
  table_names = vars,
  ...,
  na_rm = TRUE,
  .stats = c("count_fraction"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_count_cumulative(
  x,
  thresholds,
  lower_tail = TRUE,
  include_eq = TRUE,
  denom = c("N_col", "n", "N_row"),
  .N_col,
  .N_row,
  na_rm = TRUE,
  ...
)

a_count_cumulative(
  x,
  ...,
  .stats = NULL,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- thresholds:

  (`numeric`)  
  vector of cutoff values for the counts.

- lower_tail:

  (`flag`)  
  whether to count lower tail, default is `TRUE`.

- include_eq:

  (`flag`)  
  whether to include value equal to the `threshold` in count, default is
  `TRUE`.

- var_labels:

  (`character`)  
  variable labels.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- ...:

  additional arguments for the lower level functions.

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'count_fraction'`

- .stat_names:

  (`character`)  
  names of the statistics that are passed directly to name single
  statistics (`.stats`). This option is visible when producing
  [`rtables::as_result_df()`](https://insightsengineering.github.io/rtables/latest-tag/reference/data.frame_export.html)
  with `make_ard = TRUE`.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

## Value

- `count_cumulative()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_cumulative()` to the table
  layout.

&nbsp;

- `s_count_cumulative()` returns a named list of `count_fraction`s: a
  list with each `thresholds` value as a component, each component
  containing a vector for the count and fraction.

&nbsp;

- `a_count_cumulative()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_cumulative()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_cumulative()`: Statistics function that produces a named list
  given a numeric vector of thresholds.

- `a_count_cumulative()`: Formatted analysis function which is used as
  `afun` in `count_cumulative()`.

## See also

Relevant helper function
[`h_count_cumulative()`](https://insightsengineering.github.io/tern/reference/h_count_cumulative.md),
and descriptive function
[`d_count_cumulative()`](https://insightsengineering.github.io/tern/reference/d_count_cumulative.md).

## Examples

``` r
basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_cumulative(
    vars = "AGE",
    thresholds = c(40, 60)
  ) %>%
  build_table(tern_ex_adsl)
#>           A: Drug X    B: Placebo   C: Combination
#>             (N=69)       (N=73)         (N=58)    
#> ——————————————————————————————————————————————————
#> AGE                                               
#>   <= 40   52 (75.4%)   58 (79.5%)     41 (70.7%)  
#>   <= 60   69 (100%)    73 (100%)      58 (100%)   
```
