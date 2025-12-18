# Count specific values

**\[stable\]**

The analyze function `count_values()` creates a layout element to
calculate counts of specific values within a variable of interest.

This function analyzes one or more variables of interest supplied as a
vector to `vars`. Values to count for variable(s) in `vars` can be given
as a vector via the `values` argument. One row of counts will be
generated for each variable.

## Usage

``` r
count_values(
  lyt,
  vars,
  values,
  na_str = default_na_str(),
  na_rm = TRUE,
  nested = TRUE,
  ...,
  table_names = vars,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = c(count_fraction = "xx (xx.xx%)", count = "xx"),
  .labels = c(count_fraction = paste(values, collapse = ", ")),
  .indent_mods = NULL
)

s_count_values(x, values, na.rm = TRUE, denom = c("n", "N_col", "N_row"), ...)

# S3 method for class 'character'
s_count_values(x, values = "Y", na.rm = TRUE, ...)

# S3 method for class 'factor'
s_count_values(x, values = "Y", ...)

# S3 method for class 'logical'
s_count_values(x, values = TRUE, ...)

a_count_values(
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

- values:

  (`character`)  
  specific values that should be counted.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- ...:

  additional arguments for the lower level functions.

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are:
  `'n', 'count', 'count_fraction', 'count_fraction_fixed_dp', 'n_blq'`

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

- na.rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

## Value

- `count_values()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_values()` to the table layout.

&nbsp;

- `s_count_values()` returns output of
  [`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
  for specified values of a non-numeric variable.

&nbsp;

- `a_count_values()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_values()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_values()`: S3 generic function to count values.

- `s_count_values(character)`: Method for `character` class.

- `s_count_values(factor)`: Method for `factor` class. This makes an
  automatic conversion to `character` and then forwards to the method
  for characters.

- `s_count_values(logical)`: Method for `logical` class.

- `a_count_values()`: Formatted analysis function which is used as
  `afun` in `count_values()`.

## Note

- For `factor` variables, `s_count_values` checks whether `values` are
  all included in the levels of `x` and fails otherwise.

- For `count_values()`, variable labels are shown when there is more
  than one element in `vars`, otherwise they are hidden.

## Examples

``` r
# `count_values`
basic_table() %>%
  count_values("Species", values = "setosa") %>%
  build_table(iris)
#>            all obs  
#> ————————————————————
#> setosa   50 (33.33%)

# `s_count_values.character`
s_count_values(x = c("a", "b", "a"), values = "a")
#> $n
#> n 
#> 3 
#> 
#> $count
#> count 
#>     2 
#> 
#> $count_fraction
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $count_fraction_fixed_dp
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $fraction
#>   num denom 
#>     2     3 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 
s_count_values(x = c("a", "b", "a", NA, NA), values = "b", na.rm = FALSE)
#> $n
#> n 
#> 5 
#> 
#> $count
#> count 
#>     1 
#> 
#> $count_fraction
#>    count fraction 
#>      1.0      0.2 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>      1.0      0.2 
#> 
#> $fraction
#>   num denom 
#>     1     5 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

# `s_count_values.factor`
s_count_values(x = factor(c("a", "b", "a")), values = "a")
#> $n
#> n 
#> 3 
#> 
#> $count
#> count 
#>     2 
#> 
#> $count_fraction
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $count_fraction_fixed_dp
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $fraction
#>   num denom 
#>     2     3 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

# `s_count_values.logical`
s_count_values(x = c(TRUE, FALSE, TRUE))
#> $n
#> n 
#> 3 
#> 
#> $count
#> count 
#>     2 
#> 
#> $count_fraction
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $count_fraction_fixed_dp
#>     count  fraction 
#> 2.0000000 0.6666667 
#> 
#> $fraction
#>   num denom 
#>     2     3 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

# `a_count_values`
a_count_values(x = factor(c("a", "b", "a")), values = "a", .N_col = 10, .N_row = 10)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                  row_name formatted_cell indent_mod               row_label
#> 1                       n              3          0                       n
#> 2                   count              2          0                   count
#> 3          count_fraction      2 (66.7%)          0          count_fraction
#> 4 count_fraction_fixed_dp      2 (66.7%)          0 count_fraction_fixed_dp
#> 5                fraction    2/3 (66.7%)          0                fraction
#> 6                   n_blq              0          0                   n_blq
```
