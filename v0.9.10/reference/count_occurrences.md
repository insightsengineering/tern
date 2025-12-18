# Count occurrences

**\[stable\]**

The analyze function `count_occurrences()` creates a layout element to
calculate occurrence counts for patients.

This function analyzes the variable(s) supplied to `vars` and returns a
table of occurrence counts for each unique value (or level) of the
variable(s). This variable (or variables) must be non-numeric. The `id`
variable is used to indicate unique subject identifiers (defaults to
`USUBJID`).

If there are multiple occurrences of the same value recorded for a
patient, the value is only counted once.

The summarize function `summarize_occurrences()` performs the same
function as `count_occurrences()` except it creates content rows, not
data rows, to summarize the current table row/column context and
operates on the level of the latest row split or the root of the table
if no row splits have occurred.

## Usage

``` r
count_occurrences(
  lyt,
  vars,
  id = "USUBJID",
  drop = TRUE,
  var_labels = vars,
  show_labels = "hidden",
  riskdiff = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = vars,
  .stats = "count_fraction_fixed_dp",
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

summarize_occurrences(
  lyt,
  var,
  id = "USUBJID",
  drop = TRUE,
  riskdiff = FALSE,
  na_str = default_na_str(),
  ...,
  .stats = "count_fraction_fixed_dp",
  .stat_names = NULL,
  .formats = NULL,
  .indent_mods = 0L,
  .labels = NULL
)

s_count_occurrences(
  df,
  .var = "MHDECOD",
  .N_col,
  .N_row,
  .df_row,
  ...,
  drop = TRUE,
  id = "USUBJID",
  denom = c("N_col", "n", "N_row")
)

a_count_occurrences(
  df,
  labelstr = "",
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

- id:

  (`string`)  
  subject variable name.

- drop:

  (`flag`)  
  whether non-appearing occurrence levels should be dropped from the
  resulting table. Note that in that case the remaining occurrence
  levels in the table are sorted alphabetically.

- var_labels:

  (`character`)  
  variable labels.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- riskdiff:

  (`flag`)  
  whether a risk difference column is present. When set to `TRUE`,
  [`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
  must be used as `split_fun` in the prior column split of the table
  layout, specifying which columns should be compared. See
  [`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
  for details on risk difference calculation.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

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
  `'count', 'count_fraction', 'count_fraction_fixed_dp', 'fraction'`

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

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `N_col`: total number of patients in this column across rows.

  - `n`: number of patients with any occurrences.

  - `N_row`: total number of patients in this row across columns.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

## Value

- `count_occurrences()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_occurrences()` to the table
  layout.

&nbsp;

- `summarize_occurrences()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted content
  rows containing the statistics from `s_count_occurrences()` to the
  table layout.

&nbsp;

- `s_count_occurrences()` returns a list with:

  - `count`: list of counts with one element per occurrence.

  - `count_fraction`: list of counts and fractions with one element per
    occurrence.

  - `fraction`: list of numerators and denominators with one element per
    occurrence.

&nbsp;

- `a_count_occurrences()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_occurrences()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `summarize_occurrences()`: Layout-creating function which can take
  content function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_count_occurrences()`: Statistics function which counts number of
  patients that report an occurrence.

- `a_count_occurrences()`: Formatted analysis function which is used as
  `afun` in `count_occurrences()`.

## Note

By default, occurrences which don't appear in a given row split are
dropped from the table and the occurrences in the table are sorted
alphabetically per row split. Therefore, the corresponding layout needs
to use `split_fun = drop_split_levels` in the `split_rows_by` calls. Use
`drop = FALSE` if you would like to show all occurrences.

## Examples

``` r
library(dplyr)
df <- data.frame(
  USUBJID = as.character(c(
    1, 1, 2, 4, 4, 4,
    6, 6, 6, 7, 7, 8
  )),
  MHDECOD = c(
    "MH1", "MH2", "MH1", "MH1", "MH1", "MH3",
    "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"
  ),
  ARM = rep(c("A", "B"), each = 6),
  SEX = c("F", "F", "M", "M", "M", "M", "F", "F", "F", "M", "M", "F")
)
df_adsl <- df %>%
  select(USUBJID, ARM) %>%
  unique()

# Create table layout
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_occurrences(vars = "MHDECOD", .stats = c("count_fraction"))

# Apply table layout to data and produce `rtable` object
tbl <- lyt %>%
  build_table(df, alt_counts_df = df_adsl) %>%
  prune_table()

tbl
#>           A           B    
#>         (N=3)       (N=3)  
#> ———————————————————————————
#> MH1   3 (100%)    1 (33.3%)
#> MH2   1 (33.3%)   2 (66.7%)
#> MH3   1 (33.3%)   1 (33.3%)
#> MH4       0       1 (33.3%)

# Layout creating function with custom format.
basic_table() %>%
  add_colcounts() %>%
  split_rows_by("SEX", child_labels = "visible") %>%
  summarize_occurrences(
    var = "MHDECOD",
    .formats = c("count_fraction" = "xx.xx (xx.xx%)")
  ) %>%
  build_table(df, alt_counts_df = df_adsl)
#>          all obs 
#>           (N=6)  
#> —————————————————
#> F                
#>   MH1   1 (16.7%)
#>   MH2   2 (33.3%)
#>   MH3   1 (16.7%)
#>   MH4   1 (16.7%)
#> M                
#>   MH1   3 (50.0%)
#>   MH2   1 (16.7%)
#>   MH3   1 (16.7%)

# Count unique occurrences per subject.
s_count_occurrences(
  df,
  .N_col = 4L,
  .N_row = 4L,
  .df_row = df,
  .var = "MHDECOD",
  id = "USUBJID"
)
#> $count
#> $count$MH1
#> [1] 4
#> 
#> $count$MH2
#> [1] 3
#> 
#> $count$MH3
#> [1] 2
#> 
#> $count$MH4
#> [1] 1
#> 
#> 
#> $count_fraction
#> $count_fraction$MH1
#> [1] 4 1
#> 
#> $count_fraction$MH2
#> [1] 3.00 0.75
#> 
#> $count_fraction$MH3
#> [1] 2.0 0.5
#> 
#> $count_fraction$MH4
#> [1] 1.00 0.25
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$MH1
#> [1] 4 1
#> 
#> $count_fraction_fixed_dp$MH2
#> [1] 3.00 0.75
#> 
#> $count_fraction_fixed_dp$MH3
#> [1] 2.0 0.5
#> 
#> $count_fraction_fixed_dp$MH4
#> [1] 1.00 0.25
#> 
#> 
#> $fraction
#> $fraction$MH1
#>   num denom 
#>     4     4 
#> 
#> $fraction$MH2
#>   num denom 
#>     3     4 
#> 
#> $fraction$MH3
#>   num denom 
#>     2     4 
#> 
#> $fraction$MH4
#>   num denom 
#>     1     4 
#> 
#> 

a_count_occurrences(
  df,
  .N_col = 4L,
  .df_row = df,
  .var = "MHDECOD",
  id = "USUBJID"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>    row_name formatted_cell indent_mod row_label
#> 1       MH1              4          0       MH1
#> 2       MH2              3          0       MH2
#> 3       MH3              2          0       MH3
#> 4       MH4              1          0       MH4
#> 5       MH1       4 (100%)          0       MH1
#> 6       MH2        3 (75%)          0       MH2
#> 7       MH3        2 (50%)          0       MH3
#> 8       MH4        1 (25%)          0       MH4
#> 9       MH1       4 (100%)          0       MH1
#> 10      MH2      3 (75.0%)          0       MH2
#> 11      MH3      2 (50.0%)          0       MH3
#> 12      MH4      1 (25.0%)          0       MH4
#> 13      MH1   4/4 (100.0%)          0       MH1
#> 14      MH2    3/4 (75.0%)          0       MH2
#> 15      MH3    2/4 (50.0%)          0       MH3
#> 16      MH4    1/4 (25.0%)          0       MH4
```
