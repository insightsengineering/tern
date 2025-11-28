# Count the number of patients with a particular event

**\[stable\]**

The analyze function `count_patients_with_event()` creates a layout
element to calculate patient counts for a user-specified set of events.

This function analyzes primary analysis variable `vars` which indicates
unique subject identifiers. Events are defined by the user as a named
vector via the `filters` argument, where each name corresponds to a
variable and each value is the value(s) that that variable takes for the
event.

If there are multiple records with the same event recorded for a
patient, only one occurrence is counted.

## Usage

``` r
count_patients_with_event(
  lyt,
  vars,
  filters,
  riskdiff = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
  ...,
  table_names = vars,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction_fixed_dp),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_patients_with_event(
  df,
  .var,
  .N_col = ncol(df),
  .N_row = nrow(df),
  ...,
  filters,
  denom = c("n", "N_col", "N_row")
)

a_count_patients_with_event(
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

- filters:

  (`character`)  
  a character vector specifying the column names and flag variables to
  be used for counting the number of unique identifiers satisfying such
  conditions. Multiple column names and flags are accepted in this
  format `c("column_name1" = "flag1", "column_name2" = "flag2")`. Note
  that only equality is being accepted as condition.

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

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

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

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  name of the column that contains the unique identifier.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

## Value

- `count_patients_with_event()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_patients_with_event()` to the
  table layout.

&nbsp;

- `s_count_patients_with_event()` returns the count and fraction of
  unique identifiers with the defined event.

&nbsp;

- `a_count_patients_with_event()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_patients_with_event()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_patients_with_event()`: Statistics function which counts the
  number of patients for which the defined event has occurred.

- `a_count_patients_with_event()`: Formatted analysis function which is
  used as `afun` in `count_patients_with_event()`.

## See also

[`count_patients_with_flags()`](https://insightsengineering.github.io/tern/reference/count_patients_with_flags.md)

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_values(
    "STUDYID",
    values = "AB12345",
    .stats = "count",
    .labels = c(count = "Total AEs")
  ) %>%
  count_patients_with_event(
    "SUBJID",
    filters = c("TRTEMFL" = "Y"),
    .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
    table_names = "tbl_all"
  ) %>%
  count_patients_with_event(
    "SUBJID",
    filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
    .labels = c(count_fraction = "Total number of patients with fatal AEs"),
    table_names = "tbl_fatal"
  ) %>%
  count_patients_with_event(
    "SUBJID",
    filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL", "AEREL" = "Y"),
    .labels = c(count_fraction = "Total number of patients with related fatal AEs"),
    .indent_mods = c(count_fraction = 2L),
    table_names = "tbl_rel_fatal"
  )

build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl)
#>                                                            A: Drug X    B: Placebo   C: Combination
#>                                                              (N=69)       (N=73)         (N=58)    
#> ———————————————————————————————————————————————————————————————————————————————————————————————————
#> Total AEs                                                     202          177            162      
#> Total number of patients with at least one adverse event   59 (100%)    57 (100%)      48 (100%)   
#> Total number of patients with fatal AEs                    28 (47.5%)   31 (54.4%)     20 (41.7%)  
#>     Total number of patients with related fatal AEs        28 (47.5%)   31 (54.4%)     20 (41.7%)  

s_count_patients_with_event(
  tern_ex_adae,
  .var = "SUBJID",
  filters = c("TRTEMFL" = "Y"),
)
#> $n
#>   n 
#> 164 
#> 
#> $count
#> count 
#>   164 
#> 
#> $count_fraction
#>    count fraction 
#>      164        1 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>      164        1 
#> 
#> $fraction
#>   num denom 
#>   164   164 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

s_count_patients_with_event(
  tern_ex_adae,
  .var = "SUBJID",
  filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
)
#> $n
#>   n 
#> 164 
#> 
#> $count
#> count 
#>    79 
#> 
#> $count_fraction
#>      count   fraction 
#> 79.0000000  0.4817073 
#> 
#> $count_fraction_fixed_dp
#>      count   fraction 
#> 79.0000000  0.4817073 
#> 
#> $fraction
#>   num denom 
#>    79   164 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

s_count_patients_with_event(
  tern_ex_adae,
  .var = "SUBJID",
  filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
  denom = "N_col",
  .N_col = 456
)
#> $n
#>   n 
#> 164 
#> 
#> $count
#> count 
#>    79 
#> 
#> $count_fraction
#>      count   fraction 
#> 79.0000000  0.1732456 
#> 
#> $count_fraction_fixed_dp
#>      count   fraction 
#> 79.0000000  0.1732456 
#> 
#> $fraction
#>   num denom 
#>    79   456 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

a_count_patients_with_event(
  tern_ex_adae,
  .var = "SUBJID",
  filters = c("TRTEMFL" = "Y"),
  .N_col = 100,
  .N_row = 100
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                  row_name formatted_cell indent_mod               row_label
#> 1                       n            164          0                       n
#> 2                   count            164          0                   count
#> 3          count_fraction     164 (100%)          0          count_fraction
#> 4 count_fraction_fixed_dp     164 (100%)          0 count_fraction_fixed_dp
#> 5                   n_blq              0          0                   n_blq
```
