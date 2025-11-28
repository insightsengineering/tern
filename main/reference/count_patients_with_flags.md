# Count the number of patients with particular flags

**\[stable\]**

The analyze function `count_patients_with_flags()` creates a layout
element to calculate counts of patients for which user-specified flags
are present.

This function analyzes primary analysis variable `var` which indicates
unique subject identifiers. Flags variables to analyze are specified by
the user via the `flag_variables` argument, and must either take value
`TRUE` (flag present) or `FALSE` (flag absent) for each record.

If there are multiple records with the same flag present for a patient,
only one occurrence is counted.

## Usage

``` r
count_patients_with_flags(
  lyt,
  var,
  flag_variables,
  flag_labels = NULL,
  var_labels = var,
  show_labels = "hidden",
  riskdiff = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = paste0("tbl_flags_", var),
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction_fixed_dp),
  .indent_mods = NULL,
  .labels = NULL
)

s_count_patients_with_flags(
  df,
  .var,
  .N_col = ncol(df),
  .N_row = nrow(df),
  ...,
  flag_variables,
  flag_labels = NULL,
  denom = c("n", "N_col", "N_row")
)

a_count_patients_with_flags(
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

- var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- flag_variables:

  (`character`)  
  a vector specifying the names of `logical` variables from analysis
  dataset used for counting the number of unique identifiers.

- flag_labels:

  (`character`)  
  vector of labels to use for flag variables. If any labels are also
  specified via the `.labels` parameter, the `.labels` values will take
  precedence and replace these labels.

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

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

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

- `count_patients_with_flags()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_patients_with_flags()` to the
  table layout.

&nbsp;

- `s_count_patients_with_flags()` returns the count and the fraction of
  unique identifiers with each particular flag as a list of statistics
  `n`, `count`, `count_fraction`, and `n_blq`, with one element per
  flag.

&nbsp;

- `a_count_patients_with_flags()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_patients_with_flags()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_patients_with_flags()`: Statistics function which counts the
  number of patients for which a particular flag variable is `TRUE`.

- `a_count_patients_with_flags()`: Formatted analysis function which is
  used as `afun` in `count_patients_with_flags()`.

## Note

If `flag_labels` is not specified, variables labels will be extracted
from `df`. If variables are not labeled, variable names will be used
instead. Alternatively, a named `vector` can be supplied to
`flag_variables` such that within each name-value pair the name
corresponds to the variable name and the value is the label to use for
this variable.

## See also

[count_patients_with_event](https://insightsengineering.github.io/tern/reference/count_patients_with_event.md)

## Examples

``` r
# Add labelled flag variables to analysis dataset.
adae <- tern_ex_adae %>%
  dplyr::mutate(
    fl1 = TRUE %>% with_label("Total AEs"),
    fl2 = (TRTEMFL == "Y") %>%
      with_label("Total number of patients with at least one adverse event"),
    fl3 = (TRTEMFL == "Y" & AEOUT == "FATAL") %>%
      with_label("Total number of patients with fatal AEs"),
    fl4 = (TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y") %>%
      with_label("Total number of patients with related fatal AEs")
  )

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_patients_with_flags(
    "SUBJID",
    flag_variables = c("fl1", "fl2", "fl3", "fl4"),
    denom = "N_col"
  )

build_table(lyt, adae, alt_counts_df = tern_ex_adsl)
#>                                                            A: Drug X    B: Placebo   C: Combination
#>                                                              (N=69)       (N=73)         (N=58)    
#> ———————————————————————————————————————————————————————————————————————————————————————————————————
#> Total AEs                                                  59 (85.5%)   57 (78.1%)     48 (82.8%)  
#> Total number of patients with at least one adverse event   59 (85.5%)   57 (78.1%)     48 (82.8%)  
#> Total number of patients with fatal AEs                    28 (40.6%)   31 (42.5%)     20 (34.5%)  
#> Total number of patients with related fatal AEs            28 (40.6%)   31 (42.5%)     20 (34.5%)  

# `s_count_patients_with_flags()`

s_count_patients_with_flags(
  adae,
  "SUBJID",
  flag_variables = c("fl1", "fl2", "fl3", "fl4"),
  denom = "N_col",
  .N_col = 1000
)
#> $n
#> $n$`Total AEs`
#>   n 
#> 164 
#> 
#> $n$`Total number of patients with at least one adverse event`
#>   n 
#> 164 
#> 
#> $n$`Total number of patients with fatal AEs`
#>   n 
#> 164 
#> 
#> $n$`Total number of patients with related fatal AEs`
#>   n 
#> 164 
#> 
#> 
#> $count
#> $count$`Total AEs`
#> count 
#>   164 
#> 
#> $count$`Total number of patients with at least one adverse event`
#> count 
#>   164 
#> 
#> $count$`Total number of patients with fatal AEs`
#> count 
#>    79 
#> 
#> $count$`Total number of patients with related fatal AEs`
#> count 
#>    79 
#> 
#> 
#> $count_fraction
#> $count_fraction$`Total AEs`
#>    count fraction 
#>  164.000    0.164 
#> 
#> $count_fraction$`Total number of patients with at least one adverse event`
#>    count fraction 
#>  164.000    0.164 
#> 
#> $count_fraction$`Total number of patients with fatal AEs`
#>    count fraction 
#>   79.000    0.079 
#> 
#> $count_fraction$`Total number of patients with related fatal AEs`
#>    count fraction 
#>   79.000    0.079 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$`Total AEs`
#>    count fraction 
#>  164.000    0.164 
#> 
#> $count_fraction_fixed_dp$`Total number of patients with at least one adverse event`
#>    count fraction 
#>  164.000    0.164 
#> 
#> $count_fraction_fixed_dp$`Total number of patients with fatal AEs`
#>    count fraction 
#>   79.000    0.079 
#> 
#> $count_fraction_fixed_dp$`Total number of patients with related fatal AEs`
#>    count fraction 
#>   79.000    0.079 
#> 
#> 
#> $fraction
#> $fraction$`Total AEs`
#>   num denom 
#>   164  1000 
#> 
#> $fraction$`Total number of patients with at least one adverse event`
#>   num denom 
#>   164  1000 
#> 
#> $fraction$`Total number of patients with fatal AEs`
#>   num denom 
#>    79  1000 
#> 
#> $fraction$`Total number of patients with related fatal AEs`
#>   num denom 
#>    79  1000 
#> 
#> 
#> $n_blq
#> $n_blq$`Total AEs`
#> n_blq 
#>     0 
#> 
#> $n_blq$`Total number of patients with at least one adverse event`
#> n_blq 
#>     0 
#> 
#> $n_blq$`Total number of patients with fatal AEs`
#> n_blq 
#>     0 
#> 
#> $n_blq$`Total number of patients with related fatal AEs`
#> n_blq 
#>     0 
#> 
#> 

a_count_patients_with_flags(
  adae,
  .N_col = 10L,
  .N_row = 10L,
  .var = "USUBJID",
  flag_variables = c("fl1", "fl2", "fl3", "fl4")
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                       row_name formatted_cell indent_mod
#> 1                        n.fl1            164          0
#> 2                        n.fl2            164          0
#> 3                        n.fl3            164          0
#> 4                        n.fl4            164          0
#> 5                    count.fl1            164          0
#> 6                    count.fl2            164          0
#> 7                    count.fl3             79          0
#> 8                    count.fl4             79          0
#> 9           count_fraction.fl1     164 (100%)          0
#> 10          count_fraction.fl2     164 (100%)          0
#> 11          count_fraction.fl3     79 (48.2%)          0
#> 12          count_fraction.fl4     79 (48.2%)          0
#> 13 count_fraction_fixed_dp.fl1     164 (100%)          0
#> 14 count_fraction_fixed_dp.fl2     164 (100%)          0
#> 15 count_fraction_fixed_dp.fl3     79 (48.2%)          0
#> 16 count_fraction_fixed_dp.fl4     79 (48.2%)          0
#> 17                   n_blq.fl1              0          0
#> 18                   n_blq.fl2              0          0
#> 19                   n_blq.fl3              0          0
#> 20                   n_blq.fl4              0          0
#>                                                   row_label
#> 1                                                 Total AEs
#> 2  Total number of patients with at least one adverse event
#> 3                   Total number of patients with fatal AEs
#> 4           Total number of patients with related fatal AEs
#> 5                                                 Total AEs
#> 6  Total number of patients with at least one adverse event
#> 7                   Total number of patients with fatal AEs
#> 8           Total number of patients with related fatal AEs
#> 9                                                 Total AEs
#> 10 Total number of patients with at least one adverse event
#> 11                  Total number of patients with fatal AEs
#> 12          Total number of patients with related fatal AEs
#> 13                                                Total AEs
#> 14 Total number of patients with at least one adverse event
#> 15                  Total number of patients with fatal AEs
#> 16          Total number of patients with related fatal AEs
#> 17                                                Total AEs
#> 18 Total number of patients with at least one adverse event
#> 19                  Total number of patients with fatal AEs
#> 20          Total number of patients with related fatal AEs
```
