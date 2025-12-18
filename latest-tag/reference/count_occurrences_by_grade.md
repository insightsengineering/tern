# Count occurrences by grade

**\[stable\]**

The analyze function `count_occurrences_by_grade()` creates a layout
element to calculate occurrence counts by grade.

This function analyzes primary analysis variable `var` which indicates
toxicity grades. The `id` variable is used to indicate unique subject
identifiers (defaults to `USUBJID`). The user can also supply a list of
custom groups of grades to analyze via the `grade_groups` parameter. The
`remove_single` argument will remove single grades from the analysis so
that *only* grade groups are analyzed.

If there are multiple grades recorded for one patient only the highest
grade level is counted.

The summarize function `summarize_occurrences_by_grade()` performs the
same function as `count_occurrences_by_grade()` except it creates
content rows, not data rows, to summarize the current table row/column
context and operates on the level of the latest row split or the root of
the table if no row splits have occurred.

## Usage

``` r
count_occurrences_by_grade(
  lyt,
  var,
  id = "USUBJID",
  grade_groups = list(),
  remove_single = TRUE,
  only_grade_groups = FALSE,
  var_labels = var,
  show_labels = "default",
  riskdiff = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = var,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction_fixed_dp),
  .labels = NULL,
  .indent_mods = NULL
)

summarize_occurrences_by_grade(
  lyt,
  var,
  id = "USUBJID",
  grade_groups = list(),
  remove_single = TRUE,
  only_grade_groups = FALSE,
  riskdiff = FALSE,
  na_str = default_na_str(),
  ...,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction_fixed_dp),
  .labels = NULL,
  .indent_mods = 0L
)

s_count_occurrences_by_grade(
  df,
  labelstr = "",
  .var,
  .N_row,
  .N_col,
  ...,
  id = "USUBJID",
  grade_groups = list(),
  remove_single = TRUE,
  only_grade_groups = FALSE,
  denom = c("N_col", "n", "N_row")
)

a_count_occurrences_by_grade(
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

- id:

  (`string`)  
  subject variable name.

- grade_groups:

  (named `list` of `character`)  
  list containing groupings of grades.

- remove_single:

  (`flag`)  
  `TRUE` to not include the elements of one-element grade groups in the
  the output list; in this case only the grade groups names will be
  included in the output. If `only_grade_groups` is set to `TRUE` this
  argument is ignored.

- only_grade_groups:

  (`flag`)  
  whether only the specified grade groups should be included, with
  individual grade rows removed (`TRUE`), or all grades and grade groups
  should be displayed (`FALSE`).

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

  Options are: `'count_fraction', 'count_fraction_fixed_dp'`

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

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `N_col`: total number of patients in this column across rows.

  - `n`: number of patients with any occurrences.

  - `N_row`: total number of patients in this row across columns.

## Value

- `count_occurrences_by_grade()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_occurrences_by_grade()` to the
  table layout.

&nbsp;

- `summarize_occurrences_by_grade()` returns a layout object suitable
  for passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted content
  rows containing the statistics from `s_count_occurrences_by_grade()`
  to the table layout.

&nbsp;

- `s_count_occurrences_by_grade()` returns a list of counts and
  fractions with one element per grade level or grade level grouping.

&nbsp;

- `a_count_occurrences_by_grade()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_occurrences_by_grade()`: Layout-creating function which can
  take statistics function arguments and additional format arguments.
  This function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `summarize_occurrences_by_grade()`: Layout-creating function which can
  take content function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_count_occurrences_by_grade()`: Statistics function which counts the
  number of patients by highest grade.

- `a_count_occurrences_by_grade()`: Formatted analysis function which is
  used as `afun` in `count_occurrences_by_grade()`.

## See also

Relevant helper function
[`h_append_grade_groups()`](https://insightsengineering.github.io/tern/reference/h_append_grade_groups.md).

## Examples

``` r
library(dplyr)

df <- data.frame(
  USUBJID = as.character(c(1:6, 1)),
  ARM = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B")),
  AETOXGR = factor(c(1, 2, 3, 4, 1, 2, 3), levels = c(1:5)),
  AESEV = factor(
    x = c("MILD", "MODERATE", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE"),
    levels = c("MILD", "MODERATE", "SEVERE")
  ),
  stringsAsFactors = FALSE
)

df_adsl <- df %>%
  select(USUBJID, ARM) %>%
  unique()

# Layout creating function with custom format.
basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_occurrences_by_grade(
    var = "AESEV",
    .formats = c("count_fraction" = "xx.xx (xx.xx%)")
  ) %>%
  build_table(df, alt_counts_df = df_adsl)
#>                  A               B      
#>                (N=3)           (N=3)    
#> ————————————————————————————————————————
#> MILD       0.00 (0.00%)    2.00 (66.67%)
#> MODERATE   1.00 (33.33%)   1.00 (33.33%)
#> SEVERE     2.00 (66.67%)   0.00 (0.00%) 

# Define additional grade groupings.
grade_groups <- list(
  "-Any-" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-5" = c("3", "4", "5")
)

basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = grade_groups,
    only_grade_groups = TRUE
  ) %>%
  build_table(df, alt_counts_df = df_adsl)
#>                 A           B    
#>               (N=3)       (N=3)  
#> —————————————————————————————————
#> -Any-       3 (100%)    3 (100%) 
#> Grade 1-2   1 (33.3%)   2 (66.7%)
#> Grade 3-5   2 (66.7%)   1 (33.3%)

# Layout creating function with custom format.
basic_table() %>%
  add_colcounts() %>%
  split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
  summarize_occurrences_by_grade(
    var = "AESEV",
    .formats = c("count_fraction" = "xx.xx (xx.xx%)")
  ) %>%
  build_table(df, alt_counts_df = df_adsl)
#>                 all obs   
#>                  (N=6)    
#> ——————————————————————————
#> A                         
#>   MILD       0.00 (0.00%) 
#>   MODERATE   1.00 (16.67%)
#>   SEVERE     2.00 (33.33%)
#> B                         
#>   MILD       2.00 (33.33%)
#>   MODERATE   1.00 (16.67%)
#>   SEVERE     0.00 (0.00%) 

basic_table() %>%
  add_colcounts() %>%
  split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
  summarize_occurrences_by_grade(
    var = "AETOXGR",
    grade_groups = grade_groups
  ) %>%
  build_table(df, alt_counts_df = df_adsl)
#>                all obs 
#>                 (N=6)  
#> ———————————————————————
#> A                      
#>   -Any-       3 (50.0%)
#>   Grade 1-2   1 (16.7%)
#>   1               0    
#>   2           1 (16.7%)
#>   Grade 3-5   2 (33.3%)
#>   3           2 (33.3%)
#>   4               0    
#>   5               0    
#> B                      
#>   -Any-       3 (50.0%)
#>   Grade 1-2   2 (33.3%)
#>   1           1 (16.7%)
#>   2           1 (16.7%)
#>   Grade 3-5   1 (16.7%)
#>   3               0    
#>   4           1 (16.7%)
#>   5               0    

s_count_occurrences_by_grade(
  df,
  .N_col = 10L,
  .var = "AETOXGR",
  id = "USUBJID",
  grade_groups = list("ANY" = levels(df$AETOXGR))
)
#> $count_fraction
#> $count_fraction$ANY
#> [1] 6.0 0.6
#> 
#> $count_fraction$`1`
#> [1] 1.0 0.1
#> 
#> $count_fraction$`2`
#> [1] 2.0 0.2
#> 
#> $count_fraction$`3`
#> [1] 2.0 0.2
#> 
#> $count_fraction$`4`
#> [1] 1.0 0.1
#> 
#> $count_fraction$`5`
#> [1] 0 0
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$ANY
#> [1] 6.0 0.6
#> 
#> $count_fraction_fixed_dp$`1`
#> [1] 1.0 0.1
#> 
#> $count_fraction_fixed_dp$`2`
#> [1] 2.0 0.2
#> 
#> $count_fraction_fixed_dp$`3`
#> [1] 2.0 0.2
#> 
#> $count_fraction_fixed_dp$`4`
#> [1] 1.0 0.1
#> 
#> $count_fraction_fixed_dp$`5`
#> [1] 0 0
#> 
#> 

a_count_occurrences_by_grade(
  df,
  .N_col = 10L,
  .N_row = 10L,
  .var = "AETOXGR",
  id = "USUBJID",
  grade_groups = list("ANY" = levels(df$AETOXGR))
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>    row_name formatted_cell indent_mod row_label
#> 1       ANY        6 (60%)          0       ANY
#> 2         1        1 (10%)          0         1
#> 3         2        2 (20%)          0         2
#> 4         3        2 (20%)          0         3
#> 5         4        1 (10%)          0         4
#> 6         5              0          0         5
#> 7       ANY      6 (60.0%)          0       ANY
#> 8         1      1 (10.0%)          0         1
#> 9         2      2 (20.0%)          0         2
#> 10        3      2 (20.0%)          0         3
#> 11        4      1 (10.0%)          0         4
#> 12        5              0          0         5
```
