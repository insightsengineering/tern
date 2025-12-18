# Count number of patients with missed doses by thresholds

**\[stable\]**

The analyze function creates a layout element to calculate cumulative
counts of patients with number of missed doses at least equal to
user-specified threshold values.

This function analyzes numeric variable `vars`, a variable with numbers
of missed doses, against the threshold values supplied to the
`thresholds` argument as a numeric vector. This function assumes that
every row of the given data frame corresponds to a unique patient.

## Usage

``` r
count_missed_doses(
  lyt,
  vars,
  thresholds,
  var_labels = vars,
  show_labels = "visible",
  na_str = default_na_str(),
  nested = TRUE,
  table_names = vars,
  ...,
  na_rm = TRUE,
  .stats = c("n", "count_fraction"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_count_missed_doses(
  x,
  thresholds,
  .N_col,
  .N_row,
  denom = c("N_col", "n", "N_row"),
  ...
)

a_count_missed_doses(
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
  minimum number of missed doses the patients had.

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

  Options are: `'n', 'count_fraction'`

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

## Value

- `count_missed_doses()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_missed_doses()` to the table
  layout.

&nbsp;

- `s_count_missed_doses()` returns the statistics `n` and
  `count_fraction` with one element for each threshold.

&nbsp;

- `a_count_missed_doses()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_missed_doses()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_missed_doses()`: Statistics function to count patients with
  missed doses.

- `a_count_missed_doses()`: Formatted analysis function which is used as
  `afun` in `count_missed_doses()`.

## See also

- Relevant description function
  [`d_count_missed_doses()`](https://insightsengineering.github.io/tern/reference/d_count_missed_doses.md)
  which generates labels for `count_missed_doses()`.

- Similar analyze function
  [`count_cumulative()`](https://insightsengineering.github.io/tern/reference/count_cumulative.md)
  which more generally counts cumulative values and has more options for
  threshold handling, but uses different labels.

## Examples

``` r
library(dplyr)

anl <- tern_ex_adsl %>%
  distinct(STUDYID, USUBJID, ARM) %>%
  mutate(
    PARAMCD = "TNDOSMIS",
    PARAM = "Total number of missed doses during study",
    AVAL = sample(0:20, size = nrow(tern_ex_adsl), replace = TRUE),
    AVALC = ""
  )

basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  count_missed_doses("AVAL", thresholds = c(1, 5, 10, 15), var_labels = "Missed Doses") %>%
  build_table(anl, alt_counts_df = tern_ex_adsl)
#>                              A: Drug X    B: Placebo   C: Combination
#>                                (N=69)       (N=73)         (N=58)    
#> —————————————————————————————————————————————————————————————————————
#> Missed Doses                                                         
#>   n                              69           73             58      
#>   At least 1 missed dose     66 (95.7%)   67 (91.8%)     53 (91.4%)  
#>   At least 5 missed doses    54 (78.3%)   58 (79.5%)      40 (69%)   
#>   At least 10 missed doses    40 (58%)    37 (50.7%)     28 (48.3%)  
#>   At least 15 missed doses   18 (26.1%)   21 (28.8%)     15 (25.9%)  
```
