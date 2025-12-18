# Count number of patients

**\[stable\]**

The analyze function `analyze_num_patients()` creates a layout element
to count total numbers of unique or non-unique patients. The primary
analysis variable `vars` is used to uniquely identify patients.

The `count_by` variable can be used to identify non-unique patients such
that the number of patients with a unique combination of values in
`vars` and `count_by` will be returned instead as the `nonunique`
statistic. The `required` variable can be used to specify a variable
required to be non-missing for the record to be included in the counts.

The summarize function `summarize_num_patients()` performs the same
function as `analyze_num_patients()` except it creates content rows, not
data rows, to summarize the current table row/column context and
operates on the level of the latest row split or the root of the table
if no row splits have occurred.

## Usage

``` r
analyze_num_patients(
  lyt,
  vars,
  required = NULL,
  count_by = NULL,
  unique_count_suffix = TRUE,
  na_str = default_na_str(),
  nested = TRUE,
  show_labels = c("default", "visible", "hidden"),
  riskdiff = FALSE,
  ...,
  .stats = c("unique", "nonunique", "unique_count"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = list(unique = "Number of patients with at least one event", nonunique =
    "Number of events"),
  .indent_mods = NULL
)

summarize_num_patients(
  lyt,
  var,
  required = NULL,
  count_by = NULL,
  unique_count_suffix = TRUE,
  na_str = default_na_str(),
  riskdiff = FALSE,
  ...,
  .stats = c("unique", "nonunique", "unique_count"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = list(unique = "Number of patients with at least one event", nonunique =
    "Number of events"),
  .indent_mods = 0L
)

s_num_patients(
  x,
  labelstr,
  .N_col,
  ...,
  count_by = NULL,
  unique_count_suffix = TRUE
)

s_num_patients_content(
  df,
  labelstr = "",
  .N_col,
  .var,
  ...,
  required = NULL,
  count_by = NULL,
  unique_count_suffix = TRUE
)

a_num_patients(
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

- required:

  (`character` or `NULL`)  
  name of a variable that is required to be non-missing.

- count_by:

  (`character` or `NULL`)  
  name of a variable to be combined with `vars` when counting
  `nonunique` records.

- unique_count_suffix:

  (`flag`)  
  whether the `"(n)"` suffix should be added to `unique_count` labels.
  Defaults to `TRUE`.

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

- riskdiff:

  (`flag`)  
  whether a risk difference column is present. When set to `TRUE`,
  [`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
  must be used as `split_fun` in the prior column split of the table
  layout, specifying which columns should be compared. See
  [`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
  for details on risk difference calculation.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'unique', 'nonunique', 'unique_count'`

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

  (`character` or `factor`)  
  vector of patient IDs.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

## Value

- `analyze_num_patients()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_num_patients_content()` to the table
  layout.

&nbsp;

- `summarize_num_patients()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_num_patients_content()` to the table
  layout.

&nbsp;

- `s_num_patients()` returns a named `list` of 3 statistics:

  - `unique`: Vector of counts and percentages.

  - `nonunique`: Vector of counts.

  - `unique_count`: Counts.

&nbsp;

- `s_num_patients_content()` returns the same values as
  `s_num_patients()`.

&nbsp;

- `a_num_patients()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Details

In general, functions that starts with `analyze*` are expected to work
like
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html),
while functions that starts with `summarize*` are based upon
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).
The latter provides a value for each dividing split in the row and
column space, but, being it bound to the fundamental splits, it is
repeated by design in every page when pagination is involved.

## Functions

- `analyze_num_patients()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `summarize_num_patients()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_num_patients()`: Statistics function which counts the number of
  unique patients, the corresponding percentage taken with respect to
  the total number of patients, and the number of non-unique patients.

- `s_num_patients_content()`: Statistics function which counts the
  number of unique patients in a column (variable), the corresponding
  percentage taken with respect to the total number of patients, and the
  number of non-unique patients in the column.

- `a_num_patients()`: Formatted analysis function which is used as
  `afun` in `analyze_num_patients()` and as `cfun` in
  `summarize_num_patients()`.

## Note

As opposed to `summarize_num_patients()`, this function does not repeat
the produced rows.

## Examples

``` r
df <- data.frame(
  USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
  ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
  AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17),
  SEX = c("M", "M", "M", "F", "F", "F", "M", "F", "M")
)

# analyze_num_patients
tbl <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze_num_patients("USUBJID", .stats = c("unique")) %>%
  build_table(df)

tbl
#>                                                  A           B    
#>                                                (N=5)       (N=4)  
#> ——————————————————————————————————————————————————————————————————
#> Number of patients with at least one event   3 (60.0%)   3 (75.0%)

# summarize_num_patients
tbl <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  summarize_num_patients("USUBJID", .stats = "unique_count") %>%
  build_table(df)

tbl
#>         A   B
#> —————————————
#> M (n)   2   2
#> F (n)   1   2

# Use the statistics function to count number of unique and nonunique patients.
s_num_patients(x = as.character(c(1, 1, 1, 2, 4, NA)), labelstr = "", .N_col = 6L)
#> $unique
#> [1] 3.0 0.5
#> attr(,"label")
#> [1] ""
#> 
#> $nonunique
#> [1] 5
#> attr(,"label")
#> [1] ""
#> 
#> $unique_count
#> [1] 3
#> attr(,"label")
#> [1] "(n)"
#> 
s_num_patients(
  x = as.character(c(1, 1, 1, 2, 4, NA)),
  labelstr = "",
  .N_col = 6L,
  count_by = c(1, 1, 2, 1, 1, 1)
)
#> $unique
#> [1] 3.0 0.5
#> attr(,"label")
#> [1] ""
#> 
#> $nonunique
#> [1] 4
#> attr(,"label")
#> [1] ""
#> 
#> $unique_count
#> [1] 3
#> attr(,"label")
#> [1] "(n)"
#> 

# Count number of unique and non-unique patients.

df <- data.frame(
  USUBJID = as.character(c(1, 2, 1, 4, NA)),
  EVENT = as.character(c(10, 15, 10, 17, 8))
)
s_num_patients_content(df, .N_col = 5, .var = "USUBJID")
#> $unique
#> [1] 3.0 0.6
#> attr(,"label")
#> [1] ""
#> 
#> $nonunique
#> [1] 4
#> attr(,"label")
#> [1] ""
#> 
#> $unique_count
#> [1] 3
#> attr(,"label")
#> [1] "(n)"
#> 

df_by_event <- data.frame(
  USUBJID = as.character(c(1, 2, 1, 4, NA)),
  EVENT = c(10, 15, 10, 17, 8)
)
s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID", count_by = "EVENT")
#> $unique
#> [1] 3.0 0.6
#> attr(,"label")
#> [1] ""
#> 
#> $nonunique
#> [1] 3
#> attr(,"label")
#> [1] ""
#> 
#> $unique_count
#> [1] 3
#> attr(,"label")
#> [1] "(n)"
#> 
```
