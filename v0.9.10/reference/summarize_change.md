# Summarize change from baseline values or absolute baseline values

**\[stable\]**

The analyze function `summarize_change()` creates a layout element to
summarize the change from baseline or absolute baseline values. The
primary analysis variable `vars` indicates the numerical change from
baseline results.

Required secondary analysis variables `value` and `baseline_flag` can be
supplied to the function via the `variables` argument. The `value`
element should be the name of the analysis value variable, and the
`baseline_flag` element should be the name of the flag variable that
indicates whether or not records contain baseline values. Depending on
the baseline flag given, either the absolute baseline values (at
baseline) or the change from baseline values (post-baseline) are then
summarized.

## Usage

``` r
summarize_change(
  lyt,
  vars,
  variables,
  var_labels = vars,
  na_str = default_na_str(),
  na_rm = TRUE,
  nested = TRUE,
  show_labels = "default",
  table_names = vars,
  section_div = NA_character_,
  ...,
  .stats = c("n", "mean_sd", "median", "range"),
  .stat_names = NULL,
  .formats = c(mean_sd = "xx.xx (xx.xx)", mean_se = "xx.xx (xx.xx)", median = "xx.xx",
    range = "xx.xx - xx.xx", mean_pval = "xx.xx"),
  .labels = NULL,
  .indent_mods = NULL
)

s_change_from_baseline(df, ...)

a_change_from_baseline(
  df,
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

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- var_labels:

  (`character`)  
  variable labels.

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

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- section_div:

  (`string`)  
  string which should be repeated as a section divider after each group
  defined by this split instruction, or `NA_character_` (the default)
  for no section divider.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are:
  `'n', 'sum', 'mean', 'sd', 'se', 'mean_sd', 'mean_se', 'mean_ci', 'mean_sei', 'mean_sdi', 'mean_pval', 'median', 'mad', 'median_ci', 'quantiles', 'iqr', 'range', 'min', 'max', 'median_range', 'cv', 'geom_mean', 'geom_sd', 'geom_mean_sd', 'geom_mean_ci', 'geom_cv', 'median_ci_3d', 'mean_ci_3d', 'geom_mean_ci_3d'`

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

## Value

- `summarize_change()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_change_from_baseline()` to the table
  layout.

&nbsp;

- `s_change_from_baseline()` returns the same values returned by
  [`s_summary.numeric()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).

&nbsp;

- `a_change_from_baseline()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `summarize_change()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_change_from_baseline()`: Statistics function that summarizes
  baseline or post-baseline visits.

- `a_change_from_baseline()`: Formatted analysis function which is used
  as `afun` in `summarize_change()`.

## Note

To be used after a split on visits in the layout, such that each data
subset only contains either baseline or post-baseline data.

The data in `df` must be either all be from baseline or post-baseline
visits. Otherwise an error will be thrown.

## Examples

``` r
library(dplyr)

# Fabricate dataset
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  AVISIT = rep(paste0("V", 1:3), 6),
  ARM = rep(LETTERS[1:3], rep(6, 3)),
  AVAL = c(9:1, rep(NA, 9))
) %>%
  mutate(ABLFLL = AVISIT == "V1") %>%
  group_by(USUBJID) %>%
  mutate(
    BLVAL = AVAL[ABLFLL],
    CHG = AVAL - BLVAL
  ) %>%
  ungroup()

results <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
  build_table(dta_test)

results
#>                     A                 B               C    
#> ———————————————————————————————————————————————————————————
#> V1                                                         
#>   n                 2                 1               0    
#>   Mean (SD)    7.50 (2.12)    3.00 (<Missing>)    <Missing>
#>   Median          7.50              3.00          <Missing>
#>   Min - Max    6.00 - 9.00       3.00 - 3.00      <Missing>
#> V2                                                         
#>   n                 2                 1               0    
#>   Mean (SD)   -1.00 (0.00)    -1.00 (<Missing>)   <Missing>
#>   Median          -1.00             -1.00         <Missing>
#>   Min - Max   -1.00 - -1.00     -1.00 - -1.00     <Missing>
#> V3                                                         
#>   n                 2                 1               0    
#>   Mean (SD)   -2.00 (0.00)    -2.00 (<Missing>)   <Missing>
#>   Median          -2.00             -2.00         <Missing>
#>   Min - Max   -2.00 - -2.00     -2.00 - -2.00     <Missing>
```
