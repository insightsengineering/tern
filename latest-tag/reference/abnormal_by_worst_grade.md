# Count patients by most extreme post-baseline toxicity grade per direction of abnormality

**\[stable\]**

The analyze function `count_abnormal_by_worst_grade()` creates a layout
element to count patients by highest (worst) analysis toxicity grade
post-baseline for each direction, categorized by parameter value.

This function analyzes primary analysis variable `var` which indicates
toxicity grades. Additional analysis variables that can be supplied as a
list via the `variables` parameter are `id` (defaults to `USUBJID`), a
variable to indicate unique subject identifiers, `param` (defaults to
`PARAM`), a variable to indicate parameter values, and `grade_dir`
(defaults to `GRADE_DIR`), a variable to indicate directions (e.g. High
or Low) for each toxicity grade supplied in `var`.

For each combination of `param` and `grade_dir` levels, patient counts
by worst grade are calculated as follows:

- `1` to `4`: The number of patients with worst grades 1-4,
  respectively.

- `Any`: The number of patients with at least one abnormality (i.e.
  grade is not 0).

Fractions are calculated by dividing the above counts by the number of
patients with at least one valid measurement recorded during treatment.

Pre-processing is crucial when using this function and can be done
automatically using the
[`h_adlb_abnormal_by_worst_grade()`](https://insightsengineering.github.io/tern/reference/h_adlb_abnormal_by_worst_grade.md)
helper function. See the description of this function for details on the
necessary pre-processing steps.

Prior to using this function in your table layout you must use
[`rtables::split_rows_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
to create two row splits, one on variable `param` and one on variable
`grade_dir`.

## Usage

``` r
count_abnormal_by_worst_grade(
  lyt,
  var,
  variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR"),
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_abnormal_by_worst_grade(
  df,
  .var = "GRADE_ANL",
  .spl_context,
  variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR"),
  ...
)

a_count_abnormal_by_worst_grade(
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

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

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

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

## Value

- `count_abnormal_by_worst_grade()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_abnormal_by_worst_grade()` to
  the table layout.

&nbsp;

- `s_count_abnormal_by_worst_grade()` returns the single statistic
  `count_fraction` with grades 1 to 4 and "Any" results.

&nbsp;

- `a_count_abnormal_by_worst_grade()` returns the corresponding list
  with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_abnormal_by_worst_grade()`: Layout-creating function which can
  take statistics function arguments and additional format arguments.
  This function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_abnormal_by_worst_grade()`: Statistics function which counts
  patients by worst grade.

- `a_count_abnormal_by_worst_grade()`: Formatted analysis function which
  is used as `afun` in `count_abnormal_by_worst_grade()`.

## See also

[`h_adlb_abnormal_by_worst_grade()`](https://insightsengineering.github.io/tern/reference/h_adlb_abnormal_by_worst_grade.md)
which pre-processes ADLB data frames to be used in
`count_abnormal_by_worst_grade()`.

## Examples

``` r
library(dplyr)
library(forcats)
adlb <- tern_ex_adlb

# Data is modified in order to have some parameters with grades only in one direction
# and simulate the real data.
adlb$ATOXGR[adlb$PARAMCD == "ALT" & adlb$ATOXGR %in% c("1", "2", "3", "4")] <- "-1"
adlb$ANRIND[adlb$PARAMCD == "ALT" & adlb$ANRIND == "HIGH"] <- "LOW"
adlb$WGRHIFL[adlb$PARAMCD == "ALT"] <- ""

adlb$ATOXGR[adlb$PARAMCD == "IGA" & adlb$ATOXGR %in% c("-1", "-2", "-3", "-4")] <- "1"
adlb$ANRIND[adlb$PARAMCD == "IGA" & adlb$ANRIND == "LOW"] <- "HIGH"
adlb$WGRLOFL[adlb$PARAMCD == "IGA"] <- ""

# Pre-processing
adlb_f <- adlb %>% h_adlb_abnormal_by_worst_grade()

# Map excludes records without abnormal grade since they should not be displayed
# in the table.
map <- unique(adlb_f[adlb_f$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]) %>%
  lapply(as.character) %>%
  as.data.frame() %>%
  arrange(PARAM, desc(GRADE_DIR), GRADE_ANL)

basic_table() %>%
  split_cols_by("ARMCD") %>%
  split_rows_by("PARAM") %>%
  split_rows_by("GRADE_DIR", split_fun = trim_levels_to_map(map)) %>%
  count_abnormal_by_worst_grade(
    var = "GRADE_ANL",
    variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
  ) %>%
  build_table(df = adlb_f)
#>                                          ARM A        ARM B        ARM C   
#> ———————————————————————————————————————————————————————————————————————————
#> Alanine Aminotransferase Measurement                                       
#>   LOW                                                                      
#>     1                                  12 (17.4%)    5 (6.8%)    8 (13.8%) 
#>     2                                   9 (13%)     13 (17.8%)   6 (10.3%) 
#>     3                                   6 (8.7%)     4 (5.5%)    6 (10.3%) 
#>     4                                  7 (10.1%)     7 (9.6%)    6 (10.3%) 
#>     Any                                34 (49.3%)   29 (39.7%)   26 (44.8%)
#> C-Reactive Protein Measurement                                             
#>   LOW                                                                      
#>     1                                  11 (15.9%)   12 (16.4%)   7 (12.1%) 
#>     2                                  8 (11.6%)     2 (2.7%)    6 (10.3%) 
#>     3                                   4 (5.8%)    9 (12.3%)    6 (10.3%) 
#>     4                                  7 (10.1%)     6 (8.2%)     4 (6.9%) 
#>     Any                                30 (43.5%)   29 (39.7%)   23 (39.7%)
#>   HIGH                                                                     
#>     1                                  8 (11.6%)    11 (15.1%)    2 (3.4%) 
#>     2                                   9 (13%)     11 (15.1%)    11 (19%) 
#>     3                                  14 (20.3%)   10 (13.7%)    5 (8.6%) 
#>     4                                   2 (2.9%)     4 (5.5%)    6 (10.3%) 
#>     Any                                33 (47.8%)   36 (49.3%)   24 (41.4%)
#> Immunoglobulin A Measurement                                               
#>   HIGH                                                                     
#>     1                                  7 (10.1%)     7 (9.6%)    6 (10.3%) 
#>     2                                  8 (11.6%)     6 (8.2%)    8 (13.8%) 
#>     3                                  7 (10.1%)     5 (6.8%)    9 (15.5%) 
#>     4                                   6 (8.7%)     2 (2.7%)     3 (5.2%) 
#>     Any                                28 (40.6%)   20 (27.4%)   26 (44.8%)
```
