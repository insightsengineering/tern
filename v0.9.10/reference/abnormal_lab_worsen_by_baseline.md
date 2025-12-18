# Count patients with toxicity grades that have worsened from baseline by highest grade post-baseline

**\[stable\]**

The analyze function `count_abnormal_lab_worsen_by_baseline()` creates a
layout element to count patients with analysis toxicity grades which
have worsened from baseline, categorized by highest (worst) grade
post-baseline.

This function analyzes primary analysis variable `var` which indicates
analysis toxicity grades. Additional analysis variables that can be
supplied as a list via the `variables` parameter are `id` (defaults to
`USUBJID`), a variable to indicate unique subject identifiers,
`baseline_var` (defaults to `BTOXGR`), a variable to indicate baseline
toxicity grades, and `direction_var` (defaults to `GRADDIR`), a variable
to indicate toxicity grade directions of interest to include (e.g. `"H"`
(high), `"L"` (low), or `"B"` (both)).

For the direction(s) specified in `direction_var`, patient counts by
worst grade for patients who have worsened from baseline are calculated
as follows:

- `1` to `4`: The number of patients who have worsened from their
  baseline grades with worst grades 1-4, respectively.

- `Any`: The total number of patients who have worsened from their
  baseline grades.

Fractions are calculated by dividing the above counts by the number of
patients who's analysis toxicity grades have worsened from baseline
toxicity grades during treatment.

Prior to using this function in your table layout you must use
[`rtables::split_rows_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
to create a row split on variable `direction_var`.

## Usage

``` r
count_abnormal_lab_worsen_by_baseline(
  lyt,
  var,
  variables = list(id = "USUBJID", baseline_var = "BTOXGR", direction_var = "GRADDR"),
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = lifecycle::deprecated(),
  .stats = "fraction",
  .stat_names = NULL,
  .formats = list(fraction = format_fraction),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_abnormal_lab_worsen_by_baseline(
  df,
  .var = "ATOXGR",
  variables = list(id = "USUBJID", baseline_var = "BTOXGR", direction_var = "GRADDR"),
  ...
)

a_count_abnormal_lab_worsen_by_baseline(
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
  list of additional analysis variables including:

  - `id` (`string`)  
    subject variable name.

  - `baseline_var` (`string`)  
    name of the data column containing baseline toxicity variable.

  - `direction_var` (`string`)  
    see `direction_var` for more details.

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

  **\[deprecated\]** this parameter has no effect.

  Options are: `'fraction'`

- .stats:

  (`character`)  
  statistics to select for the table.

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

## Value

- `count_abnormal_lab_worsen_by_baseline()` returns a layout object
  suitable for passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from
  `s_count_abnormal_lab_worsen_by_baseline()` to the table layout.

&nbsp;

- `s_count_abnormal_lab_worsen_by_baseline()` returns the counts and
  fraction of patients whose worst post-baseline lab grades are worse
  than their baseline grades, for post-baseline worst grades "1", "2",
  "3", "4" and "Any".

&nbsp;

- `a_count_abnormal_lab_worsen_by_baseline()` returns the corresponding
  list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_abnormal_lab_worsen_by_baseline()`: Layout-creating function
  which can take statistics function arguments and additional format
  arguments. This function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_abnormal_lab_worsen_by_baseline()`: Statistics function for
  patients whose worst post-baseline lab grades are worse than their
  baseline grades.

- `a_count_abnormal_lab_worsen_by_baseline()`: Formatted analysis
  function which is used as `afun` in
  `count_abnormal_lab_worsen_by_baseline()`.

## See also

Relevant helper functions
[`h_adlb_worsen()`](https://insightsengineering.github.io/tern/reference/h_adlb_worsen.md)
and
[`h_worsen_counter()`](https://insightsengineering.github.io/tern/reference/h_worsen_counter.md)
which are used within `s_count_abnormal_lab_worsen_by_baseline()` to
process input data.

## Examples

``` r
library(dplyr)

# The direction variable, GRADDR, is based on metadata
adlb <- tern_ex_adlb %>%
  mutate(
    GRADDR = case_when(
      PARAMCD == "ALT" ~ "B",
      PARAMCD == "CRP" ~ "L",
      PARAMCD == "IGA" ~ "H"
    )
  ) %>%
  filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

df <- h_adlb_worsen(
  adlb,
  worst_flag_low = c("WGRLOFL" = "Y"),
  worst_flag_high = c("WGRHIFL" = "Y"),
  direction_var = "GRADDR"
)

basic_table() %>%
  split_cols_by("ARMCD") %>%
  add_colcounts() %>%
  split_rows_by("PARAMCD") %>%
  split_rows_by("GRADDR") %>%
  count_abnormal_lab_worsen_by_baseline(
    var = "ATOXGR",
    variables = list(
      id = "USUBJID",
      baseline_var = "BTOXGR",
      direction_var = "GRADDR"
    )
  ) %>%
  append_topleft("Direction of Abnormality") %>%
  build_table(df = df, alt_counts_df = tern_ex_adsl)
#>                                ARM A           ARM B           ARM C    
#> Direction of Abnormality      (N=69)          (N=73)          (N=58)    
#> ————————————————————————————————————————————————————————————————————————
#> IGA                                                                     
#>   High                                                                  
#>     1                       6/63 (9.5%)     6/64 (9.4%)      4/50 (8%)  
#>     2                      8/64 (12.5%)     5/67 (7.5%)    8/53 (15.1%) 
#>     3                      7/66 (10.6%)     5/68 (7.4%)    9/57 (15.8%) 
#>     4                       6/68 (8.8%)     2/72 (2.8%)     3/58 (5.2%) 
#>     Any                    27/68 (39.7%)    18/72 (25%)    24/58 (41.4%)
#> ALT                                                                     
#>   High                                                                  
#>     1                      7/63 (11.1%)     6/62 (9.7%)     2/48 (4.2%) 
#>     2                       12/63 (19%)      4/67 (6%)      11/50 (22%) 
#>     3                       4/65 (6.2%)    11/71 (15.5%)   7/56 (12.5%) 
#>     4                       1/67 (1.5%)    8/71 (11.3%)      4/57 (7%)  
#>     Any                    24/67 (35.8%)   29/71 (40.8%)   24/57 (42.1%)
#>   Low                                                                   
#>     1                      12/67 (17.9%)    4/66 (6.1%)    7/52 (13.5%) 
#>     2                      9/68 (13.2%)    12/69 (17.4%)   6/55 (10.9%) 
#>     3                       6/69 (8.7%)     4/71 (5.6%)     5/56 (8.9%) 
#>     4                      7/69 (10.1%)     7/73 (9.6%)    6/58 (10.3%) 
#>     Any                    34/69 (49.3%)    27/73 (37%)    24/58 (41.4%)
#> CRP                                                                     
#>   Low                                                                   
#>     1                      11/66 (16.7%)   10/67 (14.9%)    4/47 (8.5%) 
#>     2                      8/66 (12.1%)     1/70 (1.4%)     6/50 (12%)  
#>     3                       4/68 (5.9%)    9/70 (12.9%)     5/53 (9.4%) 
#>     4                      7/69 (10.1%)     6/72 (8.3%)     4/55 (7.3%) 
#>     Any                    30/69 (43.5%)   26/72 (36.1%)   19/55 (34.5%)
```
