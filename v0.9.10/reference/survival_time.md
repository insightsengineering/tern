# Survival time analysis

**\[stable\]**

The analyze function `surv_time()` creates a layout element to analyze
survival time by calculating survival time median, median confidence
interval, quantiles, and range (for all, censored, or event patients).
The primary analysis variable `vars` is the time variable and the
secondary analysis variable `is_event` indicates whether or not an event
has occurred.

## Usage

``` r
surv_time(
  lyt,
  vars,
  is_event,
  control = control_surv_time(),
  ref_fn_censor = TRUE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  var_labels = "Time to Event",
  show_labels = "visible",
  table_names = vars,
  .stats = c("median", "median_ci", "quantiles", "range"),
  .stat_names = NULL,
  .formats = list(median_ci = "(xx.x, xx.x)", quantiles = "xx.x, xx.x", range =
    "xx.x to xx.x", quantiles_lower = "xx.x (xx.x - xx.x)", quantiles_upper =
    "xx.x (xx.x - xx.x)", median_ci_3d = "xx.x (xx.x - xx.x)"),
  .labels = list(median_ci = "95% CI", range = "Range"),
  .indent_mods = list(median_ci = 1L)
)

s_surv_time(df, .var, ..., is_event, control = control_surv_time())

a_surv_time(
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

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

- control:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_surv_time()`](https://insightsengineering.github.io/tern/reference/control_surv_time.md).
  Some possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level of the interval for survival time.

  - `conf_type` (`string`)  
    confidence interval type. Options are "plain" (default), "log", or
    "log-log", see more in
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
    Note option "none" is not supported.

  - `quantiles` (`numeric`)  
    vector of length two to specify the quantiles of survival time.

- ref_fn_censor:

  (`flag`)  
  whether referential footnotes indicating censored observations should
  be printed when the `range` statistic is included.

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

- var_labels:

  (`character`)  
  variable labels.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are:
  `'median', 'median_ci', 'median_ci_3d', 'quantiles', 'quantiles_lower', 'quantiles_upper', 'range_censor', 'range_event', 'range'`

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
  indent modifiers for the labels. Each element of the vector should be
  a name-value pair with name corresponding to a statistic specified in
  `.stats` and value the indentation for that statistic's row label.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

## Value

- `surv_time()` returns a layout object suitable for passing to further
  layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_surv_time()` to the table layout.

&nbsp;

- `s_surv_time()` returns the statistics:

  - `median`: Median survival time.

  - `median_ci`: Confidence interval for median time.

  - `median_ci_3d`: Median with confidence interval for median time.

  - `quantiles`: Survival time for two specified quantiles.

  - `quantiles_lower`: quantile with confidence interval for the first
    specified quantile.

  - `quantiles_upper`: quantile with confidence interval for the second
    specified quantile.

  - `range_censor`: Survival time range for censored observations.

  - `range_event`: Survival time range for observations with events.

  - `range`: Survival time range for all observations.

&nbsp;

- `a_surv_time()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `surv_time()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_surv_time()`: Statistics function which analyzes survival times.

- `a_surv_time()`: Formatted analysis function which is used as `afun`
  in `surv_time()`.

## Examples

``` r
library(dplyr)

adtte_f <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0
  )
df <- adtte_f %>% filter(ARMCD == "ARM A")

basic_table() %>%
  split_cols_by(var = "ARMCD") %>%
  add_colcounts() %>%
  surv_time(
    vars = "AVAL",
    var_labels = "Survival Time (Months)",
    is_event = "is_event",
    control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
  ) %>%
  build_table(df = adtte_f)
#>                             ARM A          ARM B          ARM C    
#>                             (N=69)         (N=73)         (N=58)   
#> ———————————————————————————————————————————————————————————————————
#> Survival Time (Months)                                             
#>   Median                     32.0           23.9           20.8    
#>     90% CI               (22.6, 46.5)   (18.3, 29.2)   (12.9, 25.9)
#>   25% and 75%-ile         17.4, 65.3     9.8, 42.0      7.3, 37.1  
#>   Range                  0.3 to 155.5   0.1 to 154.1   0.6 to 80.7 

a_surv_time(
  df,
  .df_row = df,
  .var = "AVAL",
  is_event = "is_event"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>          row_name        formatted_cell indent_mod        row_label
#> 1          median                  32.0          0           Median
#> 2       median_ci        (22.51, 49.31)          0    Median 95% CI
#> 3    median_ci_3d 32.02 (22.51 - 49.31)          0  Median (95% CI)
#> 4       quantiles           17.4 - 65.3          0  25% and 75%-ile
#> 5 quantiles_lower 17.37 (10.13 - 22.51)          0 25%-ile (95% CI)
#> 6 quantiles_upper 65.28 (49.31 - 87.21)          0 75%-ile (95% CI)
#> 7    range_censor           0.8 to 63.5          0 Range (censored)
#> 8     range_event          0.3 to 155.5          0    Range (event)
#> 9           range           0.3 - 155.5          0        Min - Max
```
