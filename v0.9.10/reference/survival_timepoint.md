# Survival time point analysis

**\[stable\]**

The analyze function `surv_timepoint()` creates a layout element to
analyze patient survival rates and difference of survival rates between
groups at a given time point. The primary analysis variable `vars` is
the time variable. Other required inputs are `time_point`, the numeric
time point of interest, and `is_event`, a variable that indicates
whether or not an event has occurred. The `method` argument is used to
specify whether you want to analyze survival estimations (`"surv"`),
difference in survival with the control (`"surv_diff"`), or both of
these (`"both"`).

## Usage

``` r
surv_timepoint(
  lyt,
  vars,
  time_point,
  is_event,
  control = control_surv_timepoint(),
  method = c("surv", "surv_diff", "both"),
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names_suffix = "",
  var_labels = "Time",
  show_labels = "visible",
  .stats = c("pt_at_risk", "event_free_rate", "rate_ci", "rate_diff", "rate_diff_ci",
    "ztest_pval"),
  .stat_names = NULL,
  .formats = list(rate_ci = "(xx.xx, xx.xx)"),
  .labels = NULL,
  .indent_mods = if (method == "both") {
     c(rate_diff = 1L, rate_diff_ci = 2L,
    ztest_pval = 2L)
 } else {
     c(rate_diff_ci = 1L, ztest_pval = 1L)
 }
)

s_surv_timepoint(
  df,
  .var,
  time_point,
  is_event,
  control = control_surv_timepoint(),
  ...
)

s_surv_timepoint_diff(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  time_point,
  control = control_surv_timepoint(),
  ...
)

a_surv_timepoint(
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

- time_point:

  (`numeric(1)`)  
  survival time point of interest.

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

- control:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_surv_timepoint()`](https://insightsengineering.github.io/tern/reference/control_surv_timepoint.md).
  Some possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level of the interval for survival rate.

  - `conf_type` (`string`)  
    confidence interval type. Options are "plain" (default), "log",
    "log-log", see more in
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
    Note option "none" is no longer supported.

- method:

  (`string`)  
  `"surv"` (survival estimations), `"surv_diff"` (difference in survival
  with the control), or `"both"`.

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

- table_names_suffix:

  (`string`)  
  optional suffix for the `table_names` used for the `rtables` to avoid
  warnings from duplicate table names.

- var_labels:

  (`character`)  
  variable labels.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are:
  `'pt_at_risk', 'event_free_rate', 'rate_se', 'rate_ci', 'event_free_rate_3d'`

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

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

## Value

- `surv_timepoint()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_surv_timepoint()` and/or
  `s_surv_timepoint_diff()` to the table layout depending on the value
  of `method`.

&nbsp;

- `s_surv_timepoint()` returns the statistics:

  - `pt_at_risk`: Patients remaining at risk.

  - `event_free_rate`: Event-free rate (%).

  - `rate_se`: Standard error of event free rate.

  - `rate_ci`: Confidence interval for event free rate.

  - `event_free_rate_3d`: Event-free rate (%) with Confidence interval.

&nbsp;

- `s_surv_timepoint_diff()` returns the statistics:

  - `rate_diff`: Event-free rate difference between two groups.

  - `rate_diff_ci`: Confidence interval for the difference.

  - `rate_diff_ci_3d`: Event-free rate difference and confidence
    interval between two groups.

  - `ztest_pval`: p-value to test the difference is 0.

&nbsp;

- `a_surv_timepoint()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `surv_timepoint()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_surv_timepoint()`: Statistics function which analyzes survival
  rate.

- `s_surv_timepoint_diff()`: Statistics function which analyzes
  difference between two survival rates.

- `a_surv_timepoint()`: Formatted analysis function which is used as
  `afun` in `surv_timepoint()`.

## Examples

``` r
library(dplyr)

adtte_f <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0
  )

# Survival at given time points.
basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  surv_timepoint(
    vars = "AVAL",
    var_labels = "Months",
    is_event = "is_event",
    time_point = 7
  ) %>%
  build_table(df = adtte_f)
#>                                    ARM A            ARM B            ARM C     
#>                                    (N=69)           (N=73)           (N=58)    
#> ———————————————————————————————————————————————————————————————————————————————
#> 7 Months                                                                       
#>   Patients remaining at risk         54               57               42      
#>   Event Free Rate (%)              84.89            79.43            75.50     
#>   95% CI                       (76.24, 93.53)   (70.15, 88.71)   (64.33, 86.67)

# Difference in survival at given time points.
basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  surv_timepoint(
    vars = "AVAL",
    var_labels = "Months",
    is_event = "is_event",
    time_point = 9,
    method = "surv_diff",
    .indent_mods = c("rate_diff" = 0L, "rate_diff_ci" = 2L, "ztest_pval" = 2L)
  ) %>%
  build_table(df = adtte_f)
#>                                   ARM A        ARM B            ARM C     
#>                                   (N=69)       (N=73)           (N=58)    
#> ——————————————————————————————————————————————————————————————————————————
#> 9 Months                                                                  
#>   Difference in Event Free Rate                -9.64            -13.03    
#>       95% CI                               (-22.80, 3.52)   (-27.59, 1.53)
#>       p-value (Z-test)                         0.1511           0.0794    

# Survival and difference in survival at given time points.
basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  surv_timepoint(
    vars = "AVAL",
    var_labels = "Months",
    is_event = "is_event",
    time_point = 9,
    method = "both"
  ) %>%
  build_table(df = adtte_f)
#>                                       ARM A            ARM B            ARM C     
#>                                       (N=69)           (N=73)           (N=58)    
#> ——————————————————————————————————————————————————————————————————————————————————
#> 9 Months                                                                          
#>   Patients remaining at risk            53               53               39      
#>   Event Free Rate (%)                 84.89            75.25            71.86     
#>   95% CI                          (76.24, 93.53)   (65.32, 85.17)   (60.14, 83.57)
#>   Difference in Event Free Rate                        -9.64            -13.03    
#>     95% CI                                         (-22.80, 3.52)   (-27.59, 1.53)
#>     p-value (Z-test)                                   0.1511           0.0794    

library(dplyr)

adtte_f <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0
  )

s_surv_timepoint(
  df = subset(adtte_f, ARMCD == "ARM A"),
  .var = "AVAL",
  is_event = "is_event",
  time_point = c(10),
  control = control_surv_timepoint()
)
#> $pt_at_risk
#> [1] 53
#> attr(,"label")
#> [1] "Patients remaining at risk"
#> 
#> $event_free_rate
#> [1] 84.88886
#> attr(,"label")
#> [1] "Event Free Rate (%)"
#> 
#> $rate_se
#> [1] 4.410585
#> attr(,"label")
#> [1] "Standard Error of Event Free Rate"
#> 
#> $rate_ci
#> [1] 76.24428 93.53345
#> attr(,"label")
#> [1] "95% CI"
#> 
#> $event_free_rate_3d
#> [1] 84.88886 76.24428 93.53345
#> attr(,"label")
#> [1] "Event Free Rate (95% CI)"
#> 
```
