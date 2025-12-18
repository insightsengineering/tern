# Incidence rate estimation

**\[stable\]**

The analyze function `estimate_incidence_rate()` creates a layout
element to estimate an event rate adjusted for person-years at risk,
otherwise known as incidence rate. The primary analysis variable
specified via `vars` is the person-years at risk. In addition to this
variable, the `n_events` variable for number of events observed (where a
value of 1 means an event was observed and 0 means that no event was
observed) must also be specified.

## Usage

``` r
estimate_incidence_rate(
  lyt,
  vars,
  n_events,
  id_var = "USUBJID",
  control = control_incidence_rate(),
  na_str = default_na_str(),
  nested = TRUE,
  summarize = FALSE,
  label_fmt = "%s - %.labels",
  ...,
  show_labels = "hidden",
  table_names = vars,
  .stats = c("person_years", "n_events", "rate", "rate_ci"),
  .stat_names = NULL,
  .formats = list(rate = "xx.xx", rate_ci = "(xx.xx, xx.xx)"),
  .labels = NULL,
  .indent_mods = NULL
)

s_incidence_rate(
  df,
  .var,
  ...,
  n_events,
  is_event = lifecycle::deprecated(),
  id_var = "USUBJID",
  control = control_incidence_rate()
)

a_incidence_rate(
  df,
  labelstr = "",
  label_fmt = "%s - %.labels",
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

- n_events:

  (`string`)  
  name of integer variable indicating whether an event has been
  observed (1) or not (0).

- id_var:

  (`string`)  
  name of variable used as patient identifier if `"n_unique"` is
  included in `.stats`. Defaults to `"USUBJID"`.

- control:

  (`list`)  
  parameters for estimation details, specified by using the helper
  function
  [`control_incidence_rate()`](https://insightsengineering.github.io/tern/reference/control_incidence_rate.md).
  Possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level for the estimated incidence rate.

  - `conf_type` (`string`)  
    `normal` (default), `normal_log`, `exact`, or `byar` for confidence
    interval type.

  - `input_time_unit` (`string`)  
    `day`, `week`, `month`, or `year` (default) indicating time unit for
    data input.

  - `num_pt_year` (`numeric`)  
    time unit for desired output (in person-years).

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- summarize:

  (`flag`)  
  whether the function should act as an analyze function
  (`summarize = FALSE`), or a summarize function (`summarize = TRUE`).
  Defaults to `FALSE`.

- label_fmt:

  (`string`)  
  how labels should be formatted after a row split occurs if
  `summarize = TRUE`. The string should use `"%s"` to represent row
  split levels, and `"%.labels"` to represent labels supplied to the
  `.labels` argument. Defaults to `"%s - %.labels"`.

- ...:

  additional arguments for the lower level functions.

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
  `'person_years', 'n_events', 'rate', 'rate_ci', 'n_unique', 'n_rate'`

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
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

## Value

- `estimate_incidence_rate()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_incidence_rate()` to the table
  layout.

&nbsp;

- `s_incidence_rate()` returns the following statistics:

  - `person_years`: Total person-years at risk.

  - `n_events`: Total number of events observed.

  - `rate`: Estimated incidence rate.

  - `rate_ci`: Confidence interval for the incidence rate.

  - `n_unique`: Total number of patients with at least one event
    observed.

  - `n_rate`: Total number of events observed & estimated incidence
    rate.

&nbsp;

- `a_incidence_rate()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `estimate_incidence_rate()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_incidence_rate()`: Statistics function which estimates the
  incidence rate and the associated confidence interval.

- `a_incidence_rate()`: Formatted analysis function which is used as
  `afun` in `estimate_incidence_rate()`.

## See also

[`control_incidence_rate()`](https://insightsengineering.github.io/tern/reference/control_incidence_rate.md)
and helper functions
[h_incidence_rate](https://insightsengineering.github.io/tern/reference/h_incidence_rate.md).

## Examples

``` r
df <- data.frame(
  USUBJID = as.character(seq(6)),
  CNSR = c(0, 1, 1, 0, 0, 0),
  AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
  ARM = factor(c("A", "A", "A", "B", "B", "B")),
  STRATA1 = factor(c("X", "Y", "Y", "X", "X", "Y"))
)
df$n_events <- 1 - df$CNSR

basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  estimate_incidence_rate(
    vars = "AVAL",
    n_events = "n_events",
    control = control_incidence_rate(
      input_time_unit = "month",
      num_pt_year = 100
    )
  ) %>%
  build_table(df)
#>                                            A                 B       
#>                                          (N=3)             (N=3)     
#> —————————————————————————————————————————————————————————————————————
#> Total patient-years at risk               3.8               5.2      
#> Number of adverse events observed          1                 3       
#> AE rate per 100 patient-years            26.20             57.23     
#> 95% CI                              (-25.15, 77.55)   (-7.53, 122.00)

# summarize = TRUE
basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_rows_by("STRATA1", child_labels = "visible") %>%
  estimate_incidence_rate(
    vars = "AVAL",
    n_events = "n_events",
    .stats = c("n_unique", "n_rate"),
    summarize = TRUE,
    label_fmt = "%.labels"
  ) %>%
  build_table(df)
#>                                                                          A         B   
#>                                                                        (N=3)     (N=3) 
#> ———————————————————————————————————————————————————————————————————————————————————————
#> X                                                                                      
#>   Total number of patients with at least one adverse event               1         2   
#>   Number of adverse events observed (AE rate per 100 patient-years)   1 (9.9)   2 (5.1)
#> Y                                                                                      
#>   Total number of patients with at least one adverse event               0         1   
#>   Number of adverse events observed (AE rate per 100 patient-years)   0 (0.0)   1 (4.3)

a_incidence_rate(
  df,
  .var = "AVAL",
  .df_row = df,
  n_events = "n_events"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name   formatted_cell indent_mod
#> 1 person_years            108.7          0
#> 2     n_events                4          0
#> 3         rate           3.6799          0
#> 4      rate_ci (0.0737, 7.2860)          0
#> 5     n_unique                4          0
#> 6       n_rate          4 (3.7)          0
#>                                                           row_label
#> 1                                       Total patient-years at risk
#> 2                                 Number of adverse events observed
#> 3                                     AE rate per 100 patient-years
#> 4                                                            95% CI
#> 5          Total number of patients with at least one adverse event
#> 6 Number of adverse events observed (AE rate per 100 patient-years)
```
