# Analyze a pairwise Cox-PH model

**\[stable\]**

The analyze function `coxph_pairwise()` creates a layout element to
analyze a pairwise Cox-PH model.

This function can return statistics including p-value, hazard ratio
(HR), and HR confidence intervals from both stratified and unstratified
Cox-PH models. The variable(s) to be analyzed is specified via the
`vars` argument and any stratification factors via the `strata`
argument.

## Usage

``` r
coxph_pairwise(
  lyt,
  vars,
  strata = NULL,
  control = control_coxph(),
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  var_labels = "CoxPH",
  show_labels = "visible",
  table_names = vars,
  .stats = c("pvalue", "hr", "hr_ci"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_coxph_pairwise(
  df,
  .ref_group,
  .in_ref_col,
  .var,
  is_event,
  strata = NULL,
  strat = lifecycle::deprecated(),
  control = control_coxph(),
  ...
)

a_coxph_pairwise(
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

- strata:

  (`character` or `NULL`)  
  variable names indicating stratification factors.

- control:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).
  Some possible parameter options are:

  - `pval_method` (`string`)  
    p-value method for testing the null hypothesis that hazard ratio
    = 1. Default method is `"log-rank"` which comes from
    [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html),
    can also be set to `"wald"` or `"likelihood"` (from
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)).

  - `ties` (`string`)  
    specifying the method for tie handling. Default is `"efron"`, can
    also be set to `"breslow"` or `"exact"`. See more in
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

  - `conf_level` (`proportion`)  
    confidence level of the interval for HR.

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

  Options are: `'pvalue', 'hr', 'hr_ci', 'n_tot', 'n_tot_events'`

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

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

- strat:

  **\[deprecated\]** Please use the `strata` argument instead.

## Value

- `coxph_pairwise()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_coxph_pairwise()` to the table
  layout.

&nbsp;

- `s_coxph_pairwise()` returns the statistics:

  - `pvalue`: p-value to test the null hypothesis that hazard ratio = 1.

  - `hr`: Hazard ratio.

  - `hr_ci`: Confidence interval for hazard ratio.

  - `n_tot`: Total number of observations.

  - `n_tot_events`: Total number of events.

&nbsp;

- `a_coxph_pairwise()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `coxph_pairwise()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_coxph_pairwise()`: Statistics function which analyzes HR, CIs of
  HR, and p-value of a Cox-PH model.

- `a_coxph_pairwise()`: Formatted analysis function which is used as
  `afun` in `coxph_pairwise()`.

## Examples

``` r
library(dplyr)

adtte_f <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(is_event = CNSR == 0)

df <- adtte_f %>% filter(ARMCD == "ARM A")
df_ref_group <- adtte_f %>% filter(ARMCD == "ARM B")

basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  coxph_pairwise(
    vars = "AVAL",
    is_event = "is_event",
    var_labels = "Unstratified Analysis"
  ) %>%
  build_table(df = adtte_f)
#>                         ARM A       ARM B          ARM C    
#>                         (N=69)      (N=73)         (N=58)   
#> ————————————————————————————————————————————————————————————
#> Unstratified Analysis                                       
#>   p-value (log-rank)                0.0905         0.0086   
#>   Hazard Ratio                       1.41           1.81    
#>   95% CI                         (0.95, 2.09)   (1.16, 2.84)

basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  coxph_pairwise(
    vars = "AVAL",
    is_event = "is_event",
    var_labels = "Stratified Analysis",
    strata = "SEX",
    control = control_coxph(pval_method = "wald")
  ) %>%
  build_table(df = adtte_f)
#>                       ARM A       ARM B          ARM C    
#>                       (N=69)      (N=73)         (N=58)   
#> ——————————————————————————————————————————————————————————
#> Stratified Analysis                                       
#>   p-value (wald)                  0.0784         0.0066   
#>   Hazard Ratio                     1.44           1.89    
#>   95% CI                       (0.96, 2.15)   (1.19, 2.98)
```
