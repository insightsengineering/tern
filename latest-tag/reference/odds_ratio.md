# Odds ratio estimation

**\[stable\]**

The analyze function `estimate_odds_ratio()` creates a layout element to
compare bivariate responses between two groups by estimating an odds
ratio and its confidence interval.

The primary analysis variable specified by `vars` is the group variable.
Additional variables can be included in the analysis via the `variables`
argument, which accepts `arm`, an arm variable, and `strata`, a
stratification variable. If more than two arm levels are present, they
can be combined into two groups using the `groups_list` argument.

## Usage

``` r
estimate_odds_ratio(
  lyt,
  vars,
  variables = list(arm = NULL, strata = NULL),
  conf_level = 0.95,
  groups_list = NULL,
  method = "exact",
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = vars,
  show_labels = "hidden",
  var_labels = vars,
  .stats = "or_ci",
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_odds_ratio(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  .df_row,
  variables = list(arm = NULL, strata = NULL),
  conf_level = 0.95,
  groups_list = NULL,
  method = "exact",
  ...
)

a_odds_ratio(
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

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- groups_list:

  (named `list` of `character`)  
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- method:

  (`string`)  
  whether to use the correct (`"exact"`) calculation in the conditional
  likelihood or one of the approximations. See
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  for details.

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

  additional arguments to
  [`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
  in order. For instance, to control formats (`format`), add a joint
  column for all groups (`incl_all`).

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- var_labels:

  (`character`)  
  variable labels.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'or_ci', 'n_tot'`

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

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

## Value

- `estimate_odds_ratio()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_odds_ratio()` to the table layout.

&nbsp;

- `s_odds_ratio()` returns a named list with the statistics `or_ci`
  (containing `est`, `lcl`, and `ucl`) and `n_tot`.

&nbsp;

- `a_odds_ratio()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `estimate_odds_ratio()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_odds_ratio()`: Statistics function which estimates the odds ratio
  between a treatment and a control. A `variables` list with `arm` and
  `strata` variable names must be passed if a stratified analysis is
  required.

- `a_odds_ratio()`: Formatted analysis function which is used as `afun`
  in `estimate_odds_ratio()`.

## Note

- This function uses logistic regression for unstratified analyses, and
  conditional logistic regression for stratified analyses. The Wald
  confidence interval is calculated with the specified confidence level.

- For stratified analyses, there is currently no implementation for
  conditional likelihood confidence intervals, therefore the likelihood
  confidence interval is not available as an option.

- When `vars` contains only responders or non-responders no odds ratio
  estimation is possible so the returned values will be `NA`.

## See also

Relevant helper function
[`h_odds_ratio()`](https://insightsengineering.github.io/tern/reference/h_odds_ratio.md).

## Examples

``` r
set.seed(12)
dta <- data.frame(
  rsp = sample(c(TRUE, FALSE), 100, TRUE),
  grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
  strata = factor(sample(c("C", "D"), 100, TRUE))
)

l <- basic_table() %>%
  split_cols_by(var = "grp", ref_group = "B") %>%
  estimate_odds_ratio(vars = "rsp")

build_table(l, df = dta)
#>                               A            B
#> ————————————————————————————————————————————
#> Odds Ratio (95% CI)   0.85 (0.38 - 1.88)    

# Unstratified analysis.
s_odds_ratio(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta
)
#> $or_ci
#>       est       lcl       ucl 
#> 0.8484848 0.3831831 1.8788053 
#> attr(,"label")
#> [1] "Odds Ratio (95% CI)"
#> 
#> $n_tot
#> n_tot 
#>   100 
#> attr(,"label")
#> [1] "Total n"
#> 

# Stratified analysis.
s_odds_ratio(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta,
  variables = list(arm = "grp", strata = "strata")
)
#> $or_ci
#>       est       lcl       ucl 
#> 0.7689750 0.3424155 1.7269154 
#> attr(,"label")
#> [1] "Odds Ratio (95% CI)"
#> 
#> $n_tot
#> n_tot 
#>   100 
#> attr(,"label")
#> [1] "Total n"
#> 

a_odds_ratio(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>              row_name     formatted_cell indent_mod           row_label
#> 1 Odds Ratio (95% CI) 0.85 (0.38 - 1.88)          0 Odds Ratio (95% CI)
#> 2             Total n                100          0             Total n
```
