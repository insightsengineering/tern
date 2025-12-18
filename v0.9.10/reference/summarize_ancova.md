# Summarize analysis of covariance (ANCOVA) results

**\[stable\]**

The analyze function `summarize_ancova()` creates a layout element to
summarize ANCOVA results.

This function can be used to analyze multiple endpoints and/or multiple
timepoints within the response variable(s) specified as `vars`.

Additional variables for the analysis, namely an arm (grouping) variable
and covariate variables, can be defined via the `variables` argument.
See below for more details on how to specify `variables`. An interaction
term can be implemented in the model if needed. The interaction variable
that should interact with the arm variable is specified via the
`interaction_term` parameter, and the specific value of
`interaction_term` for which to extract the ANCOVA results via the
`interaction_y` parameter.

## Usage

``` r
summarize_ancova(
  lyt,
  vars,
  variables,
  conf_level,
  interaction_y = FALSE,
  interaction_item = NULL,
  weights_emmeans = NULL,
  var_labels,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  show_labels = "visible",
  table_names = vars,
  .stats = c("n", "lsmean", "lsmean_diff", "lsmean_diff_ci", "pval"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = list(lsmean_diff_ci = 1L, pval = 1L)
)

s_ancova(
  df,
  .var,
  .df_row,
  .ref_group,
  .in_ref_col,
  variables,
  conf_level,
  interaction_y = FALSE,
  interaction_item = NULL,
  weights_emmeans = NULL,
  ...
)

a_ancova(
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
  list of additional analysis variables, with expected elements:

  - `arm` (`string`)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of `arm`
    variable is taken as the reference group.

  - `covariates` (`character`)  
    a vector that can contain single variable names (such as `"X1"`),
    and/or interaction terms indicated by `"X1 * X2"`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- interaction_y:

  (`string` or `flag`)  
  a selected item inside of the `interaction_item` variable which will
  be used to select the specific ANCOVA results. if the interaction is
  not needed, the default option is `FALSE`.

- interaction_item:

  (`string` or `NULL`)  
  name of the variable that should have interactions with arm. if the
  interaction is not needed, the default option is `NULL`.

- weights_emmeans:

  (`string` or `NULL`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)

- var_labels:

  (`character`)  
  variable labels.

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

  Options are: `'n', 'lsmean', 'lsmean_diff', 'lsmean_diff_ci', 'pval'`

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

- .df_row:

  (`data.frame`)  
  data set that includes all the variables that are called in `.var` and
  `variables`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

## Value

- `summarize_ancova()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_ancova()` to the table layout.

&nbsp;

- `s_ancova()` returns a named list of 5 statistics:

  - `n`: Count of complete sample size for the group.

  - `lsmean`: Estimated marginal means in the group.

  - `lsmean_diff`: Difference in estimated marginal means in comparison
    to the reference group. If working with the reference group, this
    will be empty.

  - `lsmean_diff_ci`: Confidence level for difference in estimated
    marginal means in comparison to the reference group.

  - `pval`: p-value (not adjusted for multiple comparisons).

&nbsp;

- `a_ancova()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `summarize_ancova()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_ancova()`: Statistics function that produces a named list of
  results of the investigated linear model.

- `a_ancova()`: Formatted analysis function which is used as `afun` in
  `summarize_ancova()`.

## Examples

``` r
basic_table() %>%
  split_cols_by("Species", ref_group = "setosa") %>%
  add_colcounts() %>%
  summarize_ancova(
    vars = "Petal.Length",
    variables = list(arm = "Species", covariates = NULL),
    table_names = "unadj",
    conf_level = 0.95, var_labels = "Unadjusted comparison",
    .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
  ) %>%
  summarize_ancova(
    vars = "Petal.Length",
    variables = list(arm = "Species", covariates = c("Sepal.Length", "Sepal.Width")),
    table_names = "adj",
    conf_level = 0.95, var_labels = "Adjusted comparison (covariates: Sepal.Length and Sepal.Width)"
  ) %>%
  build_table(iris)
#>                                                                  setosa    versicolor     virginica  
#>                                                                  (N=50)      (N=50)         (N=50)   
#> —————————————————————————————————————————————————————————————————————————————————————————————————————
#> Unadjusted comparison                                                                                
#>   n                                                                50          50             50     
#>   Mean                                                            1.46        4.26           5.55    
#>   Difference in Means                                                         2.80           4.09    
#>     95% CI                                                                (2.63, 2.97)   (3.92, 4.26)
#>     p-value                                                                 <0.0001        <0.0001   
#> Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                       
#>   n                                                                50          50             50     
#>   Adjusted Mean                                                   2.02        4.19           5.07    
#>   Difference in Adjusted Means                                                2.17           3.05    
#>     95% CI                                                                (1.96, 2.38)   (2.81, 3.29)
#>     p-value                                                                 <0.0001        <0.0001   
```
