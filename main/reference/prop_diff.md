# Proportion difference estimation

**\[stable\]**

The analysis function `estimate_proportion_diff()` creates a layout
element to estimate the difference in proportion of responders within a
studied population. The primary analysis variable, `vars`, is a logical
variable indicating whether a response has occurred for each record. See
the `method` parameter for options of methods to use when constructing
the confidence interval of the proportion difference. A stratification
variable can be supplied via the `strata` element of the `variables`
argument.

## Usage

``` r
estimate_proportion_diff(
  lyt,
  vars,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("waldcc", "wald", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = "cmh",
  var_labels = vars,
  na_str = default_na_str(),
  nested = TRUE,
  show_labels = "hidden",
  table_names = vars,
  section_div = NA_character_,
  ...,
  na_rm = TRUE,
  .stats = c("diff", "diff_ci"),
  .stat_names = NULL,
  .formats = c(diff = "xx.x", diff_ci = "(xx.x, xx.x)"),
  .labels = NULL,
  .indent_mods = c(diff = 0L, diff_ci = 1L)
)

s_proportion_diff(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("waldcc", "wald", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = "cmh",
  ...
)

a_proportion_diff(
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

- method:

  (`string`)  
  the method used for the confidence interval estimation.

- weights_method:

  (`string`)  
  weights method. Can be either `"cmh"` or `"heuristic"` and directs the
  way weights are estimated.

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

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'diff', 'diff_ci'`

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

## Value

- `estimate_proportion_diff()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_proportion_diff()` to the table
  layout.

&nbsp;

- `s_proportion_diff()` returns a named list of elements `diff` and
  `diff_ci`.

&nbsp;

- `a_proportion_diff()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `estimate_proportion_diff()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_proportion_diff()`: Statistics function estimating the difference
  in terms of responder proportion.

- `a_proportion_diff()`: Formatted analysis function which is used as
  `afun` in `estimate_proportion_diff()`.

## Note

When performing an unstratified analysis, methods `"cmh"`,
`"strat_newcombe"`, and `"strat_newcombecc"` are not permitted.

## See also

[`d_proportion_diff()`](https://insightsengineering.github.io/tern/reference/d_proportion_diff.md)

## Examples

``` r
## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
nex <- 100 # Number of example rows
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)

l <- basic_table() %>%
  split_cols_by(var = "grp", ref_group = "B") %>%
  estimate_proportion_diff(
    vars = "rsp",
    conf_level = 0.90,
    method = "ha"
  )

build_table(l, df = dta)
#>                                        A         B
#> ——————————————————————————————————————————————————
#> Difference in Response rate (%)       12.0        
#>   90% CI (Anderson-Hauck)         (-5.4, 29.4)    

s_proportion_diff(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  conf_level = 0.90,
  method = "ha"
)
#> $diff
#> diff_ha 
#>      12 
#> attr(,"label")
#> [1] "Difference in Response rate (%)"
#> 
#> $diff_ci
#> diff_ci_ha_l diff_ci_ha_u 
#>    -5.374519    29.374519 
#> attr(,"label")
#> [1] "90% CI (Anderson-Hauck)"
#> 

# CMH example with strata
s_proportion_diff(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  variables = list(strata = c("f1", "f2")),
  conf_level = 0.90,
  method = "cmh"
)
#> $diff
#> diff_cmh 
#> 12.27932 
#> attr(,"label")
#> [1] "Difference in Response rate (%)"
#> 
#> $diff_ci
#> diff_ci_cmh_l diff_ci_cmh_u 
#>     -2.657093     27.215725 
#> attr(,"label")
#> [1] "90% CI (CMH, without correction)"
#> 

a_proportion_diff(
  df = subset(dta, grp == "A"),
  .stats = c("diff"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  conf_level = 0.90,
  method = "ha"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod                       row_label
#> 1     diff             12          0 Difference in Response rate (%)
```
