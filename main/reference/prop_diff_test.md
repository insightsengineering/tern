# Difference test for two proportions

**\[stable\]**

The analyze function `test_proportion_diff()` creates a layout element
to test the difference between two proportions. The primary analysis
variable, `vars`, indicates whether a response has occurred for each
record. See the `method` parameter for options of methods to use to
calculate the p-value. The argument `alternative` specifies the
direction of the alternative hypothesis. Additionally, a stratification
variable can be supplied via the `strata` element of the `variables`
argument.

## Usage

``` r
test_proportion_diff(
  lyt,
  vars,
  variables = list(strata = NULL),
  method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh"),
  alternative = c("two.sided", "less", "greater"),
  var_labels = vars,
  na_str = default_na_str(),
  nested = TRUE,
  show_labels = "hidden",
  table_names = vars,
  section_div = NA_character_,
  ...,
  na_rm = TRUE,
  .stats = c("pval"),
  .stat_names = NULL,
  .formats = c(pval = "x.xxxx | (<0.0001)"),
  .labels = NULL,
  .indent_mods = c(pval = 1L)
)

s_test_proportion_diff(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh"),
  alternative = c("two.sided", "less", "greater"),
  ...
)

a_test_proportion_diff(
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

- method:

  (`string`)  
  one of `chisq`, `cmh`, `cmh_wh`, `fisher`, or `schouten`; specifies
  the test used to calculate the p-value.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

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

  Options are: `'pval'`

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

- `test_proportion_diff()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_test_proportion_diff()` to the table
  layout.

&nbsp;

- `s_test_proportion_diff()` returns a named `list` with a single item
  `pval` with an attribute `label` describing the method used. The
  p-value tests the null hypothesis that proportions in two groups are
  the same.

&nbsp;

- `a_test_proportion_diff()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `test_proportion_diff()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_test_proportion_diff()`: Statistics function which tests the
  difference between two proportions.

- `a_test_proportion_diff()`: Formatted analysis function which is used
  as `afun` in `test_proportion_diff()`.

## See also

[h_prop_diff_test](https://insightsengineering.github.io/tern/reference/h_prop_diff_test.md)

## Examples

``` r
dta <- data.frame(
  rsp = sample(c(TRUE, FALSE), 100, TRUE),
  grp = factor(rep(c("A", "B"), each = 50)),
  strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
)

# With `rtables` pipelines.
l <- basic_table() %>%
  split_cols_by(var = "grp", ref_group = "B") %>%
  test_proportion_diff(
    vars = "rsp",
    method = "cmh", variables = list(strata = "strata")
  )

build_table(l, df = dta)
#>                                              A      B
#> —————————————————————————————————————————————————————
#>   p-value (Cochran-Mantel-Haenszel Test)   0.3736    


## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
nex <- 100 # Number of example rows
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)
s_test_proportion_diff(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  variables = NULL,
  method = "chisq"
)
#> $pval
#> [1] 0.1107862
#> attr(,"label")
#> [1] "p-value (Chi-Squared Test)"
#> 
```
