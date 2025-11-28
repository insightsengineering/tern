# Proportion estimation

**\[stable\]**

The analyze function `estimate_proportion()` creates a layout element to
estimate the proportion of responders within a studied population. The
primary analysis variable, `vars`, indicates whether a response has
occurred for each record. See the `method` parameter for options of
methods to use when constructing the confidence interval of the
proportion. Additionally, a stratification variable can be supplied via
the `strata` element of the `variables` argument.

## Usage

``` r
estimate_proportion(
  lyt,
  vars,
  conf_level = 0.95,
  method = c("waldcc", "wald", "clopper-pearson", "wilson", "wilsonc", "strat_wilson",
    "strat_wilsonc", "agresti-coull", "jeffreys"),
  weights = NULL,
  max_iterations = 50,
  variables = list(strata = NULL),
  long = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  show_labels = "hidden",
  table_names = vars,
  .stats = c("n_prop", "prop_ci"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_proportion(
  df,
  .var,
  conf_level = 0.95,
  method = c("waldcc", "wald", "clopper-pearson", "wilson", "wilsonc", "strat_wilson",
    "strat_wilsonc", "agresti-coull", "jeffreys"),
  weights = NULL,
  max_iterations = 50,
  variables = list(strata = NULL),
  long = FALSE,
  denom = c("n", "N_col", "N_row"),
  ...
)

a_proportion(
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

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string`)  
  the method used to construct the confidence interval for proportion of
  successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`,
  `wilson`, `wilsonc`, `strat_wilson`, `strat_wilsonc`, `agresti-coull`
  or `jeffreys`.

- weights:

  (`numeric` or `NULL`)  
  weights for each level of the strata. If `NULL`, they are estimated
  using the iterative algorithm proposed in Yan and Su (2010) that
  minimizes the weighted squared length of the confidence interval.

- max_iterations:

  (`count`)  
  maximum number of iterations for the iterative procedure used to find
  estimates of optimal weights.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- long:

  (`flag`)  
  whether a long description is required.

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

  Options are: `'n_prop', 'prop_ci'`

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

  (`logical` or `data.frame`)  
  if only a logical vector is used, it indicates whether each subject is
  a responder or not. `TRUE` represents a successful outcome. If a
  `data.frame` is provided, also the `strata` variable names must be
  provided in `variables` as a list element with the strata strings. In
  the case of `data.frame`, the logical vector of responses must be
  indicated as a variable name in `.var`.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

## Value

- `estimate_proportion()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_proportion()` to the table layout.

&nbsp;

- `s_proportion()` returns statistics `n_prop` (`n` and proportion) and
  `prop_ci` (proportion CI) for a given variable.

&nbsp;

- `a_proportion()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `estimate_proportion()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_proportion()`: Statistics function estimating a proportion along
  with its confidence interval.

- `a_proportion()`: Formatted analysis function which is used as `afun`
  in `estimate_proportion()`.

## See also

[h_proportions](https://insightsengineering.github.io/tern/reference/h_proportions.md)

## Examples

``` r
dta_test <- data.frame(
  USUBJID = paste0("S", 1:12),
  ARM = rep(LETTERS[1:3], each = 4),
  AVAL = rep(LETTERS[1:3], each = 4)
) %>%
  dplyr::mutate(is_rsp = AVAL == "A")

basic_table() %>%
  split_cols_by("ARM") %>%
  estimate_proportion(vars = "is_rsp") %>%
  build_table(df = dta_test)
#>                                        A              B             C     
#> ——————————————————————————————————————————————————————————————————————————
#> Responders                        4 (100.0%)      0 (0.0%)      0 (0.0%)  
#> 95% CI (Wald, with correction)   (87.5, 100.0)   (0.0, 12.5)   (0.0, 12.5)

# Case with only logical vector.
rsp_v <- c(1, 0, 1, 0, 1, 1, 0, 0)
s_proportion(rsp_v)
#> $n_prop
#> [1] 4.0 0.5
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1]  9.102404 90.897596
#> attr(,"label")
#> [1] "95% CI (Wald, with correction)"
#> 

# Example for Stratified Wilson CI
nex <- 100 # Number of example rows
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)

s_proportion(
  df = dta,
  .var = "rsp",
  variables = list(strata = c("f1", "f2")),
  conf_level = 0.90,
  method = "strat_wilson"
)
#> $n_prop
#> [1] 47.00  0.47
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#>    lower    upper 
#> 38.15468 53.64792 
#> attr(,"label")
#> [1] "90% CI (Stratified Wilson, without correction)"
#> 
```
