# Analyze variables

**\[stable\]**

The analyze function `analyze_vars()` creates a layout element to
summarize one or more variables, using the S3 generic function
`s_summary()` to calculate a list of summary statistics. A list of all
available statistics for numeric variables can be viewed by running
`get_stats("analyze_vars_numeric")` and for non-numeric variables by
running `get_stats("analyze_vars_counts")`. Use the `.stats` parameter
to specify the statistics to include in your output summary table. Use
`compare_with_ref_group = TRUE` to compare the variable with reference
groups.

## Usage

``` r
analyze_vars(
  lyt,
  vars,
  var_labels = vars,
  na_str = default_na_str(),
  na_str_drop = "<Missing>",
  nested = TRUE,
  show_labels = "default",
  table_names = vars,
  section_div = NA_character_,
  ...,
  na_rm = TRUE,
  compare_with_ref_group = FALSE,
  .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_summary(x, ...)

# S3 method for class 'numeric'
s_summary(x, control = control_analyze_vars(), ...)

# S3 method for class 'factor'
s_summary(x, denom = c("n", "N_col", "N_row"), ...)

# S3 method for class 'character'
s_summary(x, denom = c("n", "N_col", "N_row"), ...)

# S3 method for class 'logical'
s_summary(x, denom = c("n", "N_col", "N_row"), ...)

a_summary(
  x,
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

- var_labels:

  (`character`)  
  variable labels.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- na_str_drop:

  (`string`)  
  Additional `NA` string to be dropped from factor calculations. If
  `NULL` nothing will be removed beyond standard `NA` handling.

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

  additional arguments passed to `s_summary()`, including:

  - `denom`: (`string`) See parameter description below.

  - `.N_row`: (`numeric(1)`) Row-wise N (row group count) for the group
    of observations being analyzed (i.e. with no column-based
    subsetting).

  - `.N_col`: (`numeric(1)`) Column-wise N (column count) for the full
    column being tabulated within.

  - `verbose`: (`flag`) Whether additional warnings and messages should
    be printed. Mainly used to print out information about factor
    casting. Defaults to `TRUE`. Used for `character`/`factor` variables
    only.

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- compare_with_ref_group:

  (`flag`)  
  whether comparison statistics should be analyzed instead of summary
  statistics (`compare_with_ref_group = TRUE` adds `pval` statistic
  comparing against reference group).

- .stats:

  (`character`)  
  statistics to select for the table.

  Options for numeric variables are:
  `'n', 'sum', 'mean', 'sd', 'se', 'mean_sd', 'mean_se', 'mean_ci', 'mean_sei', 'mean_sdi', 'mean_pval', 'median', 'mad', 'median_ci', 'quantiles', 'iqr', 'range', 'min', 'max', 'median_range', 'cv', 'geom_mean', 'geom_sd', 'geom_mean_sd', 'geom_mean_ci', 'geom_cv', 'median_ci_3d', 'mean_ci_3d', 'geom_mean_ci_3d'`

  Options for non-numeric variables are:
  `'n', 'count', 'count_fraction', 'count_fraction_fixed_dp', 'fraction', 'n_blq'`

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

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- control:

  (`list`)  
  parameters for descriptive statistics details, specified by using the
  helper function
  [`control_analyze_vars()`](https://insightsengineering.github.io/tern/reference/control_analyze_vars.md).
  Some possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level of the interval for mean and median.

  - `quantiles` (`numeric(2)`)  
    vector of length two to specify the quantiles.

  - `quantile_type` (`numeric(1)`)  
    between 1 and 9 selecting quantile algorithms to be used. See more
    about `type` in
    [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

  - `test_mean` (`numeric(1)`)  
    value to test against the mean under the null hypothesis when
    calculating p-value.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

## Value

- `analyze_vars()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_summary()` to the table layout.

&nbsp;

- `s_summary()` returns different statistics depending on the class of
  `x`.

&nbsp;

- If `x` is of class `numeric`, returns a `list` with the following
  named `numeric` items:

  - `n`: The [`length()`](https://rdrr.io/r/base/length.html) of `x`.

  - `sum`: The [`sum()`](https://rdrr.io/r/base/sum.html) of `x`.

  - `mean`: The [`mean()`](https://rdrr.io/r/base/mean.html) of `x`.

  - `sd`: The [`stats::sd()`](https://rdrr.io/r/stats/sd.html) of `x`.

  - `se`: The standard error of `x` mean, i.e.:
    (`sd(x) / sqrt(length(x))`).

  - `mean_sd`: The [`mean()`](https://rdrr.io/r/base/mean.html) and
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html) of `x`.

  - `mean_se`: The [`mean()`](https://rdrr.io/r/base/mean.html) of `x`
    and its standard error (see above).

  - `mean_ci`: The CI for the mean of `x` (from
    [`stat_mean_ci()`](https://insightsengineering.github.io/tern/reference/stat_mean_ci.md)).

  - `mean_sei`: The SE interval for the mean of `x`, i.e.:
    ([`mean()`](https://rdrr.io/r/base/mean.html) -/+
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html) /
    [`sqrt()`](https://rdrr.io/r/base/MathFun.html)).

  - `mean_sdi`: The SD interval for the mean of `x`, i.e.:
    ([`mean()`](https://rdrr.io/r/base/mean.html) -/+
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html)).

  - `mean_pval`: The two-sided p-value of the mean of `x` (from
    [`stat_mean_pval()`](https://insightsengineering.github.io/tern/reference/stat_mean_pval.md)).

  - `median`: The
    [`stats::median()`](https://rdrr.io/r/stats/median.html) of `x`.

  - `mad`: The median absolute deviation of `x`, i.e.:
    ([`stats::median()`](https://rdrr.io/r/stats/median.html) of `xc`,
    where `xc` = `x` -
    [`stats::median()`](https://rdrr.io/r/stats/median.html)).

  - `median_ci`: The CI for the median of `x` (from
    [`stat_median_ci()`](https://insightsengineering.github.io/tern/reference/stat_median_ci.md)).

  - `quantiles`: Two sample quantiles of `x` (from
    [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)).

  - `iqr`: The [`stats::IQR()`](https://rdrr.io/r/stats/IQR.html) of
    `x`.

  - `range`: The
    [`range_noinf()`](https://insightsengineering.github.io/tern/reference/range_noinf.md)
    of `x`.

  - `min`: The [`max()`](https://rdrr.io/r/base/Extremes.html) of `x`.

  - `max`: The [`min()`](https://rdrr.io/r/base/Extremes.html) of `x`.

  - `median_range`: The
    [`median()`](https://rdrr.io/r/stats/median.html) and
    [`range_noinf()`](https://insightsengineering.github.io/tern/reference/range_noinf.md)
    of `x`.

  - `cv`: The coefficient of variation of `x`, i.e.:
    ([`stats::sd()`](https://rdrr.io/r/stats/sd.html) /
    [`mean()`](https://rdrr.io/r/base/mean.html) \* 100).

  - `geom_mean`: The geometric mean of `x`, i.e.: (`exp(mean(log(x)))`).

  - `geom_cv`: The geometric coefficient of variation of `x`, i.e.:
    (`sqrt(exp(sd(log(x)) ^ 2) - 1) * 100`).

&nbsp;

- If `x` is of class `factor` or converted from `character`, returns a
  `list` with named `numeric` items:

  - `n`: The [`length()`](https://rdrr.io/r/base/length.html) of `x`.

  - `count`: A list with the number of cases for each level of the
    factor `x`.

  - `count_fraction`: Similar to `count` but also includes the
    proportion of cases for each level of the factor `x` relative to the
    denominator, or `NA` if the denominator is zero.

&nbsp;

- If `x` is of class `logical`, returns a `list` with named `numeric`
  items:

  - `n`: The [`length()`](https://rdrr.io/r/base/length.html) of `x`
    (possibly after removing `NA`s).

  - `count`: Count of `TRUE` in `x`.

  - `count_fraction`: Count and proportion of `TRUE` in `x` relative to
    the denominator, or `NA` if the denominator is zero. Note that `NA`s
    in `x` are never counted or leading to `NA` here.

&nbsp;

- `a_summary()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Details

**Automatic digit formatting:** The number of digits to display can be
automatically determined from the analyzed variable(s) (`vars`) for
certain statistics by setting the statistic format to `"auto"` in
`.formats`. This utilizes the
[`format_auto()`](https://insightsengineering.github.io/tern/reference/format_auto.md)
formatting function. Note that only data for the current row & variable
(for all columns) will be considered (`.df_row[[.var]]`, see
[`rtables::additional_fun_params`](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html))
and not the whole dataset.

## Functions

- `analyze_vars()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_summary()`: S3 generic function to produces a variable summary.

- `s_summary(numeric)`: Method for `numeric` class.

- `s_summary(factor)`: Method for `factor` class.

- `s_summary(character)`: Method for `character` class. This makes an
  automatic conversion to factor (with a warning) and then forwards to
  the method for factors.

- `s_summary(logical)`: Method for `logical` class.

- `a_summary()`: Formatted analysis function which is used as `afun` in
  `analyze_vars()` and
  [`compare_vars()`](https://insightsengineering.github.io/tern/reference/compare_variables.md)
  and as `cfun` in
  [`summarize_colvars()`](https://insightsengineering.github.io/tern/reference/summarize_colvars.md).

## Note

- If `x` is an empty vector, `NA` is returned. This is the expected
  feature so as to return `rcell` content in `rtables` when the
  intersection of a column and a row delimits an empty data selection.

- When the `mean` function is applied to an empty vector, `NA` will be
  returned instead of `NaN`, the latter being standard behavior in R.

&nbsp;

- If `x` is an empty `factor`, a list is still returned for `counts`
  with one element per factor level. If there are no levels in `x`, the
  function fails.

- If factor variables contain `NA`, these `NA` values are excluded by
  default. To include `NA` values set `na_rm = FALSE` and missing values
  will be displayed as an `NA` level. Alternatively, an explicit factor
  level can be defined for `NA` values during pre-processing via
  [`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md) -
  the default `na_level` (`"<Missing>"`) will also be excluded when
  `na_rm` is set to `TRUE`.

&nbsp;

- Automatic conversion of character to factor does not guarantee that
  the table can be generated correctly. In particular for sparse tables
  this very likely can fail. It is therefore better to always
  pre-process the dataset such that factors are manually created from
  character variables before passing the dataset to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).

&nbsp;

- To use for comparison (with additional p-value statistic), parameter
  `compare_with_ref_group` must be set to `TRUE`.

- Ensure that either all `NA` values are converted to an explicit `NA`
  level or all `NA` values are left as is.

## Examples

``` r
## Fabricated dataset.
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  PARAMCD = rep("lab", 6 * 3),
  AVISIT  = rep(paste0("V", 1:3), 6),
  ARM     = rep(LETTERS[1:3], rep(6, 3)),
  AVAL    = c(9:1, rep(NA, 9))
)

# `analyze_vars()` in `rtables` pipelines
## Default output within a `rtables` pipeline.
l <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by(var = "AVISIT") %>%
  analyze_vars(vars = "AVAL")

build_table(l, df = dta_test)
#>                   A           B       C 
#> ————————————————————————————————————————
#> V1                                      
#>   n               2           1       0 
#>   Mean (SD)   7.5 (2.1)   3.0 (NA)    NA
#>   Median         7.5         3.0      NA
#>   Min - Max   6.0 - 9.0   3.0 - 3.0   NA
#> V2                                      
#>   n               2           1       0 
#>   Mean (SD)   6.5 (2.1)   2.0 (NA)    NA
#>   Median         6.5         2.0      NA
#>   Min - Max   5.0 - 8.0   2.0 - 2.0   NA
#> V3                                      
#>   n               2           1       0 
#>   Mean (SD)   5.5 (2.1)   1.0 (NA)    NA
#>   Median         5.5         1.0      NA
#>   Min - Max   4.0 - 7.0   1.0 - 1.0   NA

## Select and format statistics output.
l <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by(var = "AVISIT") %>%
  analyze_vars(
    vars = "AVAL",
    .stats = c("n", "mean_sd", "quantiles"),
    .formats = c("mean_sd" = "xx.x, xx.x"),
    .labels = c(n = "n", mean_sd = "Mean, SD", quantiles = c("Q1 - Q3"))
  )

build_table(l, df = dta_test)
#>                  A           B       C 
#> ———————————————————————————————————————
#> V1                                     
#>   n              2           1       0 
#>   Mean, SD   7.5, 2.1     3.0, NA    NA
#>   Q1 - Q3    6.0 - 9.0   3.0 - 3.0   NA
#> V2                                     
#>   n              2           1       0 
#>   Mean, SD   6.5, 2.1     2.0, NA    NA
#>   Q1 - Q3    5.0 - 8.0   2.0 - 2.0   NA
#> V3                                     
#>   n              2           1       0 
#>   Mean, SD   5.5, 2.1     1.0, NA    NA
#>   Q1 - Q3    4.0 - 7.0   1.0 - 1.0   NA

## Use arguments interpreted by `s_summary`.
l <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by(var = "AVISIT") %>%
  analyze_vars(vars = "AVAL", na_rm = FALSE)

build_table(l, df = dta_test)
#>                   A       B    C 
#> —————————————————————————————————
#> V1                               
#>   n               2       2    2 
#>   Mean (SD)   7.5 (2.1)   NA   NA
#>   Median         7.5      NA   NA
#>   Min - Max   6.0 - 9.0   NA   NA
#> V2                               
#>   n               2       2    2 
#>   Mean (SD)   6.5 (2.1)   NA   NA
#>   Median         6.5      NA   NA
#>   Min - Max   5.0 - 8.0   NA   NA
#> V3                               
#>   n               2       2    2 
#>   Mean (SD)   5.5 (2.1)   NA   NA
#>   Median         5.5      NA   NA
#>   Min - Max   4.0 - 7.0   NA   NA

## Handle `NA` levels first when summarizing factors.
dta_test$AVISIT <- NA_character_
dta_test <- df_explicit_na(dta_test)
l <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  analyze_vars(vars = "AVISIT", na_rm = FALSE)

build_table(l, df = dta_test)
#>                A          B          C    
#> ——————————————————————————————————————————
#> n              6          6          6    
#> <Missing>   6 (100%)   6 (100%)   6 (100%)

# auto format
dt <- data.frame("VAR" = c(0.001, 0.2, 0.0011000, 3, 4))
basic_table() %>%
  analyze_vars(
    vars = "VAR",
    .stats = c("n", "mean", "mean_sd", "range"),
    .formats = c("mean_sd" = "auto", "range" = "auto")
  ) %>%
  build_table(dt)
#>                  all obs     
#> —————————————————————————————
#> n                   5        
#> Mean               1.4       
#> Mean (SD)   1.44042 (1.91481)
#> Min - Max    0.0010 - 4.0000 

# `s_summary.numeric`

## Basic usage: empty numeric returns NA-filled items.
s_summary(numeric())
#> $n
#> n 
#> 0 
#> 
#> $sum
#> sum 
#>  NA 
#> 
#> $mean
#> mean 
#>   NA 
#> 
#> $sd
#> sd 
#> NA 
#> 
#> $se
#> se 
#> NA 
#> 
#> $mean_sd
#> mean   sd 
#>   NA   NA 
#> 
#> $mean_se
#> mean   se 
#>   NA   NA 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> median 
#>     NA 
#> 
#> $mad
#> mad 
#>  NA 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>            NA            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>            NA            NA 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> iqr 
#>  NA 
#> 
#> $range
#> min max 
#>  NA  NA 
#> 
#> $min
#> min 
#>  NA 
#> 
#> $max
#> max 
#>  NA 
#> 
#> $median_range
#> median    min    max 
#>     NA     NA     NA 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#> cv 
#> NA 
#> 
#> $geom_mean
#> geom_mean 
#>        NA 
#> 
#> $geom_sd
#> geom_sd 
#>      NA 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>        NA        NA 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#> geom_cv 
#>      NA 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 

## Management of NA values.
x <- c(NA_real_, 1)
s_summary(x, na_rm = TRUE)
#> $n
#> n 
#> 1 
#> 
#> $sum
#> sum 
#>   1 
#> 
#> $mean
#> mean 
#>    1 
#> 
#> $sd
#> sd 
#> NA 
#> 
#> $se
#> se 
#> NA 
#> 
#> $mean_sd
#> mean   sd 
#>    1   NA 
#> 
#> $mean_se
#> mean   se 
#>    1   NA 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>           1          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> median 
#>      1 
#> 
#> $mad
#> mad 
#>   0 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>             1            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>             1             1 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> iqr 
#>   0 
#> 
#> $range
#> min max 
#>   1   1 
#> 
#> $min
#> min 
#>   1 
#> 
#> $max
#> max 
#>   1 
#> 
#> $median_range
#> median    min    max 
#>      1      1      1 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#> cv 
#> NA 
#> 
#> $geom_mean
#> geom_mean 
#>         1 
#> 
#> $geom_sd
#> geom_sd 
#>      NA 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>         1        NA 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#> geom_cv 
#>      NA 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>           1          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
s_summary(x, na_rm = FALSE)
#> $n
#> n 
#> 2 
#> 
#> $sum
#> sum 
#>  NA 
#> 
#> $mean
#> mean 
#>   NA 
#> 
#> $sd
#> sd 
#> NA 
#> 
#> $se
#> se 
#> NA 
#> 
#> $mean_sd
#> mean   sd 
#>   NA   NA 
#> 
#> $mean_se
#> mean   se 
#>   NA   NA 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> median 
#>     NA 
#> 
#> $mad
#> mad 
#>  NA 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>            NA            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>            NA            NA 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> iqr 
#>  NA 
#> 
#> $range
#> min max 
#>  NA  NA 
#> 
#> $min
#> min 
#>  NA 
#> 
#> $max
#> max 
#>  NA 
#> 
#> $median_range
#> median    min    max 
#>     NA     NA     NA 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#> cv 
#> NA 
#> 
#> $geom_mean
#> geom_mean 
#>        NA 
#> 
#> $geom_sd
#> geom_sd 
#>      NA 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>        NA        NA 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#> geom_cv 
#>      NA 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 

x <- c(NA_real_, 1, 2)
s_summary(x)
#> $n
#> n 
#> 2 
#> 
#> $sum
#> sum 
#>   3 
#> 
#> $mean
#> mean 
#>  1.5 
#> 
#> $sd
#>        sd 
#> 0.7071068 
#> 
#> $se
#>  se 
#> 0.5 
#> 
#> $mean_sd
#>      mean        sd 
#> 1.5000000 0.7071068 
#> 
#> $mean_se
#> mean   se 
#>  1.5  0.5 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>   -4.853102    7.853102 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>            1            2 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>    0.7928932    2.2071068 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>    1.500000   -4.853102    7.853102 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#>   p_value 
#> 0.2048328 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> median 
#>    1.5 
#> 
#> $mad
#> mad 
#>   0 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>           1.5            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>             1             2 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> iqr 
#>   1 
#> 
#> $range
#> min max 
#>   1   2 
#> 
#> $min
#> min 
#>   1 
#> 
#> $max
#> max 
#>   2 
#> 
#> $median_range
#> median    min    max 
#>    1.5    1.0    2.0 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#>       cv 
#> 47.14045 
#> 
#> $geom_mean
#> geom_mean 
#>  1.414214 
#> 
#> $geom_sd
#>  geom_sd 
#> 1.632527 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>  1.414214  1.632527 
#> 
#> $geom_mean_ci
#>  mean_ci_lwr  mean_ci_upr 
#>   0.01729978 115.60839614 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#>  geom_cv 
#> 52.10922 
#> 
#> $geom_mean_ci_3d
#>    geom_mean  mean_ci_lwr  mean_ci_upr 
#>   1.41421356   0.01729978 115.60839614 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 

## Benefits in `rtables` contructions:
dta_test <- data.frame(
  Group = rep(LETTERS[seq(3)], each = 2),
  sub_group = rep(letters[seq(2)], each = 3),
  x = seq(6)
)

## The summary obtained in with `rtables`:
basic_table() %>%
  split_cols_by(var = "Group") %>%
  split_rows_by(var = "sub_group") %>%
  analyze(vars = "x", afun = s_summary) %>%
  build_table(df = dta_test)
#>                                                           A                                  B                                C                          
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> a                                                                                                                                                        
#>   n                                                       2                                  1                                0                          
#>   sum                                                     3                                  3                                NA                         
#>   mean                                                   1.5                                 3                                NA                         
#>   sd                                              0.707106781186548                         NA                                NA                         
#>   se                                                     0.5                                NA                                NA                         
#>   mean_sd                                       1.5, 0.707106781186548                     3, NA                              NA                         
#>   mean_se                                              1.5, 0.5                            3, NA                              NA                         
#>   Mean 95% CI                            -4.85310236808735, 7.85310236808735                NA                                NA                         
#>   Mean -/+ 1xSE                                          1, 2                               NA                                NA                         
#>   Mean -/+ 1xSD                          0.792893218813452, 2.20710678118655                NA                                NA                         
#>   Mean (95% CI)                        1.5, -4.85310236808735, 7.85310236808735          3, NA, NA                            NA                         
#>   Mean p-value (H0: mean = 0)                     0.204832764699133                         NA                                NA                         
#>   median                                                 1.5                                 3                                NA                         
#>   mad                                                     0                                  0                                NA                         
#>   Median 95% CI                                           NA                                NA                                NA                         
#>   Median (95% CI)                                    1.5, NA, NA                         3, NA, NA                            NA                         
#>   25% and 75%-ile                                        1, 2                              3, 3                               NA                         
#>   iqr                                                     1                                  0                                NA                         
#>   range                                                  1, 2                              3, 3                               NA                         
#>   min                                                     1                                  3                                NA                         
#>   max                                                     2                                  3                                NA                         
#>   Median (Min - Max)                                  1.5, 1, 2                           3, 3, 3                             NA                         
#>   cv                                               47.1404520791032                         NA                                NA                         
#>   geom_mean                                        1.41421356237309                          3                                NA                         
#>   geom_sd                                          1.63252691943815                         NA                                NA                         
#>   geom_mean_sd                            1.41421356237309, 1.63252691943815               3, NA                              NA                         
#>   Geometric Mean 95% CI                  0.0172997815631007, 115.608396135236               NA                                NA                         
#>   geom_cv                                          52.1092246837487                         NA                                NA                         
#>   Geometric Mean (95% CI)       1.41421356237309, 0.0172997815631007, 115.608396135236   3, NA, NA                            NA                         
#> b                                                                                                                                                        
#>   n                                                       0                                  1                                2                          
#>   sum                                                     NA                                 4                                11                         
#>   mean                                                    NA                                 4                               5.5                         
#>   sd                                                      NA                                NA                        0.707106781186548                  
#>   se                                                      NA                                NA                               0.5                         
#>   mean_sd                                                 NA                               4, NA                    5.5, 0.707106781186548               
#>   mean_se                                                 NA                               4, NA                           5.5, 0.5                      
#>   Mean 95% CI                                             NA                                NA               -0.853102368087347, 11.8531023680873        
#>   Mean -/+ 1xSE                                           NA                                NA                               5, 6                        
#>   Mean -/+ 1xSD                                           NA                                NA                4.79289321881345, 6.20710678118655         
#>   Mean (95% CI)                                           NA                             4, NA, NA        5.5, -0.853102368087347, 11.8531023680873      
#>   Mean p-value (H0: mean = 0)                             NA                                NA                        0.0577158767526089                 
#>   median                                                  NA                                 4                               5.5                         
#>   mad                                                     NA                                 0                                0                          
#>   Median 95% CI                                           NA                                NA                                NA                         
#>   Median (95% CI)                                         NA                             4, NA, NA                       5.5, NA, NA                     
#>   25% and 75%-ile                                         NA                               4, 4                              5, 6                        
#>   iqr                                                     NA                                 0                                1                          
#>   range                                                   NA                               4, 4                              5, 6                        
#>   min                                                     NA                                 4                                5                          
#>   max                                                     NA                                 4                                6                          
#>   Median (Min - Max)                                      NA                              4, 4, 4                         5.5, 5, 6                      
#>   cv                                                      NA                                NA                         12.8564869306645                  
#>   geom_mean                                               NA                                 4                         5.47722557505166                  
#>   geom_sd                                                 NA                                NA                         1.13760003310263                  
#>   geom_mean_sd                                            NA                               4, NA              5.47722557505166, 1.13760003310263         
#>   Geometric Mean 95% CI                                   NA                                NA                1.71994304449266, 17.4424380482025         
#>   geom_cv                                                 NA                                NA                         12.945835316564                   
#>   Geometric Mean (95% CI)                                 NA                             4, NA, NA   5.47722557505166, 1.71994304449266, 17.4424380482025

## By comparison with `lapply`:
X <- split(dta_test, f = with(dta_test, interaction(Group, sub_group)))
lapply(X, function(x) s_summary(x$x))
#> $A.a
#> $A.a$n
#> n 
#> 2 
#> 
#> $A.a$sum
#> sum 
#>   3 
#> 
#> $A.a$mean
#> mean 
#>  1.5 
#> 
#> $A.a$sd
#>        sd 
#> 0.7071068 
#> 
#> $A.a$se
#>  se 
#> 0.5 
#> 
#> $A.a$mean_sd
#>      mean        sd 
#> 1.5000000 0.7071068 
#> 
#> $A.a$mean_se
#> mean   se 
#>  1.5  0.5 
#> 
#> $A.a$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>   -4.853102    7.853102 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $A.a$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>            1            2 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $A.a$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>    0.7928932    2.2071068 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $A.a$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>    1.500000   -4.853102    7.853102 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $A.a$mean_pval
#>   p_value 
#> 0.2048328 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $A.a$median
#> median 
#>    1.5 
#> 
#> $A.a$mad
#> mad 
#>   0 
#> 
#> $A.a$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $A.a$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>           1.5            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $A.a$quantiles
#> quantile_0.25 quantile_0.75 
#>             1             2 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $A.a$iqr
#> iqr 
#>   1 
#> 
#> $A.a$range
#> min max 
#>   1   2 
#> 
#> $A.a$min
#> min 
#>   1 
#> 
#> $A.a$max
#> max 
#>   2 
#> 
#> $A.a$median_range
#> median    min    max 
#>    1.5    1.0    2.0 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $A.a$cv
#>       cv 
#> 47.14045 
#> 
#> $A.a$geom_mean
#> geom_mean 
#>  1.414214 
#> 
#> $A.a$geom_sd
#>  geom_sd 
#> 1.632527 
#> 
#> $A.a$geom_mean_sd
#> geom_mean   geom_sd 
#>  1.414214  1.632527 
#> 
#> $A.a$geom_mean_ci
#>  mean_ci_lwr  mean_ci_upr 
#>   0.01729978 115.60839614 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $A.a$geom_cv
#>  geom_cv 
#> 52.10922 
#> 
#> $A.a$geom_mean_ci_3d
#>    geom_mean  mean_ci_lwr  mean_ci_upr 
#>   1.41421356   0.01729978 115.60839614 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 
#> $B.a
#> $B.a$n
#> n 
#> 1 
#> 
#> $B.a$sum
#> sum 
#>   3 
#> 
#> $B.a$mean
#> mean 
#>    3 
#> 
#> $B.a$sd
#> sd 
#> NA 
#> 
#> $B.a$se
#> se 
#> NA 
#> 
#> $B.a$mean_sd
#> mean   sd 
#>    3   NA 
#> 
#> $B.a$mean_se
#> mean   se 
#>    3   NA 
#> 
#> $B.a$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $B.a$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $B.a$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $B.a$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>           3          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $B.a$mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $B.a$median
#> median 
#>      3 
#> 
#> $B.a$mad
#> mad 
#>   0 
#> 
#> $B.a$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $B.a$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>             3            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $B.a$quantiles
#> quantile_0.25 quantile_0.75 
#>             3             3 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $B.a$iqr
#> iqr 
#>   0 
#> 
#> $B.a$range
#> min max 
#>   3   3 
#> 
#> $B.a$min
#> min 
#>   3 
#> 
#> $B.a$max
#> max 
#>   3 
#> 
#> $B.a$median_range
#> median    min    max 
#>      3      3      3 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $B.a$cv
#> cv 
#> NA 
#> 
#> $B.a$geom_mean
#> geom_mean 
#>         3 
#> 
#> $B.a$geom_sd
#> geom_sd 
#>      NA 
#> 
#> $B.a$geom_mean_sd
#> geom_mean   geom_sd 
#>         3        NA 
#> 
#> $B.a$geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $B.a$geom_cv
#> geom_cv 
#>      NA 
#> 
#> $B.a$geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>           3          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 
#> $C.a
#> $C.a$n
#> n 
#> 0 
#> 
#> $C.a$sum
#> sum 
#>  NA 
#> 
#> $C.a$mean
#> mean 
#>   NA 
#> 
#> $C.a$sd
#> sd 
#> NA 
#> 
#> $C.a$se
#> se 
#> NA 
#> 
#> $C.a$mean_sd
#> mean   sd 
#>   NA   NA 
#> 
#> $C.a$mean_se
#> mean   se 
#>   NA   NA 
#> 
#> $C.a$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $C.a$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $C.a$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $C.a$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $C.a$mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $C.a$median
#> median 
#>     NA 
#> 
#> $C.a$mad
#> mad 
#>  NA 
#> 
#> $C.a$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $C.a$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>            NA            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $C.a$quantiles
#> quantile_0.25 quantile_0.75 
#>            NA            NA 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $C.a$iqr
#> iqr 
#>  NA 
#> 
#> $C.a$range
#> min max 
#>  NA  NA 
#> 
#> $C.a$min
#> min 
#>  NA 
#> 
#> $C.a$max
#> max 
#>  NA 
#> 
#> $C.a$median_range
#> median    min    max 
#>     NA     NA     NA 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $C.a$cv
#> cv 
#> NA 
#> 
#> $C.a$geom_mean
#> geom_mean 
#>       NaN 
#> 
#> $C.a$geom_sd
#> geom_sd 
#>      NA 
#> 
#> $C.a$geom_mean_sd
#> geom_mean   geom_sd 
#>       NaN        NA 
#> 
#> $C.a$geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $C.a$geom_cv
#> geom_cv 
#>      NA 
#> 
#> $C.a$geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>         NaN          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 
#> $A.b
#> $A.b$n
#> n 
#> 0 
#> 
#> $A.b$sum
#> sum 
#>  NA 
#> 
#> $A.b$mean
#> mean 
#>   NA 
#> 
#> $A.b$sd
#> sd 
#> NA 
#> 
#> $A.b$se
#> se 
#> NA 
#> 
#> $A.b$mean_sd
#> mean   sd 
#>   NA   NA 
#> 
#> $A.b$mean_se
#> mean   se 
#>   NA   NA 
#> 
#> $A.b$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $A.b$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $A.b$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $A.b$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>          NA          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $A.b$mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $A.b$median
#> median 
#>     NA 
#> 
#> $A.b$mad
#> mad 
#>  NA 
#> 
#> $A.b$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $A.b$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>            NA            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $A.b$quantiles
#> quantile_0.25 quantile_0.75 
#>            NA            NA 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $A.b$iqr
#> iqr 
#>  NA 
#> 
#> $A.b$range
#> min max 
#>  NA  NA 
#> 
#> $A.b$min
#> min 
#>  NA 
#> 
#> $A.b$max
#> max 
#>  NA 
#> 
#> $A.b$median_range
#> median    min    max 
#>     NA     NA     NA 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $A.b$cv
#> cv 
#> NA 
#> 
#> $A.b$geom_mean
#> geom_mean 
#>       NaN 
#> 
#> $A.b$geom_sd
#> geom_sd 
#>      NA 
#> 
#> $A.b$geom_mean_sd
#> geom_mean   geom_sd 
#>       NaN        NA 
#> 
#> $A.b$geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $A.b$geom_cv
#> geom_cv 
#>      NA 
#> 
#> $A.b$geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>         NaN          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 
#> $B.b
#> $B.b$n
#> n 
#> 1 
#> 
#> $B.b$sum
#> sum 
#>   4 
#> 
#> $B.b$mean
#> mean 
#>    4 
#> 
#> $B.b$sd
#> sd 
#> NA 
#> 
#> $B.b$se
#> se 
#> NA 
#> 
#> $B.b$mean_sd
#> mean   sd 
#>    4   NA 
#> 
#> $B.b$mean_se
#> mean   se 
#>    4   NA 
#> 
#> $B.b$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $B.b$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $B.b$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>           NA           NA 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $B.b$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>           4          NA          NA 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $B.b$mean_pval
#> p_value 
#>      NA 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $B.b$median
#> median 
#>      4 
#> 
#> $B.b$mad
#> mad 
#>   0 
#> 
#> $B.b$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $B.b$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>             4            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $B.b$quantiles
#> quantile_0.25 quantile_0.75 
#>             4             4 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $B.b$iqr
#> iqr 
#>   0 
#> 
#> $B.b$range
#> min max 
#>   4   4 
#> 
#> $B.b$min
#> min 
#>   4 
#> 
#> $B.b$max
#> max 
#>   4 
#> 
#> $B.b$median_range
#> median    min    max 
#>      4      4      4 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $B.b$cv
#> cv 
#> NA 
#> 
#> $B.b$geom_mean
#> geom_mean 
#>         4 
#> 
#> $B.b$geom_sd
#> geom_sd 
#>      NA 
#> 
#> $B.b$geom_mean_sd
#> geom_mean   geom_sd 
#>         4        NA 
#> 
#> $B.b$geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $B.b$geom_cv
#> geom_cv 
#>      NA 
#> 
#> $B.b$geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>           4          NA          NA 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 
#> $C.b
#> $C.b$n
#> n 
#> 2 
#> 
#> $C.b$sum
#> sum 
#>  11 
#> 
#> $C.b$mean
#> mean 
#>  5.5 
#> 
#> $C.b$sd
#>        sd 
#> 0.7071068 
#> 
#> $C.b$se
#>  se 
#> 0.5 
#> 
#> $C.b$mean_sd
#>      mean        sd 
#> 5.5000000 0.7071068 
#> 
#> $C.b$mean_se
#> mean   se 
#>  5.5  0.5 
#> 
#> $C.b$mean_ci
#> mean_ci_lwr mean_ci_upr 
#>  -0.8531024  11.8531024 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $C.b$mean_sei
#> mean_sei_lwr mean_sei_upr 
#>            5            6 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $C.b$mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>     4.792893     6.207107 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $C.b$mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>   5.5000000  -0.8531024  11.8531024 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $C.b$mean_pval
#>    p_value 
#> 0.05771588 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $C.b$median
#> median 
#>    5.5 
#> 
#> $C.b$mad
#> mad 
#>   0 
#> 
#> $C.b$median_ci
#> median_ci_lwr median_ci_upr 
#>            NA            NA 
#> attr(,"conf_level")
#> [1] NA
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $C.b$median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>           5.5            NA            NA 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $C.b$quantiles
#> quantile_0.25 quantile_0.75 
#>             5             6 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $C.b$iqr
#> iqr 
#>   1 
#> 
#> $C.b$range
#> min max 
#>   5   6 
#> 
#> $C.b$min
#> min 
#>   5 
#> 
#> $C.b$max
#> max 
#>   6 
#> 
#> $C.b$median_range
#> median    min    max 
#>    5.5    5.0    6.0 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $C.b$cv
#>       cv 
#> 12.85649 
#> 
#> $C.b$geom_mean
#> geom_mean 
#>  5.477226 
#> 
#> $C.b$geom_sd
#> geom_sd 
#>  1.1376 
#> 
#> $C.b$geom_mean_sd
#> geom_mean   geom_sd 
#>  5.477226  1.137600 
#> 
#> $C.b$geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    1.719943   17.442438 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $C.b$geom_cv
#>  geom_cv 
#> 12.94584 
#> 
#> $C.b$geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>    5.477226    1.719943   17.442438 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> 

# `s_summary.factor`

## Basic usage:
s_summary(factor(c("a", "a", "b", "c", "a")))
#> $n
#> $n$n
#> n 
#> 5 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     3 
#> 
#> $count$b
#> count 
#>     1 
#> 
#> $count$c
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>   3.0   0.6 
#> 
#> $count_fraction$b
#> count     p 
#>   1.0   0.2 
#> 
#> $count_fraction$c
#> count     p 
#>   1.0   0.2 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>   3.0   0.6 
#> 
#> $count_fraction_fixed_dp$b
#> count     p 
#>   1.0   0.2 
#> 
#> $count_fraction_fixed_dp$c
#> count     p 
#>   1.0   0.2 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3     5 
#> 
#> $fraction$b
#>   num denom 
#>     1     5 
#> 
#> $fraction$c
#>   num denom 
#>     1     5 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 

# Empty factor returns zero-filled items.
s_summary(factor(levels = c("a", "b", "c")))
#> $n
#> $n$n
#> n 
#> 0 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     0 
#> 
#> $count$b
#> count 
#>     0 
#> 
#> $count$c
#> count 
#>     0 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>     0     0 
#> 
#> $count_fraction$b
#> count     p 
#>     0     0 
#> 
#> $count_fraction$c
#> count     p 
#>     0     0 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>     0     0 
#> 
#> $count_fraction_fixed_dp$b
#> count     p 
#>     0     0 
#> 
#> $count_fraction_fixed_dp$c
#> count     p 
#>     0     0 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     0     0 
#> 
#> $fraction$b
#>   num denom 
#>     0     0 
#> 
#> $fraction$c
#>   num denom 
#>     0     0 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 

## Management of NA values.
x <- factor(c(NA, "Female"))
x <- explicit_na(x)
s_summary(x, na_rm = TRUE)
#> $n
#> $n$n
#> n 
#> 2 
#> 
#> 
#> $count
#> $count$Female
#> count 
#>     1 
#> 
#> $count$<NA>
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$Female
#> count     p 
#>   1.0   0.5 
#> 
#> $count_fraction$<NA>
#> count     p 
#>   1.0   0.5 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$Female
#> count     p 
#>   1.0   0.5 
#> 
#> $count_fraction_fixed_dp$<NA>
#> count     p 
#>   1.0   0.5 
#> 
#> 
#> $fraction
#> $fraction$Female
#>   num denom 
#>     1     2 
#> 
#> $fraction$<NA>
#>   num denom 
#>     1     2 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 
s_summary(x, na_rm = FALSE)
#> $n
#> $n$n
#> n 
#> 2 
#> 
#> 
#> $count
#> $count$Female
#> count 
#>     1 
#> 
#> $count$`NA`
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$Female
#> count     p 
#>   1.0   0.5 
#> 
#> $count_fraction$`NA`
#> count     p 
#>   1.0   0.5 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$Female
#> count     p 
#>   1.0   0.5 
#> 
#> $count_fraction_fixed_dp$`NA`
#> count     p 
#>   1.0   0.5 
#> 
#> 
#> $fraction
#> $fraction$Female
#>   num denom 
#>     1     2 
#> 
#> $fraction$`NA`
#>   num denom 
#>     1     2 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 

## Different denominators.
x <- factor(c("a", "a", "b", "c", "a"))
s_summary(x, denom = "N_row", .N_row = 10L)
#> $n
#> $n$n
#> n 
#> 5 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     3 
#> 
#> $count$b
#> count 
#>     1 
#> 
#> $count$c
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>   3.0   0.3 
#> 
#> $count_fraction$b
#> count     p 
#>   1.0   0.1 
#> 
#> $count_fraction$c
#> count     p 
#>   1.0   0.1 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>   3.0   0.3 
#> 
#> $count_fraction_fixed_dp$b
#> count     p 
#>   1.0   0.1 
#> 
#> $count_fraction_fixed_dp$c
#> count     p 
#>   1.0   0.1 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3    10 
#> 
#> $fraction$b
#>   num denom 
#>     1    10 
#> 
#> $fraction$c
#>   num denom 
#>     1    10 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 
s_summary(x, denom = "N_col", .N_col = 20L)
#> $n
#> $n$n
#> n 
#> 5 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     3 
#> 
#> $count$b
#> count 
#>     1 
#> 
#> $count$c
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>  3.00  0.15 
#> 
#> $count_fraction$b
#> count     p 
#>  1.00  0.05 
#> 
#> $count_fraction$c
#> count     p 
#>  1.00  0.05 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>  3.00  0.15 
#> 
#> $count_fraction_fixed_dp$b
#> count     p 
#>  1.00  0.05 
#> 
#> $count_fraction_fixed_dp$c
#> count     p 
#>  1.00  0.05 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3    20 
#> 
#> $fraction$b
#>   num denom 
#>     1    20 
#> 
#> $fraction$c
#>   num denom 
#>     1    20 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 

# `s_summary.character`

## Basic usage:
s_summary(c("a", "a", "b", "c", "a"), verbose = FALSE)
#> $n
#> $n$n
#> n 
#> 5 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     3 
#> 
#> $count$b
#> count 
#>     1 
#> 
#> $count$c
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>   3.0   0.6 
#> 
#> $count_fraction$b
#> count     p 
#>   1.0   0.2 
#> 
#> $count_fraction$c
#> count     p 
#>   1.0   0.2 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>   3.0   0.6 
#> 
#> $count_fraction_fixed_dp$b
#> count     p 
#>   1.0   0.2 
#> 
#> $count_fraction_fixed_dp$c
#> count     p 
#>   1.0   0.2 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3     5 
#> 
#> $fraction$b
#>   num denom 
#>     1     5 
#> 
#> $fraction$c
#>   num denom 
#>     1     5 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 
s_summary(c("a", "a", "b", "c", "a", ""), .var = "x", na_rm = FALSE, verbose = FALSE)
#> $n
#> $n$n
#> n 
#> 6 
#> 
#> 
#> $count
#> $count$a
#> count 
#>     3 
#> 
#> $count$b
#> count 
#>     1 
#> 
#> $count$c
#> count 
#>     1 
#> 
#> $count$`NA`
#> count 
#>     1 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#> count     p 
#>   3.0   0.5 
#> 
#> $count_fraction$b
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> $count_fraction$c
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> $count_fraction$`NA`
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#> count     p 
#>   3.0   0.5 
#> 
#> $count_fraction_fixed_dp$b
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> $count_fraction_fixed_dp$c
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> $count_fraction_fixed_dp$`NA`
#>     count         p 
#> 1.0000000 0.1666667 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3     6 
#> 
#> $fraction$b
#>   num denom 
#>     1     6 
#> 
#> $fraction$c
#>   num denom 
#>     1     6 
#> 
#> $fraction$`NA`
#>   num denom 
#>     1     6 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 

# `s_summary.logical`

## Basic usage:
s_summary(c(TRUE, FALSE, TRUE, TRUE))
#> $n
#> n 
#> 4 
#> 
#> $count
#> count 
#>     3 
#> 
#> $count_fraction
#>    count fraction 
#>     3.00     0.75 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>     3.00     0.75 
#> 
#> $fraction
#>   num denom 
#>     3     4 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

# Empty factor returns zero-filled items.
s_summary(as.logical(c()))
#> $n
#> n 
#> 0 
#> 
#> $count
#> count 
#>     0 
#> 
#> $count_fraction
#>    count fraction 
#>        0        0 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>        0        0 
#> 
#> $fraction
#>   num denom 
#>     0     0 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

## Management of NA values.
x <- c(NA, TRUE, FALSE)
s_summary(x, na_rm = TRUE)
#> $n
#> n 
#> 2 
#> 
#> $count
#> count 
#>     1 
#> 
#> $count_fraction
#>    count fraction 
#>      1.0      0.5 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>      1.0      0.5 
#> 
#> $fraction
#>   num denom 
#>     1     2 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 
s_summary(x, na_rm = FALSE)
#> $n
#> n 
#> 3 
#> 
#> $count
#> count 
#>     1 
#> 
#> $count_fraction
#>     count  fraction 
#> 1.0000000 0.3333333 
#> 
#> $count_fraction_fixed_dp
#>     count  fraction 
#> 1.0000000 0.3333333 
#> 
#> $fraction
#>   num denom 
#>     1     3 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

## Different denominators.
x <- c(TRUE, FALSE, TRUE, TRUE)
s_summary(x, denom = "N_row", .N_row = 10L)
#> $n
#> n 
#> 4 
#> 
#> $count
#> count 
#>     3 
#> 
#> $count_fraction
#>    count fraction 
#>      3.0      0.3 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>      3.0      0.3 
#> 
#> $fraction
#>   num denom 
#>     3    10 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 
s_summary(x, denom = "N_col", .N_col = 20L)
#> $n
#> n 
#> 4 
#> 
#> $count
#> count 
#>     3 
#> 
#> $count_fraction
#>    count fraction 
#>     3.00     0.15 
#> 
#> $count_fraction_fixed_dp
#>    count fraction 
#>     3.00     0.15 
#> 
#> $fraction
#>   num denom 
#>     3    20 
#> 
#> $n_blq
#> n_blq 
#>     0 
#> 

a_summary(factor(c("a", "a", "b", "c", "a")), .N_row = 10, .N_col = 10)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                     row_name formatted_cell indent_mod row_label
#> 1                          n              5          0         n
#> 2                    count.a              3          0         a
#> 3                    count.b              1          0         b
#> 4                    count.c              1          0         c
#> 5           count_fraction.a        3 (60%)          0         a
#> 6           count_fraction.b        1 (20%)          0         b
#> 7           count_fraction.c        1 (20%)          0         c
#> 8  count_fraction_fixed_dp.a      3 (60.0%)          0         a
#> 9  count_fraction_fixed_dp.b      1 (20.0%)          0         b
#> 10 count_fraction_fixed_dp.c      1 (20.0%)          0         c
#> 11                fraction.a    3/5 (60.0%)          0         a
#> 12                fraction.b    1/5 (20.0%)          0         b
#> 13                fraction.c    1/5 (20.0%)          0         c
#> 14                     n_blq              0          0     n_blq
a_summary(
  factor(c("a", "a", "b", "c", "a")),
  .ref_group = factor(c("a", "a", "b", "c")), compare_with_ref_group = TRUE, .in_ref_col = TRUE
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                     row_name formatted_cell indent_mod
#> 1                          n              5          0
#> 2                    count.a              3          0
#> 3                    count.b              1          0
#> 4                    count.c              1          0
#> 5           count_fraction.a        3 (60%)          0
#> 6           count_fraction.b        1 (20%)          0
#> 7           count_fraction.c        1 (20%)          0
#> 8  count_fraction_fixed_dp.a      3 (60.0%)          0
#> 9  count_fraction_fixed_dp.b      1 (20.0%)          0
#> 10 count_fraction_fixed_dp.c      1 (20.0%)          0
#> 11                fraction.a    3/5 (60.0%)          0
#> 12                fraction.b    1/5 (20.0%)          0
#> 13                fraction.c    1/5 (20.0%)          0
#> 14                     n_blq              0          0
#> 15               pval_counts                         0
#>                     row_label
#> 1                           n
#> 2                           a
#> 3                           b
#> 4                           c
#> 5                           a
#> 6                           b
#> 7                           c
#> 8                           a
#> 9                           b
#> 10                          c
#> 11                          a
#> 12                          b
#> 13                          c
#> 14                      n_blq
#> 15 p-value (chi-squared test)

a_summary(c("A", "B", "A", "C"), .var = "x", .N_col = 10, .N_row = 10, verbose = FALSE)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                     row_name formatted_cell indent_mod row_label
#> 1                          n              4          0         n
#> 2                    count.A              2          0         A
#> 3                    count.B              1          0         B
#> 4                    count.C              1          0         C
#> 5           count_fraction.A        2 (50%)          0         A
#> 6           count_fraction.B        1 (25%)          0         B
#> 7           count_fraction.C        1 (25%)          0         C
#> 8  count_fraction_fixed_dp.A      2 (50.0%)          0         A
#> 9  count_fraction_fixed_dp.B      1 (25.0%)          0         B
#> 10 count_fraction_fixed_dp.C      1 (25.0%)          0         C
#> 11                fraction.A    2/4 (50.0%)          0         A
#> 12                fraction.B    1/4 (25.0%)          0         B
#> 13                fraction.C    1/4 (25.0%)          0         C
#> 14                     n_blq              0          0     n_blq
a_summary(
  c("A", "B", "A", "C"),
  .ref_group = c("B", "A", "C"), .var = "x", compare_with_ref_group = TRUE, verbose = FALSE,
  .in_ref_col = FALSE
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                     row_name formatted_cell indent_mod
#> 1                          n              4          0
#> 2                    count.A              2          0
#> 3                    count.B              1          0
#> 4                    count.C              1          0
#> 5           count_fraction.A        2 (50%)          0
#> 6           count_fraction.B        1 (25%)          0
#> 7           count_fraction.C        1 (25%)          0
#> 8  count_fraction_fixed_dp.A      2 (50.0%)          0
#> 9  count_fraction_fixed_dp.B      1 (25.0%)          0
#> 10 count_fraction_fixed_dp.C      1 (25.0%)          0
#> 11                fraction.A    2/4 (50.0%)          0
#> 12                fraction.B    1/4 (25.0%)          0
#> 13                fraction.C    1/4 (25.0%)          0
#> 14                     n_blq              0          0
#> 15               pval_counts         0.9074          0
#>                     row_label
#> 1                           n
#> 2                           A
#> 3                           B
#> 4                           C
#> 5                           A
#> 6                           B
#> 7                           C
#> 8                           A
#> 9                           B
#> 10                          C
#> 11                          A
#> 12                          B
#> 13                          C
#> 14                      n_blq
#> 15 p-value (chi-squared test)

a_summary(c(TRUE, FALSE, FALSE, TRUE, TRUE), .N_row = 10, .N_col = 10)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                  row_name formatted_cell indent_mod               row_label
#> 1                       n              5          0                       n
#> 2                   count              3          0                   count
#> 3          count_fraction        3 (60%)          0          count_fraction
#> 4 count_fraction_fixed_dp      3 (60.0%)          0 count_fraction_fixed_dp
#> 5                fraction    3/5 (60.0%)          0                fraction
#> 6                   n_blq              0          0                   n_blq
a_summary(
  c(TRUE, FALSE, FALSE, TRUE, TRUE),
  .ref_group = c(TRUE, FALSE), .in_ref_col = TRUE, compare_with_ref_group = TRUE,
  .in_ref_col = FALSE
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                  row_name formatted_cell indent_mod                  row_label
#> 1                       n              5          0                          n
#> 2                   count              3          0                      count
#> 3          count_fraction        3 (60%)          0             count_fraction
#> 4 count_fraction_fixed_dp      3 (60.0%)          0    count_fraction_fixed_dp
#> 5                fraction    3/5 (60.0%)          0                   fraction
#> 6                   n_blq              0          0                      n_blq
#> 7             pval_counts                         0 p-value (chi-squared test)

a_summary(rnorm(10), .N_col = 10, .N_row = 20, .var = "bla")
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>           row_name       formatted_cell indent_mod                   row_label
#> 1                n                   10          0                           n
#> 2              sum                 -1.2          0                         Sum
#> 3             mean                 -0.1          0                        Mean
#> 4               sd                  1.1          0                          SD
#> 5               se                  0.4          0                          SE
#> 6          mean_sd           -0.1 (1.1)          0                   Mean (SD)
#> 7          mean_se           -0.1 (0.4)          0                   Mean (SE)
#> 8          mean_ci        (-0.94, 0.71)          0                 Mean 95% CI
#> 9         mean_sei        (-0.48, 0.25)          0               Mean -/+ 1xSE
#> 10        mean_sdi        (-1.26, 1.03)          0               Mean -/+ 1xSD
#> 11       mean_pval               0.7577          0 Mean p-value (H0: mean = 0)
#> 12          median                 -0.2          0                      Median
#> 13             mad                  0.0          0   Median Absolute Deviation
#> 14       median_ci        (-1.63, 0.63)          0               Median 95% CI
#> 15       quantiles           -0.6 - 0.5          0             25% and 75%-ile
#> 16             iqr                  1.1          0                         IQR
#> 17           range           -1.9 - 2.1          0                   Min - Max
#> 18             min                 -1.9          0                     Minimum
#> 19             max                  2.1          0                     Maximum
#> 20    median_range    -0.2 (-1.9 - 2.1)          0          Median (Min - Max)
#> 21              cv               -994.1          0                      CV (%)
#> 22       geom_mean                   NA          0              Geometric Mean
#> 23         geom_sd                   NA          0                Geometric SD
#> 24    geom_mean_sd                   NA          0         Geometric Mean (SD)
#> 25    geom_mean_ci                   NA          0       Geometric Mean 95% CI
#> 26         geom_cv                   NA          0         CV % Geometric Mean
#> 27    median_ci_3d -0.17 (-1.63 - 0.63)          0             Median (95% CI)
#> 28      mean_ci_3d -0.12 (-0.94 - 0.71)          0               Mean (95% CI)
#> 29 geom_mean_ci_3d                   NA          0     Geometric Mean (95% CI)
a_summary(rnorm(10, 5, 1),
  .ref_group = rnorm(20, -5, 1), .var = "bla", compare_with_ref_group = TRUE,
  .in_ref_col = FALSE
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>           row_name     formatted_cell indent_mod                   row_label
#> 1                n                 10          0                           n
#> 2              sum               54.1          0                         Sum
#> 3             mean                5.4          0                        Mean
#> 4               sd                0.9          0                          SD
#> 5               se                0.3          0                          SE
#> 6          mean_sd          5.4 (0.9)          0                   Mean (SD)
#> 7          mean_se          5.4 (0.3)          0                   Mean (SE)
#> 8          mean_ci       (4.74, 6.09)          0                 Mean 95% CI
#> 9         mean_sei       (5.11, 5.71)          0               Mean -/+ 1xSE
#> 10        mean_sdi       (4.47, 6.36)          0               Mean -/+ 1xSD
#> 11       mean_pval            <0.0001          0 Mean p-value (H0: mean = 0)
#> 12          median                5.1          0                      Median
#> 13             mad               -0.0          0   Median Absolute Deviation
#> 14       median_ci       (4.75, 6.07)          0               Median 95% CI
#> 15       quantiles          5.0 - 5.6          0             25% and 75%-ile
#> 16             iqr                0.6          0                         IQR
#> 17           range          4.4 - 7.8          0                   Min - Max
#> 18             min                4.4          0                     Minimum
#> 19             max                7.8          0                     Maximum
#> 20    median_range    5.1 (4.4 - 7.8)          0          Median (Min - Max)
#> 21              cv               17.5          0                      CV (%)
#> 22       geom_mean                5.3          0              Geometric Mean
#> 23         geom_sd                1.2          0                Geometric SD
#> 24    geom_mean_sd          5.3 (1.2)          0         Geometric Mean (SD)
#> 25    geom_mean_ci       (4.78, 5.99)          0       Geometric Mean 95% CI
#> 26         geom_cv               15.9          0         CV % Geometric Mean
#> 27    median_ci_3d 5.09 (4.75 - 6.07)          0             Median (95% CI)
#> 28      mean_ci_3d 5.41 (4.74 - 6.09)          0               Mean (95% CI)
#> 29 geom_mean_ci_3d 5.35 (4.78 - 5.99)          0     Geometric Mean (95% CI)
#> 30            pval            <0.0001          0            p-value (t-test)
```
