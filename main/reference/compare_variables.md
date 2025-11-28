# Compare variables between groups

**\[stable\]**

The analyze function `compare_vars()` creates a layout element to
summarize and compare one or more variables, using the S3 generic
function
[`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
to calculate a list of summary statistics. A list of all available
statistics for numeric variables can be viewed by running
`get_stats("analyze_vars_numeric", add_pval = TRUE)` and for non-numeric
variables by running
`get_stats("analyze_vars_counts", add_pval = TRUE)`. Use the `.stats`
parameter to specify the statistics to include in your output summary
table.

Prior to using this function in your table layout you must use
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
to create a column split on the variable to be used in comparisons, and
specify a reference group via the `ref_group` parameter. Comparisons can
be performed for each group (column) against the specified reference
group by including the p-value statistic.

## Usage

``` r
compare_vars(
  lyt,
  vars,
  var_labels = vars,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  na_rm = TRUE,
  show_labels = "default",
  table_names = vars,
  section_div = NA_character_,
  .stats = c("n", "mean_sd", "count_fraction", "pval"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_compare(x, ...)

# S3 method for class 'numeric'
s_compare(x, ...)

# S3 method for class 'factor'
s_compare(x, ...)

# S3 method for class 'character'
s_compare(x, ...)

# S3 method for class 'logical'
s_compare(x, ...)
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

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- ...:

  additional arguments passed to `s_compare()`, including:

  - `denom`: (`string`) choice of denominator. Options are
    `c("n", "N_col", "N_row")`. For factor variables, can only be `"n"`
    (number of values in this row and column intersection).

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

- .stats:

  (`character`)  
  statistics to select for the table.

  Options for numeric variables are:
  `'n', 'sum', 'mean', 'sd', 'se', 'mean_sd', 'mean_se', 'mean_ci', 'mean_sei', 'mean_sdi', 'mean_pval', 'median', 'mad', 'median_ci', 'quantiles', 'iqr', 'range', 'min', 'max', 'median_range', 'cv', 'geom_mean', 'geom_sd', 'geom_mean_sd', 'geom_mean_ci', 'geom_cv', 'median_ci_3d', 'mean_ci_3d', 'geom_mean_ci_3d', 'pval'`

  Options for non-numeric variables are:
  `'n', 'count', 'count_fraction', 'count_fraction_fixed_dp', 'fraction', 'n_blq', 'pval_counts'`

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

## Value

- `compare_vars()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_compare()` to the table layout.

&nbsp;

- `s_compare()` returns output of
  [`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
  and comparisons versus the reference group in the form of p-values.

## Functions

- `compare_vars()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_compare()`: S3 generic function to produce a comparison summary.

- `s_compare(numeric)`: Method for `numeric` class. This uses the
  standard t-test to calculate the p-value.

- `s_compare(factor)`: Method for `factor` class. This uses the
  chi-squared test to calculate the p-value.

- `s_compare(character)`: Method for `character` class. This makes an
  automatic conversion to `factor` (with a warning) and then forwards to
  the method for factors.

- `s_compare(logical)`: Method for `logical` class. A chi-squared test
  is used. If missing values are not removed, then they are counted as
  `FALSE`.

## Note

- For factor variables, `denom` for factor proportions can only be `n`
  since the purpose is to compare proportions between columns, therefore
  a row-based proportion would not make sense. Proportion based on
  `N_col` would be difficult since we use counts for the chi-squared
  test statistic, therefore missing values should be accounted for as
  explicit factor levels.

- If factor variables contain `NA`, these `NA` values are excluded by
  default. To include `NA` values set `na.rm = FALSE` and missing values
  will be displayed as an `NA` level. Alternatively, an explicit factor
  level can be defined for `NA` values during pre-processing via
  [`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md).

- For character variables, automatic conversion to factor does not
  guarantee that the table will be generated correctly. In particular
  for sparse tables this very likely can fail. Therefore it is always
  better to manually convert character variables to factors during
  pre-processing.

- For `compare_vars()`, the column split must define a reference group
  via `ref_group` so that the comparison is well defined.

## See also

[`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
which is used internally to compute a summary within `s_compare()`, and
[`a_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
which is used (with `compare = TRUE`) as the analysis function for
`compare_vars()`.

## Examples

``` r
# `compare_vars()` in `rtables` pipelines

## Default output within a `rtables` pipeline.
lyt <- basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM B") %>%
  compare_vars(c("AGE", "SEX"))
build_table(lyt, tern_ex_adsl)
#>                                  ARM A        ARM B        ARM C   
#> ———————————————————————————————————————————————————————————————————
#> AGE                                                                
#>   n                                69           73           58    
#>   Mean (SD)                    34.1 (6.8)   35.8 (7.1)   36.1 (7.4)
#>   p-value (t-test)               0.1446                    0.8212  
#> SEX                                                                
#>   n                                69           73           58    
#>   F                            38 (55.1%)   40 (54.8%)   32 (55.2%)
#>   M                            31 (44.9%)   33 (45.2%)   26 (44.8%)
#>   p-value (chi-squared test)     1.0000                    1.0000  

## Select and format statistics output.
lyt <- basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM C") %>%
  compare_vars(
    vars = "AGE",
    .stats = c("mean_sd", "pval"),
    .formats = c(mean_sd = "xx.x, xx.x"),
    .labels = c(mean_sd = "Mean, SD")
  )
build_table(lyt, df = tern_ex_adsl)
#>                      ARM A       ARM B       ARM C  
#> ————————————————————————————————————————————————————
#> Mean, SD           34.1, 6.8   35.8, 7.1   36.1, 7.4
#> p-value (t-test)    0.1176      0.8212              

# `s_compare.numeric`

## Usual case where both this and the reference group vector have more than 1 value.
s_compare(rnorm(10, 5, 1), .ref_group = rnorm(5, -5, 1), .in_ref_col = FALSE)
#> $n
#>  n 
#> 10 
#> 
#> $sum
#>      sum 
#> 51.54276 
#> 
#> $mean
#>     mean 
#> 5.154276 
#> 
#> $sd
#>        sd 
#> 0.7601901 
#> 
#> $se
#>        se 
#> 0.2403932 
#> 
#> $mean_sd
#>      mean        sd 
#> 5.1542760 0.7601901 
#> 
#> $mean_se
#>      mean        se 
#> 5.1542760 0.2403932 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    4.610469    5.698083 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>     4.913883     5.394669 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>     4.394086     5.914466 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>    5.154276    4.610469    5.698083 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#>      p_value 
#> 4.911465e-09 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#>   median 
#> 5.270756 
#> 
#> $mad
#> mad 
#>   0 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>      4.286272      5.988843 
#> attr(,"conf_level")
#> [1] 0.9785156
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>      5.270756      4.286272      5.988843 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>      4.613857      5.713109 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#>      iqr 
#> 1.099252 
#> 
#> $range
#>      min      max 
#> 3.890038 6.269690 
#> 
#> $min
#>      min 
#> 3.890038 
#> 
#> $max
#>     max 
#> 6.26969 
#> 
#> $median_range
#>   median      min      max 
#> 5.270756 3.890038 6.269690 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#>       cv 
#> 14.74873 
#> 
#> $geom_mean
#> geom_mean 
#>  5.102148 
#> 
#> $geom_sd
#>  geom_sd 
#> 1.163981 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>  5.102148  1.163981 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    4.576971    5.687584 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#>  geom_cv 
#> 15.27254 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>    5.102148    4.576971    5.687584 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> $pval
#> [1] 8.812242e-08
#> 

## If one group has not more than 1 value, then p-value is not calculated.
s_compare(rnorm(10, 5, 1), .ref_group = 1, .in_ref_col = FALSE)
#> $n
#>  n 
#> 10 
#> 
#> $sum
#>      sum 
#> 50.50699 
#> 
#> $mean
#>     mean 
#> 5.050699 
#> 
#> $sd
#>        sd 
#> 0.6005475 
#> 
#> $se
#>        se 
#> 0.1899098 
#> 
#> $mean_sd
#>      mean        sd 
#> 5.0506992 0.6005475 
#> 
#> $mean_se
#>      mean        se 
#> 5.0506992 0.1899098 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    4.621093    5.480305 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>     4.860789     5.240609 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>     4.450152     5.651247 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>    5.050699    4.621093    5.480305 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#>      p_value 
#> 7.263863e-10 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#>   median 
#> 5.282426 
#> 
#> $mad
#> mad 
#>   0 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>      4.824951      5.458242 
#> attr(,"conf_level")
#> [1] 0.9785156
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>      5.282426      4.824951      5.458242 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>      4.877169      5.428594 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#>       iqr 
#> 0.5514257 
#> 
#> $range
#>      min      max 
#> 3.527308 5.616535 
#> 
#> $min
#>      min 
#> 3.527308 
#> 
#> $max
#>      max 
#> 5.616535 
#> 
#> $median_range
#>   median      min      max 
#> 5.282426 3.527308 5.616535 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#>       cv 
#> 11.89038 
#> 
#> $geom_mean
#> geom_mean 
#>  5.013102 
#> 
#> $geom_sd
#>  geom_sd 
#> 1.143702 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>  5.013102  1.143702 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    4.553988    5.518502 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#>  geom_cv 
#> 13.48782 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>    5.013102    4.553988    5.518502 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> $pval
#> numeric(0)
#> 

## Empty numeric does not fail, it returns NA-filled items and no p-value.
s_compare(numeric(), .ref_group = numeric(), .in_ref_col = FALSE)
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
#> $pval
#> numeric(0)
#> 

# `s_compare.factor`

## Basic usage:
x <- factor(c("a", "a", "b", "c", "a"))
y <- factor(c("a", "b", "c"))
s_compare(x = x, .ref_group = y, .in_ref_col = FALSE)
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
#> $pval_counts
#> [1] 0.7659283
#> 

## Management of NA values.
x <- explicit_na(factor(c("a", "a", "b", "c", "a", NA, NA)))
y <- explicit_na(factor(c("a", "b", "c", NA)))
s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na_rm = TRUE)
#> $n
#> $n$n
#> n 
#> 7 
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
#> $count$<NA>
#> count 
#>     2 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#>     count         p 
#> 3.0000000 0.4285714 
#> 
#> $count_fraction$b
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction$c
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction$<NA>
#>     count         p 
#> 2.0000000 0.2857143 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#>     count         p 
#> 3.0000000 0.4285714 
#> 
#> $count_fraction_fixed_dp$b
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction_fixed_dp$c
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction_fixed_dp$<NA>
#>     count         p 
#> 2.0000000 0.2857143 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3     7 
#> 
#> $fraction$b
#>   num denom 
#>     1     7 
#> 
#> $fraction$c
#>   num denom 
#>     1     7 
#> 
#> $fraction$<NA>
#>   num denom 
#>     2     7 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 
#> $pval_counts
#> [1] 0.7659283
#> 
s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na_rm = FALSE)
#> $n
#> $n$n
#> n 
#> 7 
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
#>     2 
#> 
#> 
#> $count_fraction
#> $count_fraction$a
#>     count         p 
#> 3.0000000 0.4285714 
#> 
#> $count_fraction$b
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction$c
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction$`NA`
#>     count         p 
#> 2.0000000 0.2857143 
#> 
#> 
#> $count_fraction_fixed_dp
#> $count_fraction_fixed_dp$a
#>     count         p 
#> 3.0000000 0.4285714 
#> 
#> $count_fraction_fixed_dp$b
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction_fixed_dp$c
#>     count         p 
#> 1.0000000 0.1428571 
#> 
#> $count_fraction_fixed_dp$`NA`
#>     count         p 
#> 2.0000000 0.2857143 
#> 
#> 
#> $fraction
#> $fraction$a
#>   num denom 
#>     3     7 
#> 
#> $fraction$b
#>   num denom 
#>     1     7 
#> 
#> $fraction$c
#>   num denom 
#>     1     7 
#> 
#> $fraction$`NA`
#>   num denom 
#>     2     7 
#> 
#> 
#> $n_blq
#> $n_blq$n_blq
#> n_blq 
#>     0 
#> 
#> 
#> $pval_counts
#> [1] 0.9063036
#> 

# `s_compare.character`

## Basic usage:
x <- c("a", "a", "b", "c", "a")
y <- c("a", "b", "c")
s_compare(x, .ref_group = y, .in_ref_col = FALSE, .var = "x", verbose = FALSE)
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
#> $pval_counts
#> [1] 0.7659283
#> 

## Note that missing values handling can make a large difference:
x <- c("a", "a", "b", "c", "a", NA)
y <- c("a", "b", "c", rep(NA, 20))
s_compare(x,
  .ref_group = y, .in_ref_col = FALSE,
  .var = "x", verbose = FALSE
)
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
#> $count$`<Missing>`
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
#> $count_fraction$`<Missing>`
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
#> $count_fraction_fixed_dp$`<Missing>`
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
#> $fraction$`<Missing>`
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
#> $pval_counts
#> [1] 0.7659283
#> 
s_compare(x,
  .ref_group = y, .in_ref_col = FALSE, .var = "x",
  na.rm = FALSE, verbose = FALSE
)
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
#> $count$`<Missing>`
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
#> $count_fraction$`<Missing>`
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
#> $count_fraction_fixed_dp$`<Missing>`
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
#> $fraction$`<Missing>`
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
#> $pval_counts
#> [1] 0.7659283
#> 

# `s_compare.logical`

## Basic usage:
x <- c(TRUE, FALSE, TRUE, TRUE)
y <- c(FALSE, FALSE, TRUE)
s_compare(x, .ref_group = y, .in_ref_col = FALSE)
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
#> $pval_counts
#> [1] 0.2702894
#> 

## Management of NA values.
x <- c(NA, TRUE, FALSE)
y <- c(NA, NA, NA, NA, FALSE)
s_compare(x, .ref_group = y, .in_ref_col = FALSE, na_rm = TRUE)
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
#> $pval_counts
#> [1] 0.3864762
#> 
s_compare(x, .ref_group = y, .in_ref_col = FALSE, na_rm = FALSE)
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
#> $pval_counts
#> [1] 0.1675463
#> 
```
