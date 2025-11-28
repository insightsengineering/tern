# Summarize variables in columns

**\[stable\]**

The analyze function `summarize_colvars()` uses the statistics function
[`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
to analyze variables that are arranged in columns. The variables to
analyze should be specified in the table layout via column splits (see
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
and
[`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html))
prior to using `summarize_colvars()`.

The function is a minimal wrapper for
[`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html),
a function typically used to apply different analysis methods in rows
for each column variable. To use the analysis methods as column labels,
please refer to the
[`analyze_vars_in_cols()`](https://insightsengineering.github.io/tern/reference/analyze_vars_in_cols.md)
function.

## Usage

``` r
summarize_colvars(
  lyt,
  na_str = default_na_str(),
  ...,
  .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
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

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- ...:

  arguments passed to
  [`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).

- .stats:

  (`character`)  
  statistics to select for the table.

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

  (named `vector` of `integer`)  
  indent modifiers for the labels. Each element of the vector should be
  a name-value pair with name corresponding to a statistic specified in
  `.stats` and value the indentation for that statistic's row label.

## Value

A layout object suitable for passing to further layouting functions, or
to
[`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
Adding this function to an `rtable` layout will summarize the given
variables, arrange the output in columns, and add it to the table
layout.

## See also

[`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)
and
[`analyze_colvars_functions`](https://insightsengineering.github.io/tern/reference/analyze_colvars_functions.md).

## Examples

``` r
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  PARAMCD = rep("lab", 6 * 3),
  AVISIT = rep(paste0("V", 1:3), 6),
  ARM = rep(LETTERS[1:3], rep(6, 3)),
  AVAL = c(9:1, rep(NA, 9)),
  CHG = c(1:9, rep(NA, 9))
)

## Default output within a `rtables` pipeline.
basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  summarize_colvars() %>%
  build_table(dta_test)
#>                         A                             B                             C          
#>                 AVAL         CHG           AVAL               CHG           AVAL         CHG   
#> ———————————————————————————————————————————————————————————————————————————————————————————————
#> V1                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   7.5 (2.1)   2.5 (2.1)   3.0 (<Missing>)   7.0 (<Missing>)   <Missing>   <Missing>
#>   Median         7.5         2.5            3.0               7.0         <Missing>   <Missing>
#>   Min - Max   6.0 - 9.0   1.0 - 4.0      3.0 - 3.0         7.0 - 7.0      <Missing>   <Missing>
#> V2                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   6.5 (2.1)   3.5 (2.1)   2.0 (<Missing>)   8.0 (<Missing>)   <Missing>   <Missing>
#>   Median         6.5         3.5            2.0               8.0         <Missing>   <Missing>
#>   Min - Max   5.0 - 8.0   2.0 - 5.0      2.0 - 2.0         8.0 - 8.0      <Missing>   <Missing>
#> V3                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   5.5 (2.1)   4.5 (2.1)   1.0 (<Missing>)   9.0 (<Missing>)   <Missing>   <Missing>
#>   Median         5.5         4.5            1.0               9.0         <Missing>   <Missing>
#>   Min - Max   4.0 - 7.0   3.0 - 6.0      1.0 - 1.0         9.0 - 9.0      <Missing>   <Missing>

## Selection of statistics, formats and labels also work.
basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  summarize_colvars(
    .stats = c("n", "mean_sd"),
    .formats = c("mean_sd" = "xx.x, xx.x"),
    .labels = c(n = "n", mean_sd = "Mean, SD")
  ) %>%
  build_table(dta_test)
#>                       A                           B                            C          
#>                AVAL       CHG           AVAL             CHG           AVAL         CHG   
#> ——————————————————————————————————————————————————————————————————————————————————————————
#> V1                                                                                        
#>   n             2          2             1                1              0           0    
#>   Mean, SD   7.5, 2.1   2.5, 2.1   3.0, <Missing>   7.0, <Missing>   <Missing>   <Missing>
#> V2                                                                                        
#>   n             2          2             1                1              0           0    
#>   Mean, SD   6.5, 2.1   3.5, 2.1   2.0, <Missing>   8.0, <Missing>   <Missing>   <Missing>
#> V3                                                                                        
#>   n             2          2             1                1              0           0    
#>   Mean, SD   5.5, 2.1   4.5, 2.1   1.0, <Missing>   9.0, <Missing>   <Missing>   <Missing>

## Use arguments interpreted by `s_summary`.
basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  summarize_colvars(na.rm = FALSE) %>%
  build_table(dta_test)
#>                         A                             B                             C          
#>                 AVAL         CHG           AVAL               CHG           AVAL         CHG   
#> ———————————————————————————————————————————————————————————————————————————————————————————————
#> V1                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   7.5 (2.1)   2.5 (2.1)   3.0 (<Missing>)   7.0 (<Missing>)   <Missing>   <Missing>
#>   Median         7.5         2.5            3.0               7.0         <Missing>   <Missing>
#>   Min - Max   6.0 - 9.0   1.0 - 4.0      3.0 - 3.0         7.0 - 7.0      <Missing>   <Missing>
#> V2                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   6.5 (2.1)   3.5 (2.1)   2.0 (<Missing>)   8.0 (<Missing>)   <Missing>   <Missing>
#>   Median         6.5         3.5            2.0               8.0         <Missing>   <Missing>
#>   Min - Max   5.0 - 8.0   2.0 - 5.0      2.0 - 2.0         8.0 - 8.0      <Missing>   <Missing>
#> V3                                                                                             
#>   n               2           2              1                 1              0           0    
#>   Mean (SD)   5.5 (2.1)   4.5 (2.1)   1.0 (<Missing>)   9.0 (<Missing>)   <Missing>   <Missing>
#>   Median         5.5         4.5            1.0               9.0         <Missing>   <Missing>
#>   Min - Max   4.0 - 7.0   3.0 - 6.0      1.0 - 1.0         9.0 - 9.0      <Missing>   <Missing>
```
