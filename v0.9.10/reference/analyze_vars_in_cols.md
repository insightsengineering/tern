# Analyze numeric variables in columns

**\[experimental\]**

The layout-creating function `analyze_vars_in_cols()` creates a layout
element to generate a column-wise analysis table.

This function sets the analysis methods as column labels and is a
wrapper for
[`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html).
It was designed principally for PK tables.

## Usage

``` r
analyze_vars_in_cols(
  lyt,
  vars,
  ...,
  .stats = c("n", "mean", "sd", "se", "cv", "geom_cv"),
  .labels = c(n = "n", mean = "Mean", sd = "SD", se = "SE", cv = "CV (%)", geom_cv =
    "CV % Geometric Mean"),
  row_labels = NULL,
  do_summarize_row_groups = FALSE,
  split_col_vars = TRUE,
  imp_rule = NULL,
  avalcat_var = "AVALCAT1",
  cache = FALSE,
  .indent_mods = NULL,
  na_str = default_na_str(),
  nested = TRUE,
  .formats = NULL,
  .aligns = NULL
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- row_labels:

  (`character`)  
  as this function works in columns space, usually `.labels` character
  vector applies on the column space. You can change the row labels by
  defining this parameter to a named character vector with names
  corresponding to the split values. It defaults to `NULL` and if it
  contains only one `string`, it will duplicate that as a row label.

- do_summarize_row_groups:

  (`flag`)  
  defaults to `FALSE` and applies the analysis to the current label
  rows. This is a wrapper of
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  and it can accept `labelstr` to define row labels. This behavior is
  not supported as we never need to overload row labels.

- split_col_vars:

  (`flag`)  
  defaults to `TRUE` and puts the analysis results onto the columns.
  This option allows you to add multiple instances of this functions,
  also in a nested fashion, without adding more splits. This split must
  happen only one time on a single layout.

- imp_rule:

  (`string` or `NULL`)  
  imputation rule setting. Defaults to `NULL` for no imputation rule.
  Can also be `"1/3"` to implement 1/3 imputation rule or `"1/2"` to
  implement 1/2 imputation rule. In order to use an imputation rule, the
  `avalcat_var` argument must be specified. See
  [`imputation_rule()`](https://insightsengineering.github.io/tern/reference/imputation_rule.md)
  for more details on imputation.

- avalcat_var:

  (`string`)  
  if `imp_rule` is not `NULL`, name of variable that indicates whether a
  row in the data corresponds to an analysis value in category `"BLQ"`,
  `"LTR"`, `"<PCLLOQ"`, or none of the above (defaults to `"AVALCAT1"`).
  Variable must be present in the data and should match the variable
  used to calculate the `n_blq` statistic (if included in `.stats`).

- cache:

  (`flag`)  
  whether to store computed values in a temporary caching environment.
  This will speed up calculations in large tables, but should be set to
  `FALSE` if the same `rtable` layout is used for multiple tables with
  different data. Defaults to `FALSE`.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- .aligns:

  (`character` or `NULL`)  
  alignment for table contents (not including labels). When `NULL`,
  `"center"` is applied. See
  [`formatters::list_valid_aligns()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  for a list of all currently supported alignments.

## Value

A layout object suitable for passing to further layouting functions, or
to
[`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
Adding this function to an `rtable` layout will summarize the given
variables, arrange the output in columns, and add it to the table
layout.

## Note

- This is an experimental implementation of
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  and
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html)
  that may be subjected to changes as `rtables` extends its support to
  more complex analysis pipelines in the column space. We encourage
  users to read the examples carefully and file issues for different use
  cases.

- In this function, `labelstr` behaves atypically. If `labelstr = NULL`
  (the default), row labels are assigned automatically as the split
  values if `do_summarize_row_groups = FALSE` (the default), and as the
  group label if `do_summarize_row_groups = TRUE`.

## See also

[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md),
[`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html).

## Examples

``` r
library(dplyr)

# Data preparation
adpp <- tern_ex_adpp %>% h_pkparam_sort()

lyt <- basic_table() %>%
  split_rows_by(var = "STRATA1", label_pos = "topleft") %>%
  split_rows_by(
    var = "SEX",
    label_pos = "topleft",
    child_labels = "hidden"
  ) %>% # Removes duplicated labels
  analyze_vars_in_cols(vars = "AGE")
result <- build_table(lyt = lyt, df = adpp)
result
#> STRATA1                                                         
#>   SEX      n    Mean    SD    SE    CV (%)   CV % Geometric Mean
#> ————————————————————————————————————————————————————————————————
#> A                                                               
#>   F       81    38.8   5.4    0.6    13.8           14.3        
#>   M       81    38.9   5.9    0.7    15.1           14.9        
#> B                                                               
#>   F       90    36.0   6.4    0.7    17.7           17.9        
#>   M       81    36.4   6.5    0.7    17.9           17.8        
#> C                                                               
#>   F       117   34.1   6.2    0.6    18.2           18.3        
#>   M       72    33.2   11.5   1.4    34.6           31.6        

# By selecting just some statistics and ad-hoc labels
lyt <- basic_table() %>%
  split_rows_by(var = "ARM", label_pos = "topleft") %>%
  split_rows_by(
    var = "SEX",
    label_pos = "topleft",
    child_labels = "hidden",
    split_fun = drop_split_levels
  ) %>%
  analyze_vars_in_cols(
    vars = "AGE",
    .stats = c("n", "cv", "geom_mean"),
    .labels = c(
      n = "aN",
      cv = "aCV",
      geom_mean = "aGeomMean"
    )
  )
result <- build_table(lyt = lyt, df = adpp)
result
#> ARM                                    
#>   SEX            aN    aCV    aGeomMean
#> ———————————————————————————————————————
#> A: Drug X                              
#> B: Placebo                             
#> C: Combination                         
#>   F              288   17.6     35.5   
#>   M              234   23.4     35.3   

# Changing row labels
lyt <- basic_table() %>%
  analyze_vars_in_cols(
    vars = "AGE",
    row_labels = "some custom label"
  )
result <- build_table(lyt, df = adpp)
result
#>                      n    Mean   SD    SE    CV (%)   CV % Geometric Mean
#> —————————————————————————————————————————————————————————————————————————
#> some custom label   522   36.1   7.4   0.3    20.4           20.6        

# Pharmacokinetic parameters
lyt <- basic_table() %>%
  split_rows_by(
    var = "TLG_DISPLAY",
    split_label = "PK Parameter",
    label_pos = "topleft",
    child_labels = "hidden"
  ) %>%
  analyze_vars_in_cols(
    vars = "AVAL"
  )
result <- build_table(lyt, df = adpp)
result
#> PK Parameter   n    Mean     SD    SE    CV (%)   CV % Geometric Mean
#> —————————————————————————————————————————————————————————————————————
#> Cmax           58   29.7    5.6    0.7    19.0           19.3        
#> AUCinf obs     58   207.5   34.9   4.6    16.8           17.3        
#> CL obs         58    5.1    1.0    0.1    20.6           22.7        
#> Ae             58    1.5    0.3    0.0    21.3           24.1        
#> Fe             58   15.7    3.6    0.5    22.7           24.0        
#> CLR            58    0.0    0.0    0.0    19.9           22.2        
#> Rmax           58    9.6    2.0    0.3    21.1           21.6        
#> Tonset         58    3.0    0.7    0.1    22.4           23.3        
#> RENALCLD       58    0.0    0.0    0.0    19.0           19.4        

# Multiple calls (summarize label and analyze underneath)
lyt <- basic_table() %>%
  split_rows_by(
    var = "TLG_DISPLAY",
    split_label = "PK Parameter",
    label_pos = "topleft"
  ) %>%
  analyze_vars_in_cols(
    vars = "AVAL",
    do_summarize_row_groups = TRUE # does a summarize level
  ) %>%
  split_rows_by("SEX",
    child_labels = "hidden",
    label_pos = "topleft"
  ) %>%
  analyze_vars_in_cols(
    vars = "AVAL",
    split_col_vars = FALSE # avoids re-splitting the columns
  )
result <- build_table(lyt, df = adpp)
result
#> PK Parameter                                                         
#>   SEX          n    Mean     SD    SE    CV (%)   CV % Geometric Mean
#> —————————————————————————————————————————————————————————————————————
#> Cmax           58   29.7    5.6    0.7    19.0           19.3        
#>   F            32   30.1    6.1    1.1    20.4           21.0        
#>   M            26   29.2    5.0    1.0    17.2           17.2        
#> AUCinf obs     58   207.5   34.9   4.6    16.8           17.3        
#>   F            32   209.5   43.3   7.7    20.7           21.6        
#>   M            26   205.0   20.9   4.1    10.2           10.4        
#> CL obs         58    5.1    1.0    0.1    20.6           22.7        
#>   F            32    5.1    1.1    0.2    20.8           23.2        
#>   M            26    5.1    1.1    0.2    20.7           22.5        
#> Ae             58    1.5    0.3    0.0    21.3           24.1        
#>   F            32    1.5    0.3    0.1    19.3           20.8        
#>   M            26    1.5    0.4    0.1    23.9           27.9        
#> Fe             58   15.7    3.6    0.5    22.7           24.0        
#>   F            32   16.3    3.4    0.6    20.9           22.3        
#>   M            26   15.0    3.7    0.7    24.7           25.6        
#> CLR            58    0.0    0.0    0.0    19.9           22.2        
#>   F            32    0.0    0.0    0.0    20.5           24.8        
#>   M            26    0.0    0.0    0.0    19.5           18.9        
#> Rmax           58    9.6    2.0    0.3    21.1           21.6        
#>   F            32    9.6    2.2    0.4    22.4           23.5        
#>   M            26    9.6    1.9    0.4    19.7           19.5        
#> Tonset         58    3.0    0.7    0.1    22.4           23.3        
#>   F            32    3.0    0.7    0.1    24.7           25.9        
#>   M            26    3.0    0.6    0.1    19.8           20.0        
#> RENALCLD       58    0.0    0.0    0.0    19.0           19.4        
#>   F            32    0.0    0.0    0.0    19.6           20.4        
#>   M            26    0.0    0.0    0.0    18.0           17.6        
```
