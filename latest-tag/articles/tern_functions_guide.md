# Understanding \`tern\` functions

## Understanding `tern` functions

Every function in the `tern` package is designed to have a certain
structure that can cooperate well with every user’s need, while
maintaining a consistent and predictable behavior. This document will
guide you through an example function in the package, explaining the
purpose of many of its building blocks and how they can be used.

As we recently worked on it we will consider
[`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md)
as an example. This function is used to calculate the change from a
baseline value for a given variable. A realistic example can be found in
[`LBT03`](https://insightsengineering.github.io/tlg-catalog/stable/tables/lab-results/lbt03.html)
from the TLG-catalog.

[`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md)
is the main function that is available to the user. You can find lists
of these functions in
[`?tern::analyze_functions`](https://insightsengineering.github.io/tern/reference/analyze_functions.md).
All of these are build around
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
function, which is the core analysis function in `rtables`. All these
wrapper functions call specific analysis functions (always written as
`a_*`) that are meant to handle the statistic functions (always written
as `s_*`) and format the results with the `rtables::in_row()` function.
We can summarize this structure as follows:

[`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md)
(1)-\>
[`a_change_from_baseline()`](https://insightsengineering.github.io/tern/reference/summarize_change.md)
(2)-\>
\[[`s_change_from_baseline()`](https://insightsengineering.github.io/tern/reference/summarize_change.md) +
`rtables::in_row()`\]

The main questions that may arise are:

1.  Handling of `NA`.
2.  Handling of formats.
3.  Additional statistics.

Data set and library loading.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tern)
#> Loading required package: rtables
#> Loading required package: formatters
#> 
#> Attaching package: 'formatters'
#> The following object is masked from 'package:base':
#> 
#>     %||%
#> Loading required package: magrittr
#> 
#> Attaching package: 'rtables'
#> The following object is masked from 'package:utils':
#> 
#>     str
#> Registered S3 method overwritten by 'tern':
#>   method   from 
#>   tidy.glm broom

## Fabricate dataset
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  AVISIT = rep(paste0("V", 1:3), 6),
  ARM = rep(LETTERS[1:3], rep(6, 3)),
  AVAL = c(9:1, rep(NA, 9))
) %>%
  mutate(ABLFLL = AVISIT == "V1") %>%
  group_by(USUBJID) %>%
  mutate(
    BLVAL = AVAL[ABLFLL],
    CHG = AVAL - BLVAL
  ) %>%
  ungroup()
```

Classic use of
[`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md).

``` r
fix_layout <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT")

# Dealing with NAs: na_rm = TRUE
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
  build_table(dta_test) %>%
  print()
#>                     A               B         C 
#> ————————————————————————————————————————————————
#> V1                                              
#>   n                 2               1         0 
#>   Mean (SD)    7.50 (2.12)      3.00 (NA)     NA
#>   Median          7.50            3.00        NA
#>   Min - Max    6.00 - 9.00     3.00 - 3.00    NA
#> V2                                              
#>   n                 2               1         0 
#>   Mean (SD)   -1.00 (0.00)     -1.00 (NA)     NA
#>   Median          -1.00           -1.00       NA
#>   Min - Max   -1.00 - -1.00   -1.00 - -1.00   NA
#> V3                                              
#>   n                 2               1         0 
#>   Mean (SD)   -2.00 (0.00)     -2.00 (NA)     NA
#>   Median          -2.00           -2.00       NA
#>   Min - Max   -2.00 - -2.00   -2.00 - -2.00   NA

# Dealing with NAs: na_rm = FALSE
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL"), na_rm = FALSE) %>%
  build_table(dta_test) %>%
  print()
#>                     A         B    C 
#> —————————————————————————————————————
#> V1                                   
#>   n                 2         2    2 
#>   Mean (SD)    7.50 (2.12)    NA   NA
#>   Median          7.50        NA   NA
#>   Min - Max    6.00 - 9.00    NA   NA
#> V2                                   
#>   n                 2         2    2 
#>   Mean (SD)   -1.00 (0.00)    NA   NA
#>   Median          -1.00       NA   NA
#>   Min - Max   -1.00 - -1.00   NA   NA
#> V3                                   
#>   n                 2         2    2 
#>   Mean (SD)   -2.00 (0.00)    NA   NA
#>   Median          -2.00       NA   NA
#>   Min - Max   -2.00 - -2.00   NA   NA

# changing the NA string (it is done on all levels)
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL"), na_str = "my_na") %>%
  build_table(dta_test) %>%
  print()
#>                     A               B           C  
#> ———————————————————————————————————————————————————
#> V1                                                 
#>   n                 2               1           0  
#>   Mean (SD)    7.50 (2.12)    3.00 (my_na)    my_na
#>   Median          7.50            3.00        my_na
#>   Min - Max    6.00 - 9.00     3.00 - 3.00    my_na
#> V2                                                 
#>   n                 2               1           0  
#>   Mean (SD)   -1.00 (0.00)    -1.00 (my_na)   my_na
#>   Median          -1.00           -1.00       my_na
#>   Min - Max   -1.00 - -1.00   -1.00 - -1.00   my_na
#> V3                                                 
#>   n                 2               1           0  
#>   Mean (SD)   -2.00 (0.00)    -2.00 (my_na)   my_na
#>   Median          -2.00           -2.00       my_na
#>   Min - Max   -2.00 - -2.00   -2.00 - -2.00   my_na
```

`.formats`, `.labels`, and `.indent_mods` depend on the names of
`.stats`. Here is how you can change the default formatting.

``` r
# changing n count format and label and indentation
fix_layout %>%
  summarize_change("CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "mean"), # reducing the number of stats for visual appreciation
    .formats = c(n = "xx.xx"),
    .labels = c(n = "NnNn"),
    .indent_mods = c(n = 5), na_str = "nA"
  ) %>%
  build_table(dta_test) %>%
  print()
#>                     A      B      C  
#> —————————————————————————————————————
#> V1                                   
#>             NnNn   2.00   1.00   0.00
#>   Mean             7.5    3.0     nA 
#> V2                                   
#>             NnNn   2.00   1.00   0.00
#>   Mean             -1.0   -1.0    nA 
#> V3                                   
#>             NnNn   2.00   1.00   0.00
#>   Mean             -2.0   -2.0    nA
```

What if I want something special for the format?

``` r
# changing n count format and label and indentation
fix_layout %>%
  summarize_change("CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "mean"), # reducing the number of stats for visual appreciation
    .formats = c(n = function(x, ...) as.character(x * 100))
  ) %>% # Note you need ...!!!
  build_table(dta_test) %>%
  print()
#>           A      B     C 
#> —————————————————————————
#> V1                       
#>   n      200    100    0 
#>   Mean   7.5    3.0    NA
#> V2                       
#>   n      200    100    0 
#>   Mean   -1.0   -1.0   NA
#> V3                       
#>   n      200    100    0 
#>   Mean   -2.0   -2.0   NA
```

Adding a custom statistic (and custom format):

``` r
# changing n count format and label and indentation
fix_layout %>%
  summarize_change(
    "CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "my_stat" = function(df, ...) {
      a <- mean(df$AVAL, na.rm = TRUE)
      b <- list(...)$.N_row # It has access at all `?rtables::additional_fun_params`
      a / b
    }),
    .formats = c("my_stat" = function(x, ...) sprintf("%.2f", x))
  ) %>%
  build_table(dta_test)
#>              A      B     C 
#> ————————————————————————————
#> V1                          
#>   n          2      1     0 
#>   my_stat   1.25   0.50   NA
#> V2                          
#>   n          2      1     0 
#>   my_stat   1.08   0.33   NA
#> V3                          
#>   n          2      1     0 
#>   my_stat   0.92   0.17   NA
```

## For Developers

In all of these layers there are specific parameters that need to be
available, and, while `rtables` has multiple way to handle formatting
and `NA` values, we had to decide how to correctly handle these and
additional extra arguments. We follow the following scheme:

Level 1:
[`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md):
all parameters without a starting dot `.*` are used or added to
`extra_args`. Specifically, here we solve `NA` values by using
`inclNAs = TRUE` always in
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).
This will keep `NA` values to the analysis function `a_*`. Please follow
the way `na_rm` is used in `summarize_change`, and you will see how to
retrieve it from `...` only when you need it. In this case, only at the
[`summary()`](https://rdrr.io/r/base/summary.html) level. `na_str`,
instead is set only on the top level (in the
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
call). We may want to be statistic-dependent in the future, but we still
need to think how to accomplish that. We add the
[`rtables::additional_fun_params`](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)
to the analysis function so to make them available as `...` in the next
level. Note that they all can be retrieved with `list(...)[["na_rm"]]`.

Level 2:
[`a_change_from_baseline()`](https://insightsengineering.github.io/tern/reference/summarize_change.md):
all parameters starting with a dot `.` are ideally used or transmitted
into lower functions from here. Mainly `.stats`, `.formats`, `.labels`,
and `.indent_mods` are used only at this level. We also bring forward
`extra_afun_params` to the `...` list for the statistical function.
Notice the handling for additional parameters in the
[`do.call()`](https://rdrr.io/r/base/do.call.html) function.

Level 3 and beyond: `s_*` functions. In this case `s_summary` is at the
end used and the result brought into the main `a_*` function.
