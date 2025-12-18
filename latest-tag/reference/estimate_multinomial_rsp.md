# Estimate proportions of each level of a variable

**\[stable\]**

The analyze & summarize function `estimate_multinomial_response()`
creates a layout element to estimate the proportion and proportion
confidence interval for each level of a factor variable. The primary
analysis variable, `var`, should be a factor variable, the values of
which will be used as labels within the output table.

## Usage

``` r
estimate_multinomial_response(
  lyt,
  var,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  show_labels = "hidden",
  table_names = var,
  .stats = "prop_ci",
  .stat_names = NULL,
  .formats = list(prop_ci = "(xx.xx, xx.xx)"),
  .labels = NULL,
  .indent_mods = NULL
)

s_length_proportion(x, ..., .N_col)

a_length_proportion(
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

- var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

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

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

## Value

- `estimate_multinomial_response()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_length_proportion()` to the table
  layout.

&nbsp;

- `s_length_proportion()` returns statistics from
  [`s_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md).

&nbsp;

- `a_length_proportion()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `estimate_multinomial_response()`: Layout-creating function which can
  take statistics function arguments and additional format arguments.
  This function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
  and
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_length_proportion()`: Statistics function which feeds the length of
  `x` as number of successes, and `.N_col` as total number of successes
  and failures into
  [`s_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md).

- `a_length_proportion()`: Formatted analysis function which is used as
  `afun` in `estimate_multinomial_response()`.

## See also

Relevant description function
[`d_onco_rsp_label()`](https://insightsengineering.github.io/tern/reference/d_onco_rsp_label.md).

## Examples

``` r
library(dplyr)

# Use of the layout creating function.
dta_test <- data.frame(
  USUBJID = paste0("S", 1:12),
  ARM     = factor(rep(LETTERS[1:3], each = 4)),
  AVAL    = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
) %>% mutate(
  AVALC = factor(AVAL,
    levels = c(0, 1),
    labels = c("Complete Response (CR)", "Partial Response (PR)")
  )
)

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  estimate_multinomial_response(var = "AVALC")

tbl <- build_table(lyt, dta_test)

tbl
#>                                           A                B                 C       
#> —————————————————————————————————————————————————————————————————————————————————————
#> Complete Response (CR)                0 (0.0%)         2 (50.0%)        4 (100.0%)   
#>   95% CI (Wald, with correction)    (0.00, 12.50)    (0.00, 100.00)   (87.50, 100.00)
#> Partial Response (PR)                4 (100.0%)        2 (50.0%)         0 (0.0%)    
#>   95% CI (Wald, with correction)   (87.50, 100.00)   (0.00, 100.00)    (0.00, 12.50) 

s_length_proportion(rep("CR", 10), .N_col = 100)
#> $n_prop
#> [1] 10.0  0.1
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1]  3.620108 16.379892
#> attr(,"label")
#> [1] "95% CI (Wald, with correction)"
#> 
s_length_proportion(factor(character(0)), .N_col = 100)
#> $n_prop
#> [1] 0 0
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1] 0.0 0.5
#> attr(,"label")
#> [1] "95% CI (Wald, with correction)"
#> 

a_length_proportion(rep("CR", 10), .N_col = 100)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                         row_name formatted_cell indent_mod
#> 1                     Responders     10 (10.0%)          0
#> 2 95% CI (Wald, with correction)    (3.6, 16.4)          0
#>                        row_label
#> 1                     Responders
#> 2 95% CI (Wald, with correction)
a_length_proportion(factor(character(0)), .N_col = 100)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>                         row_name formatted_cell indent_mod
#> 1                     Responders       0 (0.0%)          0
#> 2 95% CI (Wald, with correction)     (0.0, 0.5)          0
#>                        row_label
#> 1                     Responders
#> 2 95% CI (Wald, with correction)
```
