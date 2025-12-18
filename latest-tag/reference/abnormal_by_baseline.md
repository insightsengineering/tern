# Count patients with abnormal analysis range values by baseline status

**\[stable\]**

The analyze function `count_abnormal_by_baseline()` creates a layout
element to count patients with abnormal analysis range values,
categorized by baseline status.

This function analyzes primary analysis variable `var` which indicates
abnormal range results. Additional analysis variables that can be
supplied as a list via the `variables` parameter are `id` (defaults to
`USUBJID`), a variable to indicate unique subject identifiers, and
`baseline` (defaults to `BNRIND`), a variable to indicate baseline
reference ranges.

For each direction specified via the `abnormal` parameter (e.g. High or
Low), we condition on baseline range result and count patients in the
numerator and denominator as follows for each of the following
categories:

- `Not <abnormality>`

  - `num`: The number of patients without abnormality at baseline
    (excluding those with missing baseline) and with at least one
    abnormality post-baseline.

  - `denom`: The number of patients without abnormality at baseline
    (excluding those with missing baseline).

- `<Abnormality>`

  - `num`: The number of patients with abnormality as baseline and at
    least one abnormality post-baseline.

  - `denom`: The number of patients with abnormality at baseline.

- `Total`

  - `num`: The number of patients with at least one post-baseline record
    and at least one abnormality post-baseline.

  - `denom`: The number of patients with at least one post-baseline
    record.

This function assumes that `df` has been filtered to only include
post-baseline records.

## Usage

``` r
count_abnormal_by_baseline(
  lyt,
  var,
  abnormal,
  variables = list(id = "USUBJID", baseline = "BNRIND"),
  na_str = "<Missing>",
  nested = TRUE,
  ...,
  table_names = abnormal,
  .stats = "fraction",
  .stat_names = NULL,
  .formats = list(fraction = format_fraction),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_abnormal_by_baseline(
  df,
  .var,
  abnormal,
  na_str = "<Missing>",
  variables = list(id = "USUBJID", baseline = "BNRIND"),
  ...
)

a_count_abnormal_by_baseline(
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

- abnormal:

  (`character`)  
  values identifying the abnormal range level(s) in `.var`.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- na_str:

  (`string`)  
  the explicit `na_level` argument you used in the pre-processing steps
  (maybe with
  [`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md)).
  The default is `"<Missing>"`.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- ...:

  additional arguments for the lower level functions.

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'fraction'`

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

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

## Value

- `count_abnormal_by_baseline()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_abnormal_by_baseline()` to the
  table layout.

&nbsp;

- `s_count_abnormal_by_baseline()` returns statistic `fraction` which is
  a named list with 3 labeled elements: `not_abnormal`, `abnormal`, and
  `total`. Each element contains a vector with `num` and `denom` patient
  counts.

&nbsp;

- `a_count_abnormal_by_baseline()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_abnormal_by_baseline()`: Layout-creating function which can
  take statistics function arguments and additional format arguments.
  This function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_abnormal_by_baseline()`: Statistics function for a single
  `abnormal` level.

- `a_count_abnormal_by_baseline()`: Formatted analysis function which is
  used as `afun` in `count_abnormal_by_baseline()`.

## Note

- `df` should be filtered to include only post-baseline records.

- If the baseline variable or analysis variable contains `NA` records,
  it is expected that `df` has been pre-processed using
  [`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md)
  or
  [`explicit_na()`](https://insightsengineering.github.io/tern/reference/explicit_na.md).

## See also

Relevant description function
[`d_count_abnormal_by_baseline()`](https://insightsengineering.github.io/tern/reference/d_count_abnormal_by_baseline.md).

## Examples

``` r
df <- data.frame(
  USUBJID = as.character(c(1:6)),
  ANRIND = factor(c(rep("LOW", 4), "NORMAL", "HIGH")),
  BNRIND = factor(c("LOW", "NORMAL", "HIGH", NA, "LOW", "NORMAL"))
)
df <- df_explicit_na(df)

# Layout creating function.
basic_table() %>%
  count_abnormal_by_baseline(var = "ANRIND", abnormal = c(High = "HIGH")) %>%
  build_table(df)
#>                all obs  
#> ————————————————————————
#> High                    
#>   Not high    1/4 (25%) 
#>   High           0/1    
#>   Total      1/6 (16.7%)

# Passing of statistics function and formatting arguments.
df2 <- data.frame(
  ID = as.character(c(1, 2, 3, 4)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
  BLRANGE = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
)

basic_table() %>%
  count_abnormal_by_baseline(
    var = "RANGE",
    abnormal = c(Low = "LOW"),
    variables = list(id = "ID", baseline = "BLRANGE"),
    .formats = c(fraction = "xx / xx"),
    .indent_mods = c(fraction = 2L)
  ) %>%
  build_table(df2)
#>                 all obs
#> ———————————————————————
#> Low                    
#>       Not low    1 / 3 
#>       Low        0 / 1 
#>       Total      1 / 4 
```
