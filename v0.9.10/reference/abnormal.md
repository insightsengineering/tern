# Count patients with abnormal range values

**\[stable\]**

The analyze function `count_abnormal()` creates a layout element to
count patients with abnormal analysis range values in each direction.

This function analyzes primary analysis variable `var` which indicates
abnormal range results. Additional analysis variables that can be
supplied as a list via the `variables` parameter are `id` (defaults to
`USUBJID`), a variable to indicate unique subject identifiers, and
`baseline` (defaults to `BNRIND`), a variable to indicate baseline
reference ranges.

For each direction specified via the `abnormal` parameter (e.g. High or
Low), a fraction of patient counts is returned, with numerator and
denominator calculated as follows:

- `num`: The number of patients with this abnormality recorded while on
  treatment.

- `denom`: The total number of patients with at least one post-baseline
  assessment.

This function assumes that `df` has been filtered to only include
post-baseline records.

## Usage

``` r
count_abnormal(
  lyt,
  var,
  abnormal = list(Low = "LOW", High = "HIGH"),
  variables = list(id = "USUBJID", baseline = "BNRIND"),
  exclude_base_abn = FALSE,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  table_names = var,
  .stats = "fraction",
  .stat_names = NULL,
  .formats = list(fraction = format_fraction),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_abnormal(
  df,
  .var,
  abnormal = list(Low = "LOW", High = "HIGH"),
  variables = list(id = "USUBJID", baseline = "BNRIND"),
  exclude_base_abn = FALSE,
  ...
)

a_count_abnormal(
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

  (named `list`)  
  list identifying the abnormal range level(s) in `var`. Defaults to
  `list(Low = "LOW", High = "HIGH")` but you can also group different
  levels into the named list, for example,
  `abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH"))`.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- exclude_base_abn:

  (`flag`)  
  whether to exclude subjects with baseline abnormality from numerator
  and denominator.

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

- `count_abnormal()` returns a layout object suitable for passing to
  further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_abnormal()` to the table
  layout.

&nbsp;

- `s_count_abnormal()` returns the statistic `fraction` which is a
  vector with `num` and `denom` counts of patients.

&nbsp;

- `a_count_abnormal()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_abnormal()`: Layout-creating function which can take statistics
  function arguments and additional format arguments. This function is a
  wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_abnormal()`: Statistics function which counts patients with
  abnormal range values for a single `abnormal` level.

- `a_count_abnormal()`: Formatted analysis function which is used as
  `afun` in `count_abnormal()`.

## Note

- `count_abnormal()` only considers a single variable that contains
  multiple abnormal levels.

- `df` should be filtered to only include post-baseline records.

- The denominator includes patients that may have other abnormal levels
  at baseline, and patients missing baseline records. Patients with
  these abnormalities at baseline can be optionally excluded from
  numerator and denominator via the `exclude_base_abn` parameter.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

df <- data.frame(
  USUBJID = as.character(c(1, 1, 2, 2)),
  ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
  BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

# Select only post-baseline records.
df <- df %>%
  filter(ONTRTFL == "Y")

# Layout creating function.
basic_table() %>%
  count_abnormal(var = "ANRIND", abnormal = list(high = "HIGH", low = "LOW")) %>%
  build_table(df)
#>         all obs 
#> ————————————————
#> high   1/2 (50%)
#> low    1/2 (50%)

# Passing of statistics function and formatting arguments.
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

# Select only post-baseline records.
df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE")
  ) %>%
  build_table(df2)
#>         all obs 
#> ————————————————
#> low    1/2 (50%)
#> high   1/2 (50%)
```
