# Analysis function to calculate risk difference column values

In the risk difference column, this function uses the statistics
function associated with `afun` to calculates risk difference values
from arm X (reference group) and arm Y. These arms are specified when
configuring the risk difference column which is done using the
[`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
split function in the previous call to
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html).
For all other columns, applies `afun` as usual. This function utilizes
the
[`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
function to perform risk difference calculations.

## Usage

``` r
afun_riskdiff(
  df,
  labelstr = "",
  afun,
  ...,
  .stats = NULL,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- afun:

  (named `list`)  
  a named list containing one name-value pair where the name corresponds
  to the name of the statistics function that should be used in
  calculations and the value is the corresponding analysis function.

- ...:

  additional arguments for the lower level functions.

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

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

## Value

A list of formatted
[`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## See also

- [`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
  for details on risk difference calculation.

- Split function
  [`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
  which, when used as `split_fun` within
  [`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
  with `riskdiff` argument set to `TRUE` in subsequent analyze functions
  calls, adds a risk difference column to a table layout.
