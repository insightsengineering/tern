# Apply automatic formatting

Checks if any of the listed formats in `.formats` are `"auto"`, and
replaces `"auto"` with the correct implementation of `format_auto` for
the given statistics, data, and variable.

## Usage

``` r
apply_auto_formatting(.formats, x_stats, .df_row, .var)
```

## Arguments

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- x_stats:

  (named `list`)  
  a named list of statistics where each element corresponds to an
  element in `.formats`, with matching names.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.
