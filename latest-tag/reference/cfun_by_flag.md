# Constructor for content functions given a data frame with flag input

This can be useful for tabulating model results.

## Usage

``` r
cfun_by_flag(analysis_var, flag_var, format = "xx", .indent_mods = NULL)
```

## Arguments

- analysis_var:

  (`string`)  
  variable name for the column containing values to be returned by the
  content function.

- flag_var:

  (`string`)  
  variable name for the logical column identifying which row should be
  returned.

- format:

  (`string`)  
  `rtables` format to use.

## Value

A content function which gives `df$analysis_var` at the row identified
by `.df_row$flag` in the given format.
