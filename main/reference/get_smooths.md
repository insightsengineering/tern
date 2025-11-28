# Smooth function with optional grouping

**\[stable\]**

This produces `loess` smoothed estimates of `y` with Student confidence
intervals.

## Usage

``` r
get_smooths(df, x, y, groups = NULL, level = 0.95)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- x:

  (`string`)  
  x column name.

- y:

  (`string`)  
  y column name.

- groups:

  (`character` or `NULL`)  
  vector with optional grouping variables names.

- level:

  (`proportion`)  
  level of confidence interval to use (0.95 by default).

## Value

A `data.frame` with original `x`, smoothed `y`, `ylow`, and `yhigh`, and
optional `groups` variables formatted as `factor` type.
