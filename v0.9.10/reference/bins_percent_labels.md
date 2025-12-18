# Labels for bins in percent

This creates labels for quantile based bins in percent. This assumes the
right-closed intervals as produced by
[`cut_quantile_bins()`](https://insightsengineering.github.io/tern/reference/cut_quantile_bins.md).

## Usage

``` r
bins_percent_labels(probs, digits = 0)
```

## Arguments

- probs:

  (`numeric`)  
  the probabilities identifying the quantiles. This is a sorted vector
  of unique `proportion` values, i.e. between 0 and 1, where the
  boundaries 0 and 1 must not be included.

- digits:

  (`integer(1)`)  
  number of decimal places to round the percent numbers.

## Value

A `character` vector with labels in the format `[0%,20%]`, `(20%,50%]`,
etc.
