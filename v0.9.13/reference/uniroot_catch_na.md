# Find a root while returning NA for missing function values

**\[experimental\]**

A wrapper around
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) that returns
`NA_real_` when `f` is `NA` at either end of `interval`.

## Usage

``` r
uniroot_catch_na(f, interval, ...)
```

## Arguments

- f:

  (`function`)\
  function for which the root is sought.

- interval:

  (`numeric(2)`)\
  end points of the interval to be searched.

- ...:

  further arguments passed to
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

## Value

A numeric scalar containing the found root, or `NA_real_` if `f` returns
`NA` at either endpoint of `interval`.

## See also

[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html)
