# Re-implemented `range()` default S3 method for numerical objects

**\[stable\]**

This function returns `c(NA, NA)` instead of `c(-Inf, Inf)` for
zero-length data without any warnings.

## Usage

``` r
range_noinf(x, na.rm = FALSE, finite = FALSE)
```

## Arguments

- x:

  (`numeric`)  
  a sequence of numbers for which the range is computed.

- na.rm:

  (`flag`)  
  flag indicating if `NA` should be omitted.

- finite:

  (`flag`)  
  flag indicating if non-finite elements should be removed.

## Value

A 2-element vector of class `numeric`.

## Examples

``` r
x <- rnorm(20, 1)
range_noinf(x, na.rm = TRUE)
#> [1] -0.6737692  2.7887837
range_noinf(rep(NA, 20), na.rm = TRUE)
#> [1] NA NA
range(rep(NA, 20), na.rm = TRUE)
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> [1]  Inf -Inf
```
