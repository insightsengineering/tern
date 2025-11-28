# Conversion of months to days

**\[stable\]**

Conversion of months to days. This is an approximative calculation
because it considers each month as having an average of 30.4375 days.

## Usage

``` r
month2day(x)
```

## Arguments

- x:

  (`numeric(1)`)  
  time in months.

## Value

A `numeric` vector with the time in days.

## Examples

``` r
x <- c(13.25, 8.15, 1, 2.834)
month2day(x)
#> [1] 403.29688 248.06563  30.43750  86.25988
```
