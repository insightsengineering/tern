# Stratified Miettinen-Nurminen Confidence Interval

**\[experimental\]**

Calculates the stratified Miettinen-Nurminen confidence interval and
standard error for the difference in proportions. The method uses
constrained maximum likelihood estimates within each stratum and
combines the stratum-specific variance estimates using the normalized
CMH weights.

## Usage

``` r
h_miettinen_nurminen_stratified_ci(prop, conf_level = 0.95)
```

## Arguments

- prop:

  (`list`)\
  A named list returned by
  [`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md).
  It must contain the following atomic vectors:

  `est1`

  : CMH-weighted estimated proportion for group 1. May be `NA_real_`
    when a CMH-weighted estimate cannot be calculated.

  `est2`

  : CMH-weighted estimated proportion for group 2. May be `NA_real_`
    when a CMH-weighted estimate cannot be calculated.

  `x1`

  : Number of responders in group 1 for each stratum.

  `x2`

  : Number of responders in group 2 for each stratum.

  `n1`

  : Number of observations in group 1 for each stratum.

  `n2`

  : Number of observations in group 2 for each stratum.

  `p1`

  : Observed response proportion in group 1 for each stratum.

  `p2`

  : Observed response proportion in group 2 for each stratum.

  `w`

  : Unnormalized CMH weight for each stratum.

  `w_normalized`

  : Normalized CMH weight for each stratum.

- conf_level:

  (`number(1)`)\
  Confidence level for the confidence interval.

## Value

A named list containing:

- `ci`:

  (`numeric(2)`) Lower and upper confidence limits for the stratified
  difference in proportions.

- `se`:

  (`numeric(1)`) Standard error of the stratified difference in
  proportions.

## Details

The difference in proportions is defined as the proportion in group 2
minus the proportion in group 1.

## See also

[`prop_diff_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
[`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md),
[`h_miettinen_nurminen_var()`](https://pharmaverse.github.io/tern/reference/h_miettinen_nurminen_var.md)
