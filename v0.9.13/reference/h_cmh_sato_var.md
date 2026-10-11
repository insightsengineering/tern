# Sato Variance Estimate for the CMH-weighted Difference in Proportions

**\[stable\]**

Calculates the Sato variance estimate for the difference between two
Cochran-Mantel-Haenszel (CMH)-weighted proportions. The estimate is used
to obtain the standard error and confidence interval for the stratified
difference in response proportions.

The calculation follows the variance estimator proposed by Sato et al.
(1989) . The required stratum-specific counts, sample sizes, CMH
weights, and overall CMH-weighted proportion estimates are supplied in
the `prop` object returned by
[`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md).

## Usage

``` r
h_cmh_sato_var(prop)
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

  `w`

  : Unnormalized CMH weights for each stratum.

  The vectors `x1`, `x2`, `n1`, `n2`, and `w` must be of the same
  length.

  The unnormalized CMH weights for stratum \\i\\ given by \$\$
  \frac{n\_{1i} n\_{2i}}{n\_{1i} + n\_{2i}}, \$\$ for \\n\_{1i} +
  n\_{2i} \> 0\\, where \\n\_{1i}\\ is the total number of observations
  in group \\1\\ in stratum \\i\\, and \\n\_{2i}\\ is the total number
  of observations in group \\2\\ in stratum \\i\\.

  Missing weights in `w` are allowed and are ignored when calculating
  their sum.

## Value

A `numeric(1)` containing the Sato estimate of the variance of the
CMH-weighted difference in proportions. Returns `NA_real_` when the
variance cannot be estimated because there are no usable strata or the
sum of the supplied CMH weights is zero.

## Details

`h_cmh_sato_var()` takes a `prop` list as returned by
[`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md).
The `prop` object must contain vectors `est1`, `est2`, `x1`, `x2`, `n1`,
`n2`, and `w`, which provide the overall CMH-weighted estimates and the
stratum-specific quantities required for the Sato variance calculation.

## References

Sato T, Greenland S, Robins JM (1989). “On the variance estimator for
the Mantel-Haenszel Risk Difference.” *Biometrics*, **45**(4),
1323–1324. <http://www.jstor.org/stable/2531784>.

## See also

[`prop_diff_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
[`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md)
