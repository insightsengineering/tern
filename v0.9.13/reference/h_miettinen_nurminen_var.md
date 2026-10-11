# Variance Estimate Following Miettinen and Nurminen

**\[stable\]**

Calculates the Miettinen and Nurminen (1985) variance estimate for the
difference between two proportions. The estimate is based on the
constrained maximum likelihood estimates of the two proportions under
the specified risk difference and is used to obtain the standard error
for the Miettinen-Nurminen confidence interval.

## Usage

``` r
h_miettinen_nurminen_var(est1, est2, x1, x2, n1, n2)
```

## Arguments

- est1:

  (`numeric(1)`)\
  Estimated proportion for group 1. Used together with `est2` to define
  the risk difference. May be `NA_real_` when an estimate cannot be
  calculated.

- est2:

  (`numeric(1)`)\
  Estimated proportion for group 2. Used together with `est1` to define
  the risk difference. May be `NA_real_` when an estimate cannot be
  calculated.

- x1:

  (`numeric`)\
  Number of responders in group 1 for each stratum. Must have length at
  least 1.

- x2:

  (`numeric`)\
  Number of responders in group 2 for each stratum. Must have the same
  length as `x1`.

- n1:

  (`numeric`)\
  Number of observations in group 1 for each stratum. Must have the same
  length as `x1`.

- n2:

  (`numeric`)\
  Number of observations in group 2 for each stratum. Must have the same
  length as `x1`.

## Value

A named `list` with elements:

- `p1_est`: constrained maximum likelihood estimate of the proportion in
  group 1 for each stratum.

- `p2_est`: constrained maximum likelihood estimate of the proportion in
  group 2 for each stratum.

- `var_est`: Miettinen-Nurminen variance estimate for each stratum.

## Details

The risk difference is defined as `est2` - `est1`. For each stratum, the
function calculates the constrained maximum likelihood estimate for the
proportion in group 1 and obtains the corresponding estimate for group 2
by adding the risk difference. The variance is then calculated from
these estimates using the Miettinen-Nurminen variance formula.

The variance is returned as `NA_real_` for strata where the variance
cannot be calculated.

The variable names in this function follow the notation in the original
paper by Miettinen and Nurminen (1985) , cf. Appendix 1.

## References

Miettinen OS, Nurminen M (1985). “Comparative analysis of two rates.”
*Statistics in Medicine*, **4**(2), 213–226.
[doi:10.1002/sim.4780040211](https://doi.org/10.1002/sim.4780040211) .

## See also

[`prop_diff_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
[`h_prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_cmh.md),
[`h_miettinen_nurminen_stratified_ci()`](https://pharmaverse.github.io/tern/reference/h_miettinen_nurminen_stratified_ci.md)
