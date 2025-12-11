# Variance Estimates in Strata following Miettinen and Nurminen

The variable names in this function follow the notation in the original
paper by Miettinen and Nurminen (1985) , cf. Appendix 1.

## Usage

``` r
h_miettinen_nurminen_var_est(n1, n2, x1, x2, diff_par)
```

## Arguments

- n1:

  (`numeric`)  
  sample sizes in group 1.

- n2:

  (`numeric`)  
  sample sizes in group 2.

- x1:

  (`numeric`)  
  number of responders in group 1.

- x2:

  (`numeric`)  
  number of responders in group 2.

- diff_par:

  (`numeric`)  
  assumed difference in true proportions (group 2 minus group 1).

## Value

A named `list` with elements:

- `p1_hat`: estimated proportion in group 1

- `p2_hat`: estimated proportion in group 2

- `var_est`: variance estimate of the difference in proportions

## References

Miettinen OS, Nurminen M (1985). “Comparative analysis of two rates.”
*Statistics in Medicine*, **4**(2), 213–226.
[doi:10.1002/sim.4780040211](https://doi.org/10.1002/sim.4780040211) .
