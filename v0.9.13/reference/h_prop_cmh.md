# Helper function to calculate the CMH-weighted proportions and their confidence intervals.

**\[stable\]**

## Usage

``` r
h_prop_cmh(tbl, conf_level = 0.95)
```

## Arguments

- tbl:

  (`array`)\
  A three-dimensional contingency table containing counts for each
  combination of group, response, and stratum, in that order. The first
  two dimensions must each have exactly two levels, and the second
  dimension (response) must have names `"TRUE"` and `"FALSE"`. At least
  one stratum must be present. Strata with all cell counts equal to zero
  are allowed. All cell values must be finite, non-missing integer
  counts.

- conf_level:

  (`number(1)`)\
  Confidence level for the confidence intervals.

## Value

A named list containing the CMH-weighted proportion estimates,
confidence intervals, and intermediate quantities. The stratum-specific
quantities `x1`, `n1`, `p1`, `x2`, `n2`, `p2`, `w`, and `w_normalized`
are vectors with a length equal to the number of strata in `tbl` and
retain the same stratum order. Some of these quantities may be `NA` for
strata where they are not defined. In particular, `p1` or `p2` is `NA`
when the corresponding group has no observations in that stratum.

`est1` and `est2` are the overall CMH-weighted proportion estimates for
the two groups, respectively. `est_both_groups` contains these two
estimates in group order. `ci_both_groups` contains the corresponding
confidence intervals in the same group order.

If no stratum contains observations in both groups, the CMH weights
cannot be normalized and the overall estimates and confidence intervals
are `NA`

## See also

[`prop_diff_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md)
