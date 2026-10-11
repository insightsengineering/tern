# Check the Mantel-Fleiss Criterion

**\[experimental\]**

Checks the Mantel-Fleiss criterion for stratified 2 x 2 contingency
tables.

## Usage

``` r
mantel_fleiss_crit(tbl, include_value = FALSE, threshold = 5L)
```

## Arguments

- tbl:

  (`array`)\
  A three-dimensional contingency table containing the counts for each
  combination of group, response, and stratum. The first two dimensions
  must correspond to the two variables defining the 2 x 2 contingency
  table (group and response), in either order. The third dimension must
  correspond to the strata. The first two dimensions must each have
  exactly two levels. All cell values must be finite, non-missing
  integer counts.

- include_value:

  (`logical(1)`)\
  Whether to include the calculated Mantel-Fleiss statistic as an
  attribute of the result.

- threshold:

  (`numeric(1)`)\
  The minimum Mantel-Fleiss statistic required for the criterion to be
  considered satisfied.

## Value

A logical value indicating whether the Mantel-Fleiss criterion is
satisfied. If `include_value = TRUE`, the result also contains a value
attribute with the calculated Mantel-Fleiss statistic. If there are no
non-empty strata, the result is `NA` and the value attribute is
`NA_real_`.

## Details

The Mantel-Fleiss statistic is calculated as

\$\$ MF = \min\left( \[\sum_h m\_{11h} - \sum_h {(n\_{11h})}\_L\],\\
\[\sum_h {(n\_{11h})}\_U - \sum_h m\_{11h}\] \right), \$\$

where \\h\\ indexes the non-empty strata. For each stratum \\h\\, the
expected frequency of cell \\(1, 1)\\ in table \\h\\, under the
hypothesis of no association between group and response, is

\$\$ m\_{11h} = \frac{n\_{1.h} n\_{.1h}}{n_h}. \$\$

The lower and upper bounds for \\n\_{11h}\\, given the marginal totals,
are:

\$\$ {(n\_{11h})}\_L = \max(0, n\_{1.h} - n\_{.2h}), \$\$ \$\$
{(n\_{11h})}\_U = \min(n\_{.1h}, n\_{1.h}). \$\$

The Mantel-Fleiss criterion is satisfied when \\MF \ge\\ `threshold`. By
default, `threshold = 5`, corresponding to the criterion described by
Mantel and Fleiss (1980).

Strata with all cell counts equal to zero are excluded from the
calculation. If all strata contain zero observations, there are no
non-empty strata over which to calculate the Mantel-Fleiss statistic,
and the statistic is therefore undefined. In this case, the function
returns `NA`.

## References

Mantel, N., and Fleiss, J. L. (1980). Minimum Expected Cell Size
Requirements for the Mantel-Haenszel One-Degree-of-Freedom Chi-Square
Test and a Related Rapid Procedure. *American Journal of Epidemiology*,
112(1), 129–134.

## Examples

``` r
set.seed(123)
n <- 40

grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
strata1 <- factor(sample(c("A", "B"), n, replace = TRUE))
strata2 <- factor(sample(c("x", "y"), n, replace = TRUE))
strata <- interaction(strata1, strata2)

tbl <- table(grp, rsp, strata)
tbl
#> , , strata = A.x
#> 
#>          rsp
#> grp       FALSE TRUE
#>   Active      2    3
#>   Control     1    2
#> 
#> , , strata = B.x
#> 
#>          rsp
#> grp       FALSE TRUE
#>   Active      3    5
#>   Control     1    2
#> 
#> , , strata = A.y
#> 
#>          rsp
#> grp       FALSE TRUE
#>   Active      2    2
#>   Control     1    4
#> 
#> , , strata = B.y
#> 
#>          rsp
#> grp       FALSE TRUE
#>   Active      3    3
#>   Control     2    4
#> 

is_mf_satisfied <- mantel_fleiss_crit(tbl)
is_mf_satisfied
#> [1] TRUE
mantel_fleiss_crit(tbl, include_value = TRUE)
#> [1] TRUE
#> attr(,"value")
#> [1] 6.382576

# Examples of use.

if (is_mf_satisfied) {
  print("CMH")
  prop_diff_cmh(rsp, grp, strata)$prop
} else {
  print("Exact")
  prop_diff_uncond_exact(rsp, grp)$prop
}
#> [1] "CMH"
#>    Active   Control 
#> 0.5495986 0.6985984 

if (is_mf_satisfied) {
  print("CMH")
  prop_cmh(tbl)
} else {
  print("Exact")
  prop_fisher(table(grp, rsp))
}
#> [1] "CMH"
#> [1] 0.3730088
#> attr(,"z_stat")
#> [1] -0.8908515
```
