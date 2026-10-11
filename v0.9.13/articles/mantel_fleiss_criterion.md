# The Mantel-Fleiss Criterion

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tern`](https://pharmaverse.github.io/tern/)`)`\
`#> Loading required package: rtables`\
`#> Loading required package: formatters`\
`#> `\
`#> Attaching package: 'formatters'`\
`#> The following object is masked from 'package:base':`\
`#> `\
`#>     %||%`\
`#> `\
`#> Attaching package: 'rtables'`\
`#> The following object is masked from 'package:utils':`\
`#> `\
`#>     str`\
`#> Registered S3 method overwritten by 'tern':`\
`#>   method   from `\
`#>   tidy.glm broom`

## Introduction

When comparing a binary response between two groups while adjusting for
a stratification variable, the Cochran-Mantel-Haenszel (CMH) test is a
common choice. Like other large-sample procedures, the CMH test relies
on an asymptotic (chi-square) approximation, which can be unreliable
when the stratified $`2 \times 2`$ tables are sparse. In those
situations an exact method is preferable.

The **Mantel-Fleiss criterion** (Mantel and Fleiss 1980) is a simple,
quick check that tells you whether the sample is large enough for the
asymptotic CMH approximation to be trustworthy. The
[`mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)
function in `tern` evaluates this criterion for a stratified
$`2 \times 2`$ contingency table and returns whether it is satisfied.
You can use the result to decide, in a data driven way, whether to run
the CMH test or fall back to an exact procedure.

[`mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)
is a standalone utility: it does not perform any test itself. It is
meant to be used alongside the proportion functions in `tern` (such as
[`prop_diff_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
[`prop_cmh()`](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md),
[`prop_diff_uncond_exact()`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
and
[`prop_fisher()`](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md))
when writing custom analysis functions.

## The criterion

Consider a stratified $`2 \times 2`$ table where $`h`$ indexes the
strata. Within stratum $`h`$, write the cell and margin counts as

``` math
\begin{array}{c|cc|c}
& \text{Response} & \text{No response} & \text{Row total} \\
\hline
\text{Group 1} & n_{11h} & n_{12h} & n_{1 \cdot h} \\
\text{Group 2} & n_{21h} & n_{22h} & n_{2 \cdot h} \\
\hline
\text{Column total} & n_{\cdot 1 h} & n_{\cdot 2 h} & n_{h}
\end{array}
```

Under the hypothesis of no association between group and response, the
expected count in cell $`(1, 1)`$ of stratum $`h`$ is

``` math
m_{11h} = \frac{n_{1 \cdot h}\, n_{\,\cdot 1 h}}{n_{h}} .
```

Given the fixed margins, the observed count $`n_{11h}`$ can range
between the bounds

``` math
(n_{11h})_L = \max(0,\ n_{1 \cdot h} - n_{\, \cdot 2 h}), \qquad
(n_{11h})_U = \min(n_{\, \cdot 1 h},\ n_{1 \cdot h}) .
```

The Mantel-Fleiss statistic aggregates these quantities across the
non-empty strata:

``` math
MF = \min \left(
\left[ \sum_h m_{11h} - \sum_h (n_{11h})_L \right],\
\left[ \sum_h (n_{11h})_U - \sum_h m_{11h} \right]
\right) .
```

The criterion is considered **satisfied** when $`MF \ge`$`threshold`.
The default `threshold = 5` corresponds to the rule proposed by Mantel
and Fleiss (1980): when $`MF \ge 5`$, the asymptotic CMH approximation
is generally adequate.

## Basic usage

[`mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)
expects a three-dimensional contingency table (an `array`) whose first
two dimensions are the group and response (each with two levels, in
either order) and whose third dimension is the stratum.

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
`n`` ``<-`` ``80`\
\
`grp`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Active"``, ``"Control"``)``, ``n``, replace ``=`` ``TRUE``)``)`\
`rsp`` ``<-`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``TRUE``, ``FALSE``)``, ``n``, replace ``=`` ``TRUE``)`\
`strata1`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"A"``, ``"B"``)``, ``n``, replace ``=`` ``TRUE``)``)`\
`strata2`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``)``, ``n``, replace ``=`` ``TRUE``)``)`\
`strata`` ``<-`` `[`interaction`](https://rdrr.io/r/base/interaction.html)`(``strata1``, ``strata2``)`\
\
`tbl`` ``<-`` `[`table`](https://rdrr.io/r/base/table.html)`(``grp``, ``rsp``, ``strata``)`\
`tbl`\
`#> , , strata = A.x`\
`#> `\
`#>          rsp`\
`#> grp       FALSE TRUE`\
`#>   Active      7    8`\
`#>   Control     3    4`\
`#> `\
`#> , , strata = B.x`\
`#> `\
`#>          rsp`\
`#> grp       FALSE TRUE`\
`#>   Active     10    4`\
`#>   Control     7    3`\
`#> `\
`#> , , strata = A.y`\
`#> `\
`#>          rsp`\
`#> grp       FALSE TRUE`\
`#>   Active      5    2`\
`#>   Control     2    3`\
`#> `\
`#> , , strata = B.y`\
`#> `\
`#>          rsp`\
`#> grp       FALSE TRUE`\
`#>   Active      5    7`\
`#>   Control     5    5`

Passing the table to
[`mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)
returns a single logical value:

\
[`mantel_fleiss_crit`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)`(``tbl``)`\
`#> [1] TRUE`

To see the underlying value of the `MF` statistic, set
`include_value = TRUE`. The Mantel-Fleiss value is then attached to the
result as a `"value"` attribute:

\
[`mantel_fleiss_crit`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)`(``tbl``, include_value ``=`` ``TRUE``)`\
`#> [1] TRUE`\
`#> attr(,"value")`\
`#> [1] 14.27273`

The `threshold` argument controls how large the statistic must be for
the criterion to hold. Raising it makes the criterion more conservative:

\
[`mantel_fleiss_crit`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)`(``tbl``, threshold ``=`` ``15``, include_value ``=`` ``TRUE``)`\
`#> [1] FALSE`\
`#> attr(,"value")`\
`#> [1] 14.27273`

## Choosing a test based on the criterion

The typical use case is to branch between an asymptotic and an exact
method depending on whether the criterion is satisfied. The example
below estimates the stratified difference in proportions with the CMH
method when the criterion holds, and with the unconditional exact method
otherwise:

\
`is_mf_satisfied`` ``<-`` `[`mantel_fleiss_crit`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)`(``tbl``)`\
\
`if`` ``(``is_mf_satisfied``)`` ``{`\
`  ``# Large enough sample: use the asymptotic CMH estimate.`\
`  `[`prop_diff_cmh`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md)`(``rsp``, ``grp``, ``strata``)``$``diff`\
`}`` ``else`` ``{`\
`  ``# Sparse data: fall back to the exact (unstratified) method.`\
`  `[`prop_diff_uncond_exact`](https://pharmaverse.github.io/tern/reference/h_prop_diff.md)`(``rsp``, ``grp``)``$``diff`\
`}`\
`#> [1] 0.03832335`

The same idea can be used to select a test statistic. Here the CMH test
is used when the criterion holds, and Fisher’s exact test on the
collapsed table otherwise:

\
`if`` ``(``is_mf_satisfied``)`` ``{`\
`  `[`prop_cmh`](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md)`(``tbl``)`\
`}`` ``else`` ``{`\
`  `[`prop_fisher`](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md)`(`[`table`](https://rdrr.io/r/base/table.html)`(``grp``, ``rsp``)``)`\
`}`\
`#> [1] 0.7369323`\
`#> attr(,"z_stat")`\
`#> [1] -0.3359186`

## Empty strata

Strata that contain no observations carry no information and are dropped
before the statistic is computed. If *every* stratum is empty there is
nothing to compute, so the criterion is undefined and
[`mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)
returns `NA` (with an `NA` value attribute when `include_value = TRUE`):

\
`empty_tbl`` ``<-`` `[`table`](https://rdrr.io/r/base/table.html)`(`\
`  `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`character`](https://rdrr.io/r/base/character.html)`(``0``)``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Active"``, ``"Control"``)``)``,`\
`  `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`logical`](https://rdrr.io/r/base/logical.html)`(``0``)``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRUE"``, ``"FALSE"``)``)``,`\
`  `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`character`](https://rdrr.io/r/base/character.html)`(``0``)``, levels ``=`` ``"A"``)`\
`)`\
\
[`mantel_fleiss_crit`](https://pharmaverse.github.io/tern/reference/mantel_fleiss_crit.md)`(``empty_tbl``, include_value ``=`` ``TRUE``)`\
`#> [1] NA`\
`#> attr(,"value")`\
`#> [1] NA`

When branching on the result, remember to handle this `NA` case
explicitly if your data can produce fully empty tables.

## References

Mantel, N., and J. L. Fleiss. 1980. “Minimum Expected Cell Size
Requirements for the Mantel-Haenszel One-Degree-of-Freedom Chi-Square
Test and a Related Rapid Procedure.” *American Journal of Epidemiology*
112 (1): 129–34.
