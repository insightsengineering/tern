# Helper functions to test proportion differences

Helper functions to implement various tests on the difference between
two proportions.

## Usage

``` r
prop_chisq(tbl, alternative = c("two.sided", "less", "greater"))

prop_cmh(
  ary,
  alternative = c("two.sided", "less", "greater"),
  diff_se = c("standard", "sato"),
  transform = c("none", "wilson_hilferty")
)

prop_schouten(tbl, alternative = c("two.sided", "less", "greater"))

prop_fisher(tbl, alternative = c("two.sided", "less", "greater"))
```

## Arguments

- tbl:

  (`matrix`)\
  matrix with two groups in rows and the binary response
  (`TRUE`/`FALSE`) in columns.

- alternative:

  (`string`)\
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- ary:

  (`array`, 3 dimensions)\
  array with two groups in rows, the binary response (`TRUE`/`FALSE`) in
  columns, and the strata in the third dimension.

- diff_se:

  (`string`)\
  either `standard` or `sato`; specifies whether to use the Sato
  variance estimator to calculate the chi-squared statistic.

- transform:

  (`string`)\
  either `none` or `wilson_hilferty`; specifies whether to apply the
  Wilson-Hilferty transformation of the chi-squared statistic.

## Value

A p-value.

## Functions

- `prop_chisq()`: Performs Chi-Squared test. Internally calls
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

- `prop_cmh()`: Performs stratified Cochran-Mantel-Haenszel test, using
  [`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html)
  internally. Note that strata with less than two observations are
  automatically discarded.

- `prop_schouten()`: Performs the Chi-Squared test with Schouten
  correction.

- `prop_fisher()`: Performs the Fisher's exact test. Internally calls
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## See also

[`prop_diff_test()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md)
for implementation of these helper functions.

Schouten correction is based upon Schouten et al. (1980) .

## Examples

``` r
# Chi-Squared test
tbl <- matrix(
  c(13, 7, 8, 12),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(group = c("A", "B"), response = c("TRUE", "FALSE"))
)

prop_chisq(tbl)
#> [1] 0.1133944

# Cochran-Mantel-Haenszel test with two strata
ary <- array(
  c(12, 8, 8, 12, 10, 10, 6, 14),
  dim = c(2, 2, 2),
  dimnames = list(
    group = c("A", "B"),
    response = c("TRUE", "FALSE"),
    strata = c("Low", "High")
  )
)

prop_cmh(ary)
#> [1] 0.07437734
#> attr(,"z_stat")
#> [1] -1.784285

# Chi-Squared test with Schouten correction
tbl <- matrix(
  c(13, 7, 8, 12),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(group = c("A", "B"), response = c("TRUE", "FALSE"))
)

prop_schouten(tbl)
#> [1] 0.1594617

# Fisher's exact test
tbl <- matrix(
  c(13, 7, 8, 12),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(group = c("A", "B"), response = c("TRUE", "FALSE"))
)

prop_fisher(tbl)
#> [1] 0.2049272
```
