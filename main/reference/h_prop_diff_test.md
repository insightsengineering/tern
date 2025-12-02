# Helper functions to test proportion differences

Helper functions to implement various tests on the difference between
two proportions.

## Usage

``` r
prop_chisq(tbl, alternative = c("two.sided", "less", "greater"))

prop_cmh(ary, alternative = c("two.sided", "less", "greater"))

prop_schouten(tbl, alternative = c("two.sided", "less", "greater"))

prop_fisher(tbl, alternative = c("two.sided", "less", "greater"))
```

## Arguments

- tbl:

  (`matrix`)  
  matrix with two groups in rows and the binary response
  (`TRUE`/`FALSE`) in columns.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- ary:

  (`array`, 3 dimensions)  
  array with two groups in rows, the binary response (`TRUE`/`FALSE`) in
  columns, and the strata in the third dimension.

## Value

A p-value.

## Functions

- `prop_chisq()`: Performs Chi-Squared test. Internally calls
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

- `prop_cmh()`: Performs stratified Cochran-Mantel-Haenszel test.
  Internally calls
  [`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html).
  Note that strata with less than two observations are automatically
  discarded.

- `prop_schouten()`: Performs the Chi-Squared test with Schouten
  correction.

- `prop_fisher()`: Performs the Fisher's exact test. Internally calls
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## See also

[`prop_diff_test()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md)
for implementation of these helper functions.

Schouten correction is based upon Schouten et al. (1980) .
