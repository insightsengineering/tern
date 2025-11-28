# Description of the difference test between two proportions

**\[stable\]**

This is an auxiliary function that describes the analysis in
`s_test_proportion_diff`.

## Usage

``` r
d_test_proportion_diff(method)
```

## Arguments

- method:

  (`string`)  
  one of `chisq`, `cmh`, `fisher`, or `schouten`; specifies the test
  used to calculate the p-value.

## Value

A `string` describing the test from which the p-value is derived.
