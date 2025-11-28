# Description function for `s_count_abnormal_by_baseline()`

**\[stable\]**

Description function that produces the labels for
[`s_count_abnormal_by_baseline()`](https://insightsengineering.github.io/tern/reference/abnormal_by_baseline.md).

## Usage

``` r
d_count_abnormal_by_baseline(abnormal)
```

## Arguments

- abnormal:

  (`character`)  
  values identifying the abnormal range level(s) in `.var`.

## Value

Abnormal category labels for
[`s_count_abnormal_by_baseline()`](https://insightsengineering.github.io/tern/reference/abnormal_by_baseline.md).

## Examples

``` r
d_count_abnormal_by_baseline("LOW")
#> $not_abnormal
#> [1] "Not low"
#> 
#> $abnormal
#> [1] "Low"
#> 
#> $total
#> [1] "Total"
#> 
```
