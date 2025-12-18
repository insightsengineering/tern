# Split parameters

**\[deprecated\]**

It divides the data in the vector `param` into the groups defined by `f`
based on specified `values`. It is relevant in `rtables` layers so as to
distribute parameters `.stats` or' `.formats` into lists with items
corresponding to specific analysis function.

## Usage

``` r
h_split_param(param, value, f)
```

## Arguments

- param:

  (`vector`)  
  the parameter to be split.

- value:

  (`vector`)  
  the value used to split.

- f:

  (`list`)  
  the reference to make the split.

## Value

A named `list` with the same element names as `f`, each containing the
elements specified in `.stats`.

## Examples

``` r
f <- list(
  surv = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci"),
  surv_diff = c("rate_diff", "rate_diff_ci", "ztest_pval")
)

.stats <- c("pt_at_risk", "rate_diff")
h_split_param(.stats, .stats, f = f)
#> Warning: `h_split_param()` was deprecated in tern 0.9.8.
#> $surv
#> [1] "pt_at_risk"
#> 
#> $surv_diff
#> [1] "rate_diff"
#> 

# $surv
# [1] "pt_at_risk"
#
# $surv_diff
# [1] "rate_diff"

.formats <- c("pt_at_risk" = "xx", "event_free_rate" = "xxx")
h_split_param(.formats, names(.formats), f = f)
#> $surv
#>      pt_at_risk event_free_rate 
#>            "xx"           "xxx" 
#> 
#> $surv_diff
#> NULL
#> 

# $surv
# pt_at_risk event_free_rate
# "xx"           "xxx"
#
# $surv_diff
# NULL
```
