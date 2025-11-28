# Confidence interval for median

**\[stable\]**

Convenient function for calculating the median confidence interval. It
can be used as a `ggplot` helper function for plotting.

## Usage

``` r
stat_median_ci(x, conf_level = 0.95, na.rm = TRUE, gg_helper = TRUE)
```

## Arguments

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- na.rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- gg_helper:

  (`flag`)  
  whether output should be aligned for use with `ggplot`s.

## Value

A named `vector` of values `median_ci_lwr` and `median_ci_upr`.

## Details

This function was adapted from `DescTools/versions/0.99.35/source`

## Examples

``` r
stat_median_ci(sample(10), gg_helper = FALSE)
#> median_ci_lwr median_ci_upr 
#>             2             9 
#> attr(,"conf_level")
#> [1] 0.9785156

p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
  ggplot2::geom_point()
p + ggplot2::stat_summary(
  fun.data = stat_median_ci,
  geom = "errorbar"
)

```
