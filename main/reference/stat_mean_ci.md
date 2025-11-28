# Confidence interval for mean

**\[stable\]**

Convenient function for calculating the mean confidence interval. It
calculates the arithmetic as well as the geometric mean. It can be used
as a `ggplot` helper function for plotting.

## Usage

``` r
stat_mean_ci(
  x,
  conf_level = 0.95,
  na.rm = TRUE,
  n_min = 2,
  gg_helper = TRUE,
  geom_mean = FALSE
)
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

- n_min:

  (`numeric(1)`)  
  a minimum number of non-missing `x` to estimate the confidence
  interval for mean.

- gg_helper:

  (`flag`)  
  whether output should be aligned for use with `ggplot`s.

- geom_mean:

  (`flag`)  
  whether the geometric mean should be calculated.

## Value

A named `vector` of values `mean_ci_lwr` and `mean_ci_upr`.

## Examples

``` r
stat_mean_ci(sample(10), gg_helper = FALSE)
#> mean_ci_lwr mean_ci_upr 
#>    3.334149    7.665851 

p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
  ggplot2::geom_point()

p + ggplot2::stat_summary(
  fun.data = stat_mean_ci,
  geom = "errorbar"
)


p + ggplot2::stat_summary(
  fun.data = stat_mean_ci,
  fun.args = list(conf_level = 0.5),
  geom = "errorbar"
)


p + ggplot2::stat_summary(
  fun.data = stat_mean_ci,
  fun.args = list(conf_level = 0.5, geom_mean = TRUE),
  geom = "errorbar"
)

```
