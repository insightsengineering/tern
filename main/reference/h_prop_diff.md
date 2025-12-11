# Helper functions to calculate proportion difference

**\[stable\]**

## Usage

``` r
prop_diff_wald(rsp, grp, conf_level = 0.95, correct = FALSE)

prop_diff_ha(rsp, grp, conf_level)

prop_diff_nc(rsp, grp, conf_level, correct = FALSE)

prop_diff_cmh(
  rsp,
  grp,
  strata,
  conf_level = 0.95,
  diff_se = c("standard", "sato")
)

prop_diff_strat_nc(
  rsp,
  grp,
  strata,
  weights_method = c("cmh", "wilson_h"),
  conf_level = 0.95,
  correct = FALSE
)
```

## Arguments

- rsp:

  (`logical`)  
  vector indicating whether each subject is a responder or not.

- grp:

  (`factor`)  
  vector assigning observations to one out of two groups (e.g. reference
  and treatment group).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- correct:

  (`flag`)  
  whether to include the continuity correction. For further information,
  see [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

- strata:

  (`factor`)  
  variable with one level per stratum and same length as `rsp`.

- diff_se:

  (`string`)  
  method to estimate the standard error for the difference, either
  `standard` or `sato` (Sato et al. 1989) .

- weights_method:

  (`string`)  
  weights method. Can be either `"cmh"` or `"heuristic"` and directs the
  way weights are estimated.

## Value

A named `list` of elements `diff` (proportion difference) and `diff_ci`
(proportion difference confidence interval).

## Functions

- `prop_diff_wald()`: The Wald interval follows the usual textbook
  definition for a single proportion confidence interval using the
  normal approximation. It is possible to include a continuity
  correction for Wald's interval.

- `prop_diff_ha()`: Anderson-Hauck confidence interval (Hauck and
  Anderson 1986) .

- `prop_diff_nc()`: Newcombe confidence interval. It is based on the
  Wilson score confidence interval for a single binomial proportion
  (Newcombe 1998) .

- `prop_diff_cmh()`: Calculates the weighted difference. This is defined
  as the difference in response rates between the experimental treatment
  group and the control treatment group, adjusted for stratification
  factors by applying Cochran-Mantel-Haenszel (CMH) weights. For the CMH
  chi-squared test, use
  [`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html).

- `prop_diff_strat_nc()`: Calculates the stratified Newcombe confidence
  interval and difference in response rates between the experimental
  treatment group and the control treatment group, adjusted for
  stratification factors. This implementation follows closely the one
  proposed by Yan and Su (2010) . Weights can be estimated from the
  heuristic proposed in
  [`prop_strat_wilson()`](https://insightsengineering.github.io/tern/reference/h_proportions.md)
  or from CMH-derived weights (see `prop_diff_cmh()`).

## References

Hauck WW, Anderson S (1986). “A Comparison of Large-Sample Confidence
Interval Methods for the Difference of Two Binomial Probabilities.” *The
American Statistician*, **40**(4), 318–322.
[doi:10.2307/2684618](https://doi.org/10.2307/2684618) ,
[2025-12-08](https://insightsengineering.github.io/tern/reference/2025-12-08).  
  
Newcombe RG (1998). “Interval estimation for the difference between
independent proportions: comparison of eleven methods.” *Statistics in
Medicine*, **17**(8), 873-890.
[doi:10.1002/(SICI)1097-0258(19980430)17:8\<873::AID-SIM779\>3.0.CO;2-I](https://doi.org/10.1002/%28SICI%291097-0258%2819980430%2917%3A8%3C873%3A%3AAID-SIM779%3E3.0.CO%3B2-I)
.  
  
Sato T, Greenland S, Robins JM (1989). “On the variance estimator for
the Mantel-Haenszel Risk Difference.” *Biometrics*, **45**(4),
1323–1324. <http://www.jstor.org/stable/2531784>.  
  
Yan X, Su XG (2010). “Stratified Wilson and Newcombe Confidence
Intervals for Multiple Binomial Proportions.” *Stat. Biopharm. Res.*,
**2**(3), 329–335.

## See also

[`prop_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff.md)
for implementation of these helper functions.

## Examples

``` r
# Wald confidence interval
set.seed(2)
rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
grp <- factor(c(rep("A", 10), rep("B", 10)))

prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.95, correct = FALSE)
#> $diff
#> [1] 0
#> 
#> $diff_ci
#> [1] -0.4382613  0.4382613
#> 

# Anderson-Hauck confidence interval
## "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
#> $diff
#> [1] 0.25
#> 
#> $diff_ci
#> [1] -0.9195011  1.0000000
#> 

## Edge case: Same proportion of response in A and B.
rsp <- c(TRUE, FALSE, TRUE, FALSE)
grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))

prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
#> $diff
#> [1] 0
#> 
#> $diff_ci
#> [1] -0.8451161  0.8451161
#> 

# Newcombe confidence interval

set.seed(1)
rsp <- c(
  sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
  sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
)
grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
table(rsp, grp)
#>        grp
#> rsp      B  A
#>   FALSE 20 10
#>   TRUE  20 30

prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
#> $diff
#> [1] 0.25
#> 
#> $diff_ci
#> [1] 0.07193388 0.40725819
#> 

# Cochran-Mantel-Haenszel confidence interval

set.seed(2)
rsp <- sample(c(TRUE, FALSE), 100, TRUE)
grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
grp <- factor(grp, levels = c("Placebo", "Treatment"))
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)

prop_diff_cmh(
  rsp = rsp, grp = grp, strata = interaction(strata_data),
  conf_level = 0.90
)
#> $prop
#>   Placebo Treatment 
#> 0.5331117 0.3954251 
#> 
#> $prop_ci
#> $prop_ci$Placebo
#> [1] 0.4306536 0.6355698
#> 
#> $prop_ci$Treatment
#> [1] 0.2890735 0.5017768
#> 
#> 
#> $diff
#> [1] -0.1376866
#> 
#> $diff_ci
#> [1] -0.285363076  0.009989872
#> 
#> $se_diff
#> [1] 0.08978092
#> 
#> $weights
#>       a.x       b.x       a.y       b.y       a.z       b.z 
#> 0.1148388 0.2131696 0.1148388 0.2131696 0.1767914 0.1671918 
#> 
#> $n1
#> a.x b.x a.y b.y a.z b.z 
#>   4  11   8  11  13  11 
#> 
#> $n2
#> a.x b.x a.y b.y a.z b.z 
#>   8   9   4   9   6   6 
#> 
prop_diff_cmh(
  rsp = rsp, grp = grp, strata = interaction(strata_data),
  conf_level = 0.90, diff_se = "sato"
)
#> $prop
#>   Placebo Treatment 
#> 0.5331117 0.3954251 
#> 
#> $prop_ci
#> $prop_ci$Placebo
#> [1] 0.4306536 0.6355698
#> 
#> $prop_ci$Treatment
#> [1] 0.2890735 0.5017768
#> 
#> 
#> $diff
#> [1] -0.1376866
#> 
#> $diff_ci
#> [1] -0.31541846  0.04004526
#> 
#> $se_diff
#> [1] 0.1080533
#> 
#> $weights
#>       a.x       b.x       a.y       b.y       a.z       b.z 
#> 0.1148388 0.2131696 0.1148388 0.2131696 0.1767914 0.1671918 
#> 
#> $n1
#> a.x b.x a.y b.y a.z b.z 
#>   4  11   8  11  13  11 
#> 
#> $n2
#> a.x b.x a.y b.y a.z b.z 
#>   8   9   4   9   6   6 
#> 

# Stratified Newcombe confidence interval

set.seed(2)
data_set <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), 100, TRUE),
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  "grp" = sample(c("Placebo", "Treatment"), 100, TRUE),
  stringsAsFactors = TRUE
)

prop_diff_strat_nc(
  rsp = data_set$rsp, grp = data_set$grp, strata = interaction(data_set[2:3]),
  weights_method = "cmh",
  conf_level = 0.90
)
#> $diff
#> [1] -0.05777672
#> 
#> $diff_ci
#>      lower      upper 
#> -0.2236537  0.1119331 
#> 

prop_diff_strat_nc(
  rsp = data_set$rsp, grp = data_set$grp, strata = interaction(data_set[2:3]),
  weights_method = "wilson_h",
  conf_level = 0.90
)
#> $diff
#> [1] -0.07771884
#> 
#> $diff_ci
#>      lower      upper 
#> -0.2540844  0.1027720 
#> 
```
