# Confidence intervals for a difference of binomials

**\[experimental\]**

Several confidence intervals for the difference between proportions.

## Usage

``` r
desctools_binom(
  x1,
  n1,
  x2,
  n2,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("ac", "wald", "waldcc", "score", "scorecc", "mn", "mee", "blj", "ha", "hal",
    "jp")
)

desctools_binomci(
  x,
  n,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys", "modified wilson",
    "wilsoncc", "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting",
    "pratt", "midp", "lik", "blaker"),
  rand = 123,
  tol = 1e-05
)
```

## Arguments

- conf.level:

  (`proportion`)  
  confidence level, defaults to 0.95.

- sides:

  (`string`)  
  side of the confidence interval to compute. Must be one of
  `"two-sided"` (default), `"left"`, or `"right"`.

- method:

  (`string`)  
  method to use. Can be one out of: `"wald"`, `"wilson"`, `"wilsoncc"`,
  `"agresti-coull"`, `"jeffreys"`, `"modified wilson"`,
  `"modified jeffreys"`, `"clopper-pearson"`, `"arcsine"`, `"logit"`,
  `"witting"`, `"pratt"`, `"midp"`, `"lik"`, and `"blaker"`.

- x:

  (`integer(1)`)  
  number of successes.

- n:

  (`integer(1)`)  
  number of trials.

## Value

A `matrix` of 3 values:

- `est`: estimate of proportion difference.

- `lwr.ci`: estimate of lower end of the confidence interval.

- `upr.ci`: estimate of upper end of the confidence interval.

A `matrix` with 3 columns containing:

- `est`: estimate of proportion difference.

- `lwr.ci`: lower end of the confidence interval.

- `upr.ci`: upper end of the confidence interval.

## Functions

- `desctools_binom()`: Several confidence intervals for the difference
  between proportions.

- `desctools_binomci()`: Compute confidence intervals for binomial
  proportions.
