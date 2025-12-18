# Description of the proportion summary

**\[stable\]**

This is a helper function that describes the analysis in
[`s_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md).

## Usage

``` r
d_proportion(conf_level, method, long = FALSE)
```

## Arguments

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string`)  
  the method used to construct the confidence interval for proportion of
  successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`,
  `wilson`, `wilsonc`, `strat_wilson`, `strat_wilsonc`, `agresti-coull`
  or `jeffreys`.

- long:

  (`flag`)  
  whether a long or a short (default) description is required.

## Value

String describing the analysis.
