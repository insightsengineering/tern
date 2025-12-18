# Check proportion difference arguments

**\[stable\]**

Verifies that and/or convert arguments into valid values to be used in
the estimation of difference in responder proportions.

## Usage

``` r
check_diff_prop_ci(rsp, grp, strata = NULL, conf_level, correct = NULL)
```

## Arguments

- rsp:

  (`logical`)  
  vector indicating whether each subject is a responder or not.

- grp:

  (`factor`)  
  vector assigning observations to one out of two groups (e.g. reference
  and treatment group).

- strata:

  (`factor`)  
  variable with one level per stratum and same length as `rsp`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- correct:

  (`flag`)  
  whether to include the continuity correction. For further information,
  see [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

## Examples

``` r
# example code
## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
nex <- 100 # Number of example rows
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)
check_diff_prop_ci(rsp = dta[["rsp"]], grp = dta[["grp"]], conf_level = 0.95)
```
