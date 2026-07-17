# Root-finding CI bounds from one-sided p-value functions

This function is an internal helper for
[`prop_diff_uncond_exact()`](https://insightsengineering.github.io/tern/reference/h_prop_diff.md).

## Usage

``` r
h_find_ci_bound_uniroot(
  p_value_function,
  cutoff,
  direction = c("increasing", "decreasing"),
  interval = c(-1, 1),
  tol = 1e-06,
  maxiter = 1000
)
```

## Arguments

- p_value_function:

  (`function`) one-sided p-value function in terms of `d`.

- cutoff:

  (`number`) one-sided significance level threshold.

- direction:

  (`string`) one of `"increasing"` or `"decreasing"`.

- interval:

  (`numeric`) 2-element search interval.

- tol:

  (`number`) tolerance for
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  (`integer`) maximum number of
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) iterations.

## Value

A `number` with the requested CI bound, or `NA_real_` if no bound
exists.
