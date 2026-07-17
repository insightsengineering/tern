# Worst case tail probability for unconditional exact CI calculation

This function is an internal helper for
[`prop_diff_uncond_exact()`](https://insightsengineering.github.io/tern/reference/h_prop_diff.md).

## Usage

``` r
h_worst_case_tail_probability(
  d_star,
  n1,
  n2,
  t_values,
  t0,
  tables,
  tail = c("upper", "lower")
)
```

## Arguments

- d_star:

  (`number`) hypothesized difference in proportions.

- n1:

  (`positive integer`) sample size in group 1.

- n2:

  (`positive integer`) sample size in group 2.

- t_values:

  (`numeric`) vector of test statistic values from enumerated tables.

- t0:

  (`number`) observed test statistic value.

- tables:

  (`data.frame`) with columns `n11` and `n21` containing enumerated
  outcomes in each group.

- tail:

  (`string`) one of `"upper"` or `"lower"` indicating which tail to
  compute.

## Value

A `number` between 0 and 1 corresponding to the worst-case one-sided
tail probability at the hypothesized difference.
