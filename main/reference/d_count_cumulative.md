# Description of cumulative count

**\[stable\]**

This is a helper function that describes the analysis in
[`s_count_cumulative()`](https://insightsengineering.github.io/tern/reference/count_cumulative.md).

## Usage

``` r
d_count_cumulative(threshold, lower_tail = TRUE, include_eq = TRUE)
```

## Arguments

- threshold:

  (`numeric(1)`)  
  a cutoff value as threshold to count values of `x`.

- lower_tail:

  (`flag`)  
  whether to count lower tail, default is `TRUE`.

- include_eq:

  (`flag`)  
  whether to include value equal to the `threshold` in count, default is
  `TRUE`.

## Value

Labels for
[`s_count_cumulative()`](https://insightsengineering.github.io/tern/reference/count_cumulative.md).
