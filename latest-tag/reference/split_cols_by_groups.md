# Split columns by groups of levels

**\[stable\]**

## Usage

``` r
split_cols_by_groups(lyt, var, groups_list = NULL, ref_group = NULL, ...)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- groups_list:

  (named `list` of `character`)  
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- ...:

  additional arguments to
  [`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
  in order. For instance, to control formats (`format`), add a joint
  column for all groups (`incl_all`).

## Value

A layout object suitable for passing to further layouting functions.
Adding this function to an `rtable` layout will add a column split
including the given groups to the table layout.

## See also

[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)

## Examples

``` r
# 1 - Basic use

# Without group combination `split_cols_by_groups` is
# equivalent to [rtables::split_cols_by()].
basic_table() %>%
  split_cols_by_groups("ARM") %>%
  add_colcounts() %>%
  analyze("AGE") %>%
  build_table(DM)
#>        A: Drug X   B: Placebo   C: Combination
#>         (N=121)     (N=106)        (N=129)    
#> ——————————————————————————————————————————————
#> Mean     34.91       33.02          34.57     

# Add a reference column.
basic_table() %>%
  split_cols_by_groups("ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  analyze(
    "AGE",
    afun = function(x, .ref_group, .in_ref_col) {
      if (.in_ref_col) {
        in_rows("Diff Mean" = rcell(NULL))
      } else {
        in_rows("Diff Mean" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
      }
    }
  ) %>%
  build_table(DM)
#>             A: Drug X   B: Placebo   C: Combination
#>              (N=121)     (N=106)        (N=129)    
#> ———————————————————————————————————————————————————
#> Diff Mean     1.89                        1.55     

# 2 - Adding group specification

# Manual preparation of the groups.
groups <- list(
  "Arms A+B" = c("A: Drug X", "B: Placebo"),
  "Arms A+C" = c("A: Drug X", "C: Combination")
)

# Use of split_cols_by_groups without reference column.
basic_table() %>%
  split_cols_by_groups("ARM", groups) %>%
  add_colcounts() %>%
  analyze("AGE") %>%
  build_table(DM)
#>        Arms A+B   Arms A+C
#>        (N=227)    (N=250) 
#> ——————————————————————————
#> Mean    34.03      34.73  

# Including differentiated output in the reference column.
basic_table() %>%
  split_cols_by_groups("ARM", groups_list = groups, ref_group = "Arms A+B") %>%
  analyze(
    "AGE",
    afun = function(x, .ref_group, .in_ref_col) {
      if (.in_ref_col) {
        in_rows("Diff. of Averages" = rcell(NULL))
      } else {
        in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
      }
    }
  ) %>%
  build_table(DM)
#>                     Arms A+B   Arms A+C
#> ———————————————————————————————————————
#> Diff. of Averages                0.71  

# 3 - Binary list dividing factor levels into reference and treatment

# `combine_groups` defines reference and treatment.
groups <- combine_groups(
  fct = DM$ARM,
  ref = c("A: Drug X", "B: Placebo")
)
groups
#> $`A: Drug X/B: Placebo`
#> [1] "A: Drug X"  "B: Placebo"
#> 
#> $`C: Combination`
#> [1] "C: Combination"
#> 

# Use group definition without reference column.
basic_table() %>%
  split_cols_by_groups("ARM", groups_list = groups) %>%
  add_colcounts() %>%
  analyze("AGE") %>%
  build_table(DM)
#>        A: Drug X/B: Placebo   C: Combination
#>              (N=227)             (N=129)    
#> ————————————————————————————————————————————
#> Mean          34.03               34.57     

# Use group definition with reference column (first item of groups).
basic_table() %>%
  split_cols_by_groups("ARM", groups, ref_group = names(groups)[1]) %>%
  add_colcounts() %>%
  analyze(
    "AGE",
    afun = function(x, .ref_group, .in_ref_col) {
      if (.in_ref_col) {
        in_rows("Diff Mean" = rcell(NULL))
      } else {
        in_rows("Diff Mean" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
      }
    }
  ) %>%
  build_table(DM)
#>             A: Drug X/B: Placebo   C: Combination
#>                   (N=227)             (N=129)    
#> —————————————————————————————————————————————————
#> Diff Mean                               0.54     
```
