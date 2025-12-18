# Combine counts

Simplifies the estimation of column counts, especially when group
combination is required.

## Usage

``` r
combine_counts(fct, groups_list = NULL)
```

## Arguments

- fct:

  (`factor`)  
  the variable with levels which needs to be grouped.

- groups_list:

  (named `list` of `character`)  
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

## Value

A `vector` of column counts.

## See also

[`combine_groups()`](https://insightsengineering.github.io/tern/reference/combine_groups.md)

## Examples

``` r
ref <- c("A: Drug X", "B: Placebo")
groups <- combine_groups(fct = DM$ARM, ref = ref)

col_counts <- combine_counts(
  fct = DM$ARM,
  groups_list = groups
)

basic_table() %>%
  split_cols_by_groups("ARM", groups) %>%
  add_colcounts() %>%
  analyze_vars("AGE") %>%
  build_table(DM, col_counts = col_counts)
#>             A: Drug X/B: Placebo   C: Combination
#>                   (N=227)             (N=129)    
#> —————————————————————————————————————————————————
#> n                   227                 129      
#> Mean (SD)        34.0 (7.2)          34.6 (6.5)  
#> Median              33.0                33.0     
#> Min - Max       20.0 - 60.0         22.0 - 53.0  

ref <- "A: Drug X"
groups <- combine_groups(fct = DM$ARM, ref = ref)
col_counts <- combine_counts(
  fct = DM$ARM,
  groups_list = groups
)

basic_table() %>%
  split_cols_by_groups("ARM", groups) %>%
  add_colcounts() %>%
  analyze_vars("AGE") %>%
  build_table(DM, col_counts = col_counts)
#>              A: Drug X    B: Placebo/C: Combination
#>               (N=121)              (N=235)         
#> ———————————————————————————————————————————————————
#> n               121                  235           
#> Mean (SD)   34.9 (7.8)           33.9 (6.5)        
#> Median         33.0                 33.0           
#> Min - Max   20.0 - 60.0          21.0 - 55.0       
```
