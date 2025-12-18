# Reference and treatment group combination

**\[stable\]**

Facilitate the re-combination of groups divided as reference and
treatment groups; it helps in arranging groups of columns in the
`rtables` framework and teal modules.

## Usage

``` r
combine_groups(fct, ref = NULL, collapse = "/")
```

## Arguments

- fct:

  (`factor`)  
  the variable with levels which needs to be grouped.

- ref:

  (`character`)  
  the reference level(s).

- collapse:

  (`string`)  
  a character string to separate `fct` and `ref`.

## Value

A `list` with first item `ref` (reference) and second item `trt`
(treatment).

## Examples

``` r
groups <- combine_groups(
  fct = DM$ARM,
  ref = c("B: Placebo")
)

basic_table() %>%
  split_cols_by_groups("ARM", groups) %>%
  add_colcounts() %>%
  analyze_vars("AGE") %>%
  build_table(DM)
#>             B: Placebo    A: Drug X/C: Combination
#>               (N=106)             (N=250)         
#> ——————————————————————————————————————————————————
#> n               106                 250           
#> Mean (SD)   33.0 (6.3)           34.7 (7.1)       
#> Median         32.0                 33.0          
#> Min - Max   21.0 - 55.0         20.0 - 60.0       
```
