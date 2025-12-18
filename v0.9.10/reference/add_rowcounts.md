# Layout-creating function to add row total counts

**\[stable\]**

This works analogously to
[`rtables::add_colcounts()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_colcounts.html)
but on the rows. This function is a wrapper for
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

## Usage

``` r
add_rowcounts(lyt, alt_counts = FALSE)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- alt_counts:

  (`flag`)  
  whether row counts should be taken from `alt_counts_df` (`TRUE`) or
  from `df` (`FALSE`). Defaults to `FALSE`.

## Value

A modified layout where the latest row split labels now have the
row-wise total counts (i.e. without column-based subsetting) attached in
parentheses.

## Note

Row count values are contained in these row count rows but are not
displayed so that they are not considered zero rows by default when
pruning.

## Examples

``` r
basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  split_rows_by("RACE", split_fun = drop_split_levels) %>%
  add_rowcounts() %>%
  analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx") %>%
  build_table(DM)
#>                                    A: Drug X   B: Placebo   C: Combination
#>                                     (N=121)     (N=106)        (N=129)    
#> ——————————————————————————————————————————————————————————————————————————
#> ASIAN (N=231)                                                             
#>   Min.                               20.00       21.00          22.00     
#>   1st Qu.                            29.00       28.00          30.00     
#>   Median                             33.00       32.50          33.50     
#>   Mean                               34.20       32.68          34.63     
#>   3rd Qu.                            38.50       36.00          38.00     
#>   Max.                               58.00       55.00          53.00     
#> BLACK OR AFRICAN AMERICAN (N=79)                                          
#>   Min.                               23.00       21.00          24.00     
#>   1st Qu.                            29.00       28.75          29.00     
#>   Median                             33.00       30.00          32.00     
#>   Mean                               34.68       31.71          34.00     
#>   3rd Qu.                            37.25       36.25          39.00     
#>   Max.                               60.00       42.00          51.00     
#> WHITE (N=46)                                                              
#>   Min.                               30.00       25.00          28.00     
#>   1st Qu.                            38.00       31.00          30.25     
#>   Median                             40.50       37.50          35.00     
#>   Mean                               39.36       36.93          35.11     
#>   3rd Qu.                            43.50       40.00          37.50     
#>   Max.                               47.00       55.00          47.00     
```
