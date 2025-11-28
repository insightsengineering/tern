# Add variable labels to top left corner in table

**\[stable\]**

Helper layout-creating function to append the variable labels of a given
variables vector from a given dataset in the top left corner. If a
variable label is not found then the variable name itself is used
instead. Multiple variable labels are concatenated with slashes.

## Usage

``` r
append_varlabels(lyt, df, vars, indent = 0L)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- vars:

  (`character`)  
  variable names of which the labels are to be looked up in `df`.

- indent:

  (`integer(1)`)  
  non-negative number of nested indent space, default to 0L which means
  no indent. 1L means two spaces indent, 2L means four spaces indent and
  so on.

## Value

A modified layout with the new variable label(s) added to the top-left
material.

## Note

This is not an optimal implementation of course, since we are using here
the data set itself during the layout creation. When we have a more
mature `rtables` implementation then this will also be improved or not
necessary anymore.

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX") %>%
  append_varlabels(DM, "SEX") %>%
  analyze("AGE", afun = mean) %>%
  append_varlabels(DM, "AGE", indent = 1)
build_table(lyt, DM)
#> SEX                   A: Drug X          B: Placebo       C: Combination 
#>   Age                  (N=121)            (N=106)            (N=129)     
#> —————————————————————————————————————————————————————————————————————————
#> F                                                                        
#>   mean             33.7142857142857   33.8392857142857   34.8852459016393
#> M                                                                        
#>   mean             36.5490196078431         32.1         34.2794117647059
#> U                                                                        
#>   mean                    NA                 NA                 NA       
#> UNDIFFERENTIATED                                                         
#>   mean                    NA                 NA                 NA       

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze("AGE", afun = mean) %>%
  append_varlabels(DM, c("SEX", "AGE"))
build_table(lyt, DM)
#> SEX / Age             A: Drug X          B: Placebo       C: Combination 
#> —————————————————————————————————————————————————————————————————————————
#> F                                                                        
#>   mean             33.7142857142857   33.8392857142857   34.8852459016393
#> M                                                                        
#>   mean             36.5490196078431         32.1         34.2794117647059
#> U                                                                        
#>   mean                    NA                 NA                 NA       
#> UNDIFFERENTIATED                                                         
#>   mean                    NA                 NA                 NA       
```
