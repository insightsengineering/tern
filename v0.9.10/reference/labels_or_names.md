# Labels or names of list elements

Helper function for working with nested statistic function results which
typically don't have labels but names that we can use.

## Usage

``` r
labels_or_names(x)
```

## Arguments

- x:

  (`list`)  
  a list.

## Value

A `character` vector with the labels or names for the list elements.

## Examples

``` r
x <- data.frame(
  a = 1:10,
  b = rnorm(10)
)
labels_or_names(x)
#>   a   b 
#> "a" "b" 
var_labels(x) <- c(b = "Label for b", a = NA)
labels_or_names(x)
#>             a             b 
#>           "a" "Label for b" 
```
