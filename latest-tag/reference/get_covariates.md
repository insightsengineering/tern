# Utility function to return a named list of covariate names

**\[stable\]**

## Usage

``` r
get_covariates(covariates)
```

## Arguments

- covariates:

  (`character`)  
  a vector that can contain single variable names (such as `"X1"`),
  and/or interaction terms indicated by `"X1 * X2"`.

## Value

A named `list` of `character` vector.

## Examples

``` r
get_covariates(c("a * b", "c"))
#> $a
#> [1] "a"
#> 
#> $b
#> [1] "b"
#> 
#> $c
#> [1] "c"
#> 
```
