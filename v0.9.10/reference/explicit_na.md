# Missing data

**\[stable\]**

Substitute missing data with a string or factor level.

## Usage

``` r
explicit_na(x, label = default_na_str(), drop_na = default_drop_na())

default_drop_na()

set_default_drop_na(drop_na)
```

## Arguments

- x:

  (`factor` or `character`)  
  values for which any missing values should be substituted.

- label:

  (`string`)  
  string that missing data should be replaced with.

- drop_na:

  (`flag`)  
  if `TRUE` and `x` is a factor, any levels that are only `label` will
  be dropped.

## Value

`x` with any `NA` values substituted by `label`.

- `tern_default_drop_na`: (`flag`)  
  default value for `drop_na` argument in `explicit_na()`.

&nbsp;

- `tern_default_drop_na` has no return value.

## Functions

- `default_drop_na()`: should `NA` values without a dedicated level be
  dropped?

- `set_default_drop_na()`: Setter for default `NA` value replacement
  string. Sets the option `"tern_default_drop_na"` within the R
  environment.

## Examples

``` r
explicit_na(c(NA, "a", "b"))
#> [1] "<Missing>" "a"         "b"        
is.na(explicit_na(c(NA, "a", "b")))
#> [1] FALSE FALSE FALSE

explicit_na(factor(c(NA, "a", "b")))
#> [1] <Missing> a         b        
#> Levels: a b <Missing>
is.na(explicit_na(factor(c(NA, "a", "b"))))
#> [1] FALSE FALSE FALSE

explicit_na(sas_na(c("a", "")))
#> [1] "a"         "<Missing>"

explicit_na(factor(levels = c(NA, "a")))
#> factor()
#> Levels: a
explicit_na(factor(levels = c(NA, "a")), drop_na = TRUE) # previous default
#> factor()
#> Levels: a
```
