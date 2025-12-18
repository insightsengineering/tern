# Convert strings to `NA`

**\[stable\]**

SAS imports missing data as empty strings or strings with whitespaces
only. This helper function can be used to convert these values to `NA`s.

## Usage

``` r
sas_na(x, empty = TRUE, whitespaces = TRUE)
```

## Arguments

- x:

  (`factor` or `character`)  
  values for which any missing values should be substituted.

- empty:

  (`flag`)  
  if `TRUE`, empty strings get replaced by `NA`.

- whitespaces:

  (`flag`)  
  if `TRUE`, strings made from only whitespaces get replaced with `NA`.

## Value

`x` with `""` and/or whitespace-only values substituted by `NA`,
depending on the values of `empty` and `whitespaces`.

## Examples

``` r
sas_na(c("1", "", " ", "   ", "b"))
#> [1] "1" NA  NA  NA  "b"
sas_na(factor(c("", " ", "b")))
#> [1] <NA> <NA> b   
#> Levels: b

is.na(sas_na(c("1", "", " ", "   ", "b")))
#> [1] FALSE  TRUE  TRUE  TRUE FALSE
```
