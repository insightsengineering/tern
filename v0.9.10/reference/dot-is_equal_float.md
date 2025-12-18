# Utility function to check if a float value is equal to another float value

Uses `.Machine$double.eps` as the tolerance for the comparison.

## Usage

``` r
.is_equal_float(x, y)
```

## Arguments

- x:

  (`numeric(1)`)  
  a float number.

- y:

  (`numeric(1)`)  
  a float number.

## Value

`TRUE` if identical, otherwise `FALSE`.
