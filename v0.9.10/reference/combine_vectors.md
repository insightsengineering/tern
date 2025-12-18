# Element-wise combination of two vectors

Element-wise combination of two vectors

## Usage

``` r
combine_vectors(x, y)
```

## Arguments

- x:

  (`vector`)  
  first vector to combine.

- y:

  (`vector`)  
  second vector to combine.

## Value

A `list` where each element combines corresponding elements of `x` and
`y`.

## Examples

``` r
combine_vectors(1:3, 4:6)
#> [[1]]
#> [1] 1 4
#> 
#> [[2]]
#> [1] 2 5
#> 
#> [[3]]
#> [1] 3 6
#> 
```
