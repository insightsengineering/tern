# Convert to `rtable`

**\[stable\]**

This is a new generic function to convert objects to `rtable` tables.

## Usage

``` r
as.rtable(x, ...)

# S3 method for class 'data.frame'
as.rtable(x, format = "xx.xx", ...)
```

## Arguments

- x:

  (`data.frame`)\
  the object which should be converted to an `rtable`.

- ...:

  additional arguments for methods.

- format:

  (`string` or `function`)\
  the format which should be used for the columns.

## Value

An `rtables` table object. Note that the concrete class will depend on
the method used.

## Methods (by class)

- `as.rtable(data.frame)`: Method for converting a `data.frame` that
  contains numeric columns to `rtable`.

## Examples

``` r
x <- data.frame(
  a = 1:10,
  b = rnorm(10)
)
as.rtable(x)
#>        a       b  
#> ——————————————————
#> 1    1.00    0.07 
#> 2    2.00    -0.64
#> 3    3.00    -0.05
#> 4    4.00    -0.25
#> 5    5.00    0.44 
#> 6    6.00    2.76 
#> 7    7.00    0.05 
#> 8    8.00    0.58 
#> 9    9.00    0.12 
#> 10   10.00   -1.91
```
