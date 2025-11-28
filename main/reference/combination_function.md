# Class for `CombinationFunction`

**\[stable\]**

`CombinationFunction` is an S4 class which extends standard functions.
These are special functions that can be combined and negated with the
logical operators.

## Usage

``` r
# S4 method for class 'CombinationFunction,CombinationFunction'
e1 & e2

# S4 method for class 'CombinationFunction,CombinationFunction'
e1 | e2

# S4 method for class 'CombinationFunction'
!x
```

## Arguments

- e1:

  (`CombinationFunction`)  
  left hand side of logical operator.

- e2:

  (`CombinationFunction`)  
  right hand side of logical operator.

- x:

  (`CombinationFunction`)  
  the function which should be negated.

## Value

A logical value indicating whether the left hand side of the equation
equals the right hand side.

## Functions

- `e1 & e2`: Logical "AND" combination of `CombinationFunction`
  functions. The resulting object is of the same class, and evaluates
  the two argument functions. The result is then the "AND" of the two
  individual results.

- `e1 | e2`: Logical "OR" combination of `CombinationFunction`
  functions. The resulting object is of the same class, and evaluates
  the two argument functions. The result is then the "OR" of the two
  individual results.

- `` `!`(CombinationFunction) ``: Logical negation of
  `CombinationFunction` functions. The resulting object is of the same
  class, and evaluates the original function. The result is then the
  opposite of this results.

## Examples

``` r
higher <- function(a) {
  force(a)
  CombinationFunction(
    function(x) {
      x > a
    }
  )
}

lower <- function(b) {
  force(b)
  CombinationFunction(
    function(x) {
      x < b
    }
  )
}

c1 <- higher(5)
c2 <- lower(10)
c3 <- higher(5) & lower(10)
c3(7)
#> [1] TRUE
```
