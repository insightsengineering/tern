# Replicate entries of a vector if required

**\[stable\]**

Replicate entries of a vector if required.

## Usage

``` r
to_n(x, n)
```

## Arguments

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- n:

  (`integer(1)`)  
  number of entries that are needed.

## Value

`x` if it has the required length already or is `NULL`, otherwise if it
is scalar the replicated version of it with `n` entries.

## Note

This function will fail if `x` is not of length `n` and/or is not a
scalar.
