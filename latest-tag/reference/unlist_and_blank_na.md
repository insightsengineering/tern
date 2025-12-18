# Blank for missing input

Helper function to use in tabulating model results.

## Usage

``` r
unlist_and_blank_na(x)
```

## Arguments

- x:

  (`vector`)  
  input for a cell.

## Value

An empty `character` vector if all entries in `x` are missing (`NA`),
otherwise the unlisted version of `x`.
