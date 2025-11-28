# Extract elements by name

This utility function extracts elements from a vector `x` by `names`.
Differences to the standard `[` function are:

## Usage

``` r
extract_by_name(x, names)
```

## Arguments

- x:

  (named `vector`)  
  where to extract named elements from.

- names:

  (`character`)  
  vector of names to extract.

## Value

`NULL` if `x` is `NULL`, otherwise the extracted elements from `x`.

## Details

- If `x` is `NULL`, then still always `NULL` is returned (same as in
  base function).

- If `x` is not `NULL`, then the intersection of its names is made with
  `names` and those elements are returned. That is, `names` which don't
  appear in `x` are not returned as `NA`s.
