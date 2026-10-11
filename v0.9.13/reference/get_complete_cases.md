# Remove rows with missing values

**\[experimental\]**

Remove rows containing one or more missing values from a data frame. If
any rows are omitted, a warning is issued reporting the number of
removed rows, unless `quiet` is `TRUE`.

## Usage

``` r
get_complete_cases(df, quiet = FALSE, additional_message = ".")
```

## Arguments

- df:

  (`data.frame`)\
  A data frame.

- quiet:

  (`logical(1)`)\
  Whether to suppress the warning when rows with missing values are
  omitted.

- additional_message:

  (`character(1)`)\
  A message appended to the default warning. The default warning reports
  the number of rows removed.

## Value

A `data.frame` containing only complete rows. The original column
structure is preserved. If no rows contain missing values, `df` is
returned unchanged.

## Details

A row is considered incomplete if at least one of its values is missing.
Missingness is determined using
[`stats::complete.cases()`](https://rdrr.io/r/stats/complete.cases.html).

If one or more rows contain missing values, those rows are omitted and a
warning is issued reporting the number of omitted rows, unless `quiet`
is `TRUE`. The value of `additional_message` is appended to the warning
message.

If no rows contain missing values, `df` is returned unchanged and no
warning is issued.

## Examples

``` r
df <- data.frame(a = c(1:5, NA), b = c(NA, letters[1:5]))
df
#>    a    b
#> 1  1 <NA>
#> 2  2    a
#> 3  3    b
#> 4  4    c
#> 5  5    d
#> 6 NA    e

get_complete_cases(df)
#> Warning: 2 row(s) with missing values were omitted.
#>   a b
#> 2 2 a
#> 3 3 b
#> 4 4 c
#> 5 5 d
```
