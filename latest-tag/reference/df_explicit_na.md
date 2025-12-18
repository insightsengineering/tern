# Encode categorical missing values in a data frame

**\[stable\]**

This is a helper function to encode missing entries across groups of
categorical variables in a data frame.

## Usage

``` r
df_explicit_na(
  data,
  omit_columns = NULL,
  char_as_factor = TRUE,
  logical_as_factor = FALSE,
  na_level = "<Missing>"
)
```

## Arguments

- data:

  (`data.frame`)  
  data set.

- omit_columns:

  (`character`)  
  names of variables from `data` that should not be modified by this
  function.

- char_as_factor:

  (`flag`)  
  whether to convert character variables in `data` to factors.

- logical_as_factor:

  (`flag`)  
  whether to convert logical variables in `data` to factors.

- na_level:

  (`string`)  
  string used to replace all `NA` or empty values inside
  non-`omit_columns` columns.

## Value

A `data.frame` with the chosen modifications applied.

## Details

Missing entries are those with `NA` or empty strings and will be
replaced with a specified value. If factor variables include missing
values, the missing value will be inserted as the last level. Similarly,
in case character or logical variables should be converted to factors
with the `char_as_factor` or `logical_as_factor` options, the missing
values will be set as the last level.

## See also

[`sas_na()`](https://insightsengineering.github.io/tern/reference/sas_na.md)
and
[`explicit_na()`](https://insightsengineering.github.io/tern/reference/explicit_na.md)
for other missing data helper functions.

## Examples

``` r
my_data <- data.frame(
  u = c(TRUE, FALSE, NA, TRUE),
  v = factor(c("A", NA, NA, NA), levels = c("Z", "A")),
  w = c("A", "B", NA, "C"),
  x = c("D", "E", "F", NA),
  y = c("G", "H", "I", ""),
  z = c(1, 2, 3, 4),
  stringsAsFactors = FALSE
)

# Example 1
# Encode missing values in all character or factor columns.
df_explicit_na(my_data)
#>       u         v         w         x         y z
#> 1  TRUE         A         A         D         G 1
#> 2 FALSE <Missing>         B         E         H 2
#> 3    NA <Missing> <Missing>         F         I 3
#> 4  TRUE <Missing>         C <Missing> <Missing> 4
# Also convert logical columns to factor columns.
df_explicit_na(my_data, logical_as_factor = TRUE)
#>           u         v         w         x         y z
#> 1      TRUE         A         A         D         G 1
#> 2     FALSE <Missing>         B         E         H 2
#> 3 <Missing> <Missing> <Missing>         F         I 3
#> 4      TRUE <Missing>         C <Missing> <Missing> 4
# Encode missing values in a subset of columns.
df_explicit_na(my_data, omit_columns = c("x", "y"))
#>       u         v         w    x y z
#> 1  TRUE         A         A    D G 1
#> 2 FALSE <Missing>         B    E H 2
#> 3    NA <Missing> <Missing>    F I 3
#> 4  TRUE <Missing>         C <NA>   4

# Example 2
# Here we purposefully convert all `M` values to `NA` in the `SEX` variable.
# After running `df_explicit_na` the `NA` values are encoded as `<Missing>` but they are not
# included when generating `rtables`.
adsl <- tern_ex_adsl
adsl$SEX[adsl$SEX == "M"] <- NA
adsl <- df_explicit_na(adsl)

# If you want the `Na` values to be displayed in the table use the `na_level` argument.
adsl <- tern_ex_adsl
adsl$SEX[adsl$SEX == "M"] <- NA
adsl <- df_explicit_na(adsl, na_level = "Missing Values")

# Example 3
# Numeric variables that have missing values are not altered. This means that any `NA` value in
# a numeric variable will not be included in the summary statistics, nor will they be included
# in the denominator value for calculating the percent values.
adsl <- tern_ex_adsl
adsl$AGE[adsl$AGE < 30] <- NA
adsl <- df_explicit_na(adsl)
```
