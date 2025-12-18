# Split data frame by subgroups

**\[stable\]**

Split a data frame into a non-nested list of subsets.

## Usage

``` r
h_split_by_subgroups(data, subgroups, groups_lists = list())
```

## Arguments

- data:

  (`data.frame`)  
  dataset to split.

- subgroups:

  (`character`)  
  names of factor variables from `data` used to create subsets. Unused
  levels not present in `data` are dropped. Note that the order in this
  vector determines the order in the downstream table.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

## Value

A list with subset data (`df`) and metadata about the subset
(`df_labels`).

## Details

Main functionality is to prepare data for use in forest plot layouts.

## Examples

``` r
df <- data.frame(
  x = c(1:5),
  y = factor(c("A", "B", "A", "B", "A"), levels = c("A", "B", "C")),
  z = factor(c("C", "C", "D", "D", "D"), levels = c("D", "C"))
)
formatters::var_labels(df) <- paste("label for", names(df))

h_split_by_subgroups(
  data = df,
  subgroups = c("y", "z")
)
#> $y.A
#> $y.A$df
#>   x y z
#> 1 1 A C
#> 2 3 A D
#> 3 5 A D
#> 
#> $y.A$df_labels
#>   subgroup var   var_label
#> 1        A   y label for y
#> 
#> 
#> $y.B
#> $y.B$df
#>   x y z
#> 1 2 B C
#> 2 4 B D
#> 
#> $y.B$df_labels
#>   subgroup var   var_label
#> 1        B   y label for y
#> 
#> 
#> $z.D
#> $z.D$df
#>   x y z
#> 1 3 A D
#> 2 4 B D
#> 3 5 A D
#> 
#> $z.D$df_labels
#>   subgroup var   var_label
#> 1        D   z label for z
#> 
#> 
#> $z.C
#> $z.C$df
#>   x y z
#> 1 1 A C
#> 2 2 B C
#> 
#> $z.C$df_labels
#>   subgroup var   var_label
#> 1        C   z label for z
#> 
#> 

h_split_by_subgroups(
  data = df,
  subgroups = c("y", "z"),
  groups_lists = list(
    y = list("AB" = c("A", "B"), "C" = "C")
  )
)
#> $y.AB
#> $y.AB$df
#>   x y z
#> 1 1 A C
#> 2 2 B C
#> 3 3 A D
#> 4 4 B D
#> 5 5 A D
#> 
#> $y.AB$df_labels
#>   subgroup var   var_label
#> 1       AB   y label for y
#> 
#> 
#> $z.D
#> $z.D$df
#>   x y z
#> 1 3 A D
#> 2 4 B D
#> 3 5 A D
#> 
#> $z.D$df_labels
#>   subgroup var   var_label
#> 1        D   z label for z
#> 
#> 
#> $z.C
#> $z.C$df
#>   x y z
#> 1 1 A C
#> 2 2 B C
#> 
#> $z.C$df_labels
#>   subgroup var   var_label
#> 1        C   z label for z
#> 
#> 
```
