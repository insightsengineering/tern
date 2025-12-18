# Convert list of groups to a data frame

This converts a list of group levels into a data frame format which is
expected by
[`rtables::add_combo_levels()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html).

## Usage

``` r
groups_list_to_df(groups_list)
```

## Arguments

- groups_list:

  (named `list` of `character`)  
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

## Value

A `tibble` in the required format.

## Examples

``` r
grade_groups <- list(
  "Any Grade (%)" = c("1", "2", "3", "4", "5"),
  "Grade 3-4 (%)" = c("3", "4"),
  "Grade 5 (%)" = "5"
)
groups_list_to_df(grade_groups)
#> # A tibble: 3 × 4
#>   valname  label         levelcombo exargs    
#>   <chr>    <chr>         <list>     <list>    
#> 1 AnyGrade Any Grade (%) <chr [5]>  <list [0]>
#> 2 Grade34  Grade 3-4 (%) <chr [2]>  <list [0]>
#> 3 Grade5   Grade 5 (%)   <chr [1]>  <list [0]>
```
