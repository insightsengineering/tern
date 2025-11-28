# Helper function for `s_count_occurrences_by_grade()`

**\[stable\]**

Helper function for
[`s_count_occurrences_by_grade()`](https://insightsengineering.github.io/tern/reference/count_occurrences_by_grade.md)
to insert grade groupings into list with individual grade frequencies.
The order of the final result follows the order of `grade_groups`. The
elements under any-grade group (if any), i.e. the grade group equal to
`refs` will be moved to the end. Grade groups names must be unique.

## Usage

``` r
h_append_grade_groups(
  grade_groups,
  refs,
  remove_single = TRUE,
  only_grade_groups = FALSE
)
```

## Arguments

- grade_groups:

  (named `list` of `character`)  
  list containing groupings of grades.

- refs:

  (named `list` of `numeric`)  
  named list where each name corresponds to a reference grade level and
  each entry represents a count.

- remove_single:

  (`flag`)  
  `TRUE` to not include the elements of one-element grade groups in the
  the output list; in this case only the grade groups names will be
  included in the output. If `only_grade_groups` is set to `TRUE` this
  argument is ignored.

- only_grade_groups:

  (`flag`)  
  whether only the specified grade groups should be included, with
  individual grade rows removed (`TRUE`), or all grades and grade groups
  should be displayed (`FALSE`).

## Value

Formatted list of grade groupings.

## Examples

``` r
h_append_grade_groups(
  list(
    "Any Grade" = as.character(1:5),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4")
  ),
  list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50)
)
#> $`Any Grade`
#> [1] 150
#> 
#> $`Grade 1-2`
#> [1] 30
#> 
#> $`1`
#> [1] 10
#> 
#> $`2`
#> [1] 20
#> 
#> $`Grade 3-4`
#> [1] 70
#> 
#> $`3`
#> [1] 30
#> 
#> $`4`
#> [1] 40
#> 
#> $`5`
#> [1] 50
#> 

h_append_grade_groups(
  list(
    "Any Grade" = as.character(5:1),
    "Grade A" = "5",
    "Grade B" = c("4", "3")
  ),
  list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50)
)
#> $`Any Grade`
#> [1] 150
#> 
#> $`Grade A`
#> [1] 50
#> 
#> $`Grade B`
#> [1] 70
#> 
#> $`4`
#> [1] 40
#> 
#> $`3`
#> [1] 30
#> 
#> $`2`
#> [1] 20
#> 
#> $`1`
#> [1] 10
#> 

h_append_grade_groups(
  list(
    "Any Grade" = as.character(1:5),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4")
  ),
  list("1" = 10, "2" = 5, "3" = 0)
)
#> $`Any Grade`
#> [1] 15
#> 
#> $`Grade 1-2`
#> [1] 15
#> 
#> $`1`
#> [1] 10
#> 
#> $`2`
#> [1] 5
#> 
#> $`Grade 3-4`
#> [1] 0
#> 
#> $`3`
#> [1] 0
#> 
```
