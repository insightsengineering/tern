# Factor utilities

**\[stable\]**

A collection of utility functions for factors.

## Usage

``` r
combine_levels(x, levels, new_level = paste(levels, collapse = "/"))

as_factor_keep_attributes(
  x,
  x_name = deparse(substitute(x)),
  na_level = "<Missing>",
  verbose = TRUE
)

fct_discard(x, discard)

fct_explicit_na_if(x, condition, na_level = "<Missing>")

fct_collapse_only(.f, ..., .na_level = "<Missing>")
```

## Arguments

- x:

  (`factor`)  
  factor variable or object to convert (for
  `as_factor_keep_attributes`).

- levels:

  (`character`)  
  level names to be combined.

- new_level:

  (`string`)  
  name of new level.

- x_name:

  (`string`)  
  name of `x`.

- na_level:

  (`string`)  
  which level to use for missing values.

- verbose:

  (`flag`)  
  defaults to `TRUE`. It prints out warnings and messages.

- discard:

  (`character`)  
  levels to discard.

- condition:

  (`logical`)  
  positions at which to insert missing values.

- .f:

  (`factor` or `character`)  
  original vector.

- ...:

  (named `character`)  
  levels in each vector provided will be collapsed into the new level
  given by the respective name.

- .na_level:

  (`string`)  
  which level to use for other levels, which should be missing in the
  new factor. Note that this level must not be contained in the new
  levels specified in `...`.

## Value

- `combine_levels`: A `factor` with the new levels.

&nbsp;

- `as_factor_keep_attributes`: A `factor` with same attributes (except
  class) as `x`. Does not modify `x` if already a `factor`.

&nbsp;

- `fct_discard`: A modified `factor` with observations as well as levels
  from `discard` dropped.

&nbsp;

- `fct_explicit_na_if`: A modified `factor` with inserted and existing
  `NA` converted to `na_level`.

&nbsp;

- `fct_collapse_only`: A modified `factor` with collapsed levels. Values
  and levels which are not included in the given `character` vector
  input will be set to the missing level `.na_level`.

## Functions

- `combine_levels()`: Combine specified old factor Levels in a single
  new level.

- `as_factor_keep_attributes()`: Converts `x` to a factor and keeps its
  attributes. Warns appropriately such that the user can decide whether
  they prefer converting to factor manually (e.g. for full control of
  factor levels).

- `fct_discard()`: This discards the observations as well as the levels
  specified from a factor.

- `fct_explicit_na_if()`: This inserts explicit missing values in a
  factor based on a condition. Additionally, existing `NA` values will
  be explicitly converted to given `na_level`.

- `fct_collapse_only()`: This collapses levels and only keeps those new
  group levels, in the order provided. The returned factor has levels in
  the order given, with the possible missing level last (this will only
  be included if there are missing values).

## Note

Any existing `NA`s in the input vector will not be replaced by the
missing level. If needed,
[`explicit_na()`](https://insightsengineering.github.io/tern/reference/explicit_na.md)
can be called separately on the result.

## See also

[`cut_quantile_bins()`](https://insightsengineering.github.io/tern/reference/cut_quantile_bins.md)
for splitting numeric vectors into quantile bins.

[`forcats::fct_na_value_to_level()`](https://forcats.tidyverse.org/reference/fct_na_value_to_level.html)
which is used internally.

[`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html),
[`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
which are used internally.

## Examples

``` r
x <- factor(letters[1:5], levels = letters[5:1])
combine_levels(x, levels = c("a", "b"))
#> [1] a/b a/b c   d   e  
#> Levels: e d c a/b

combine_levels(x, c("e", "b"))
#> [1] a   e/b c   d   e/b
#> Levels: e/b d c a

a_chr_with_labels <- c("a", "b", NA)
attr(a_chr_with_labels, "label") <- "A character vector with labels"
as_factor_keep_attributes(a_chr_with_labels)
#> Warning: automatically converting character variable a_chr_with_labels to factor, better manually convert to factor to avoid failures
#> [1] a         b         <Missing>
#> attr(,"label")
#> [1] A character vector with labels
#> Levels: a b <Missing>

fct_discard(factor(c("a", "b", "c")), "c")
#> [1] a b
#> Levels: a b

fct_explicit_na_if(factor(c("a", "b", NA)), c(TRUE, FALSE, FALSE))
#> [1] <Missing> b         <Missing>
#> Levels: a b <Missing>

fct_collapse_only(factor(c("a", "b", "c", "d")), TRT = "b", CTRL = c("c", "d"))
#> [1] <Missing> TRT       CTRL      CTRL     
#> Levels: TRT CTRL <Missing>
```
