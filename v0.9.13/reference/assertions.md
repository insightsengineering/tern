# Additional assertions to use with `checkmate`

**\[stable\]**

Additional assertion functions which can be used together with the
`checkmate` package.

## Usage

``` r
assert_list_of_variables(x, .var.name = checkmate::vname(x), add = NULL)

assert_df_with_variables(
  df,
  variables,
  na_level = NULL,
  .var.name = checkmate::vname(df),
  add = NULL
)

assert_valid_factor(
  x,
  min.levels = 1,
  max.levels = NULL,
  null.ok = TRUE,
  any.missing = TRUE,
  n.levels = NULL,
  len = NULL,
  .var.name = checkmate::vname(x),
  add = NULL
)

assert_df_with_factors(
  df,
  variables,
  min.levels = 1,
  max.levels = NULL,
  any.missing = TRUE,
  na_level = NULL,
  .var.name = checkmate::vname(df),
  add = NULL
)

assert_proportion_value(x, include_boundaries = FALSE)

assert_proportion_data(rsp, grp, strata = NULL)

assert_stratification_compatibility(method, stratified_methods, strata_vars)
```

## Arguments

- x:

  (`any`)\
  object to test.

- .var.name:

  \[`character(1)`\]\
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]\
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

- df:

  (`data.frame`)\
  data set to test.

- variables:

  (named `list` of `character`)\
  list of variables to test.

- na_level:

  (`string`)\
  the string you have been using to represent NA or missing data. For
  `NA` values please consider using directly
  [`is.na()`](https://rdrr.io/r/base/NA.html) or similar approaches.

- min.levels:

  \[`integer(1)`\]\
  Minimum number of factor levels. Default is `NULL` (no check).

- max.levels:

  \[`integer(1)`\]\
  Maximum number of factor levels. Default is `NULL` (no check).

- null.ok:

  \[`logical(1)`\]\
  If set to `TRUE`, `x` may also be `NULL`. In this case only a type
  check of `x` is performed, all additional checks are disabled.

- any.missing:

  \[`logical(1)`\]\
  Are vectors with missing values allowed? Default is `TRUE`.

- n.levels:

  \[`integer(1)`\]\
  Exact number of factor levels. Default is `NULL` (no check).

- len:

  \[`integer(1)`\]\
  Exact expected length of `x`.

- include_boundaries:

  (`flag`)\
  whether to include boundaries when testing for proportions.

- rsp:

  (`logical`)\
  Indicates whether each observation is a responder (`TRUE`) or a
  non-responder (`FALSE`). Missing values are not allowed.

- grp:

  (`factor`)\
  Assigns each observation to one of two groups, such as a reference and
  a treatment group. Must have exactly two levels and the same length as
  `rsp`. Missing values are not allowed.

- strata:

  (`factor` or `NULL`)\
  Defines the stratification variable. If not `NULL`, it must have the
  same length as `rsp` and must not contain any missing values.

- method:

  (`character(1)`)\
  Specifies the statistical method.

- stratified_methods:

  (`character`)\
  Names of the methods that require stratified data.

- strata_vars:

  (`character` or `NULL`)\
  Names of the variables defining the strata, or `NULL` if no
  stratification is used.

## Value

Nothing if assertion passes, otherwise prints the error message.

## Functions

- `assert_list_of_variables()`: Checks whether `x` is a valid list of
  variable names. `NULL` elements of the list `x` are dropped with
  `Filter(Negate(is.null), x)`.

- `assert_df_with_variables()`: Check whether `df` is a data frame with
  the analysis `variables`. Please notice how this produces an error
  when not all variables are present in the data.frame while the
  opposite is not required.

- `assert_valid_factor()`: Check whether `x` is a valid factor (i.e. has
  levels and no empty string levels). Note that `NULL` and `NA` elements
  are allowed.

- `assert_df_with_factors()`: Check whether `df` is a data frame where
  the analysis `variables` are all factors. Note that the creation of
  `NA` by direct call of
  [`factor()`](https://rdrr.io/r/base/factor.html) will trim `NA` levels
  out of the vector list itself.

- `assert_proportion_value()`: Check whether `x` is a proportion: number
  between 0 and 1.

- `assert_proportion_data()`: Validates the data required for a
  proportion analysis, including responder status, group assignment, and
  optional stratification.

- `assert_stratification_compatibility()`: Assert compatibility between
  a method and stratification. Checks that the selected method is
  compatible with the presence or absence of stratification variables.
  Methods included in `stratified_methods` require at least one variable
  defining the strata, while unstratified methods must not use
  stratification variables.

## Examples

``` r
x <- data.frame(
  a = 1:10,
  b = rnorm(10)
)
assert_df_with_variables(x, variables = list(a = "a", b = "b"))

x <- ex_adsl
assert_df_with_variables(x, list(a = "ARM", b = "USUBJID"))

x <- ex_adsl
assert_df_with_factors(x, list(a = "ARM"))

assert_proportion_value(0.95)
assert_proportion_value(1.0, include_boundaries = TRUE)

rsp <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
grp <- factor(c(rep("Placebo", 3), rep("X", 3)))
strata <- factor(c("A", "A", "B", "A", "B", "B"))

assert_proportion_data(rsp, grp, strata)

if (FALSE) { # \dontrun{
# An error is raised when `grp` has only one level.
grp <- factor(rep("X", 6))
assert_proportion_data(rsp, grp, strata)
} # }
```
