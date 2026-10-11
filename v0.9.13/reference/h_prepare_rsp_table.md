# Helper Function to Prepare Data for Proportion Analyses

**\[experimental\]**

Prepares response, group, and optional strata vectors, and constructs a
2 x 2 contingency table for proportion-based analyses. The function
extracts the variables from an analysis dataset and, optionally, a
reference dataset, combines them into vectors suitable for downstream
statistical functions, and returns the resulting contingency table
together with the prepared vectors.

## Usage

``` r
h_prepare_rsp_table(
  df,
  df_ref = NULL,
  var,
  val = TRUE,
  strata_vars = NULL,
  complete_cases = FALSE,
  quiet = FALSE
)
```

## Arguments

- df:

  (`data.frame`)\
  A data frame containing the observations for the non-reference group.

- df_ref:

  (`data.frame` or `NULL`)\
  An optional data frame containing the observations for the reference
  group. Columns specified by `var` and `strata_vars` (if not `NULL`)
  must have the same classes as the corresponding columns in `df`. If
  they are factors, their levels must also be identical between `df` and
  `df_ref`.

- var:

  (`character(1)`)\
  The column name in `df` (and, if supplied, `df_ref`) specifying the
  response variable. The response is converted to a logical vector by
  comparing its values with `val`. `df[[var]]` (and `df_ref[[var]]`, if
  supplied) must be an atomic vector as defined by
  [`checkmate::check_atomic_vector()`](https://mllg.github.io/checkmate/reference/checkAtomicVector.html),
  of one of the following types: `logical`, `integer`, `numeric`, or
  `character`.

- val:

  (`logical(1)` or `integer(1)` or `numeric(1)` or `character(1)`)\
  The value in `df[[var]]` (and, if supplied, in `df_ref[[var]]`) that
  defines a positive response. Observations matching this value are
  returned as `TRUE` in the `rsp` vector; all other observations are
  returned as `FALSE`. If `df[[var]]` is a factor, `val` must be a
  character value matching one of its levels. Otherwise, `val` must have
  the same class as `df[[var]]`.

- strata_vars:

  (`character` or `NULL`)\
  Optional column names in `df` (and, if supplied, `df_ref`) specifying
  the strata variables. The specified columns must all be factors.

- complete_cases:

  (`logical(1)`)\
  Whether to remove incomplete rows from `df` (and, if supplied,
  `df_ref`) before constructing the response, group, strata, and
  contingency table. Completeness is assessed only for the columns
  specified by `var` and `strata_vars` (if supplied) using
  [`get_complete_cases()`](https://pharmaverse.github.io/tern/reference/get_complete_cases.md).
  If `complete_cases = TRUE`, rows containing missing values in any of
  these columns are removed. If `complete_cases = FALSE`, the function
  fails if any of these columns contain missing values, rather than
  returning results containing `NA` values.

- quiet:

  (`logical(1)`)\
  Passed to
  [`get_complete_cases()`](https://pharmaverse.github.io/tern/reference/get_complete_cases.md).
  If `complete_cases = TRUE`, controls whether a message is displayed
  for rows removed due to missing values. Has no effect when
  `complete_cases = FALSE`.

## Value

A named `list` containing:

- `rsp`:

  A logical vector indicating whether each observation has the value
  specified by `val` in `df[[var]]` (and, if supplied, `df_ref`).

- `grp`:

  A factor identifying the group of each observation. The levels are
  always `"ref"` and `"Not-ref"` (in this order), corresponding to
  observations from `df_ref` and `df`, respectively. If `df_ref` is
  `NULL`, all observations belong to `"Not-ref"`.

- `strata`:

  A factor defining the analysis strata when `strata_vars` is supplied,
  or `NULL` otherwise. When multiple stratification variables are
  supplied, their combinations, using
  [`interaction()`](https://rdrr.io/r/base/interaction.html), are used
  to define the strata.

- `tbl`:

  A contingency table produced by
  [`base::table()`](https://rdrr.io/r/base/table.html) from `grp`, `rsp`
  (after converting it to a factor with two levels, `"TRUE"` and
  `"FALSE"`), and, if `strata_vars` is supplied, `strata`. When
  `strata_vars` is `NULL`, a 2 x 2 table is returned, with `grp`
  defining the rows and `rsp` defining the columns. The `rsp` dimension
  always contains the levels `"TRUE"` and `"FALSE"`, in that order, even
  when one or both response outcomes are not observed. When
  `strata_vars` is supplied, a 3-dimensional contingency table with
  dimensions 2 x 2 x k is returned, where k is the number of strata. The
  dimensions correspond to `grp`, `rsp`, and `strata`, respectively.

## Details

The function prepares the response, group, and optional strata variables
required for proportion-based analyses and constructs a safe contingency
table for downstream statistical functions. See the proportion
difference documentation
[h_prop_diff](https://pharmaverse.github.io/tern/reference/h_prop_diff.md)
and
[h_prop_diff_test](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md)
for related functions.

If `complete_cases = TRUE`, incomplete observations are removed
separately from `df` and `df_ref` (if supplied) before the vectors and
contingency table are constructed. Completeness is assessed jointly
across the `var` and `strata_vars` columns when `strata_vars` is
supplied, and only across `var` otherwise. This is performed using
[`get_complete_cases()`](https://pharmaverse.github.io/tern/reference/get_complete_cases.md).
If `complete_cases = FALSE`, the function fails if any of these columns
contain missing values, rather than returning results containing `NA`
values.

The response variable specified by `var`, and optionally the strata
variables specified by `strata_vars`, are extracted independently from
`df` and `df_ref` (if supplied). The response vectors are then combined
into a single vector and converted to a logical vector by comparing each
value with `val`, such that observations matching `val` are `TRUE` and
all other observations are `FALSE`.

When multiple stratification variables are provided, their combinations
are collapsed into a single factor using
[`interaction()`](https://rdrr.io/r/base/interaction.html). This is done
independently for `df` and `df_ref` (if supplied), after which the
resulting strata vectors are combined into a single factor.

A group factor is constructed to identify the source of each
observation. Observations from `df` are assigned to the `"Not-ref"`
group, while observations from `df_ref` are assigned to the `"ref"`
group. The factor always has `"ref"` and `"Not-ref"` as its levels, in
this order. The level order is important because proportion-difference
calculations in `tern` use the first level as the reference group and
calculate the difference as `"Not-ref"` - `"ref"`.

The contingency table is constructed from group, response (after
converting it to a factor with two levels, `"TRUE"` and `"FALSE"`), and,
if `strata_vars` is supplied, `strata`.

## See also

[h_prop_diff](https://pharmaverse.github.io/tern/reference/h_prop_diff.md),
[h_prop_diff_test](https://pharmaverse.github.io/tern/reference/h_prop_diff_test.md),
[`get_complete_cases()`](https://pharmaverse.github.io/tern/reference/get_complete_cases.md)

## Examples

``` r

set.seed(123)
n <- 28
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), n, TRUE),
  "grp" = sample(c("X", "Placebo"), n, TRUE),
  "f1" = sample(c("a1", "a2"), n, TRUE),
  "f2" = sample(c("x", "y"), n, TRUE),
  stringsAsFactors = TRUE
)
head(dta)
#>     rsp     grp f1 f2
#> 1  TRUE       X a2  y
#> 2  TRUE Placebo a1  y
#> 3  TRUE       X a2  x
#> 4 FALSE Placebo a1  x
#> 5  TRUE       X a1  y
#> 6 FALSE Placebo a2  x

trgs <- h_prepare_rsp_table(
  df = subset(dta, grp == "X"),
  df_ref = subset(dta, grp == "Placebo"),
  var = "rsp",
  strata_vars = c("f1", "f2")
)

rbind(
  subset(dta, grp == "X"),
  subset(dta, grp == "Placebo"),
  make.row.names = FALSE
)
#>      rsp     grp f1 f2
#> 1   TRUE       X a2  y
#> 2   TRUE       X a2  x
#> 3   TRUE       X a1  y
#> 4  FALSE       X a1  y
#> 5   TRUE       X a1  x
#> 6   TRUE       X a2  y
#> 7  FALSE       X a1  y
#> 8  FALSE       X a1  x
#> 9   TRUE       X a1  x
#> 10 FALSE       X a1  x
#> 11  TRUE       X a1  y
#> 12  TRUE       X a1  y
#> 13  TRUE       X a2  y
#> 14 FALSE       X a1  y
#> 15  TRUE       X a2  x
#> 16  TRUE       X a2  x
#> 17  TRUE       X a2  y
#> 18 FALSE       X a1  y
#> 19  TRUE Placebo a1  y
#> 20 FALSE Placebo a1  x
#> 21 FALSE Placebo a2  x
#> 22 FALSE Placebo a2  y
#> 23 FALSE Placebo a1  x
#> 24  TRUE Placebo a1  y
#> 25 FALSE Placebo a2  x
#> 26  TRUE Placebo a2  x
#> 27  TRUE Placebo a2  x
#> 28 FALSE Placebo a2  y

trgs$rsp
#>  [1]  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
#> [13]  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
#> [25]  TRUE  TRUE  TRUE FALSE
trgs$grp
#>  [1] ref     ref     ref     ref     ref     ref     ref     ref     ref    
#> [10] ref     Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref
#> [19] Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref Not-ref
#> [28] Not-ref
#> Levels: ref Not-ref
trgs$strata
#>  [1] a1.y a1.x a2.x a2.y a1.x a1.y a2.x a2.x a2.x a2.y a2.y a2.x a1.y a1.y a1.x
#> [16] a2.y a1.y a1.x a1.x a1.x a1.y a1.y a2.y a1.y a2.x a2.x a2.y a1.y
#> Levels: a1.x a2.x a1.y a2.y
trgs$tbl
#> , , strata = a1.x
#> 
#>          rsp
#> grp       TRUE FALSE
#>   ref        0     2
#>   Not-ref    2     2
#> 
#> , , strata = a2.x
#> 
#>          rsp
#> grp       TRUE FALSE
#>   ref        2     2
#>   Not-ref    3     0
#> 
#> , , strata = a1.y
#> 
#>          rsp
#> grp       TRUE FALSE
#>   ref        2     0
#>   Not-ref    3     4
#> 
#> , , strata = a2.y
#> 
#>          rsp
#> grp       TRUE FALSE
#>   ref        0     2
#>   Not-ref    4     0
#> 

# Example use case.
prop_diff_cmh(trgs$rsp, trgs$grp, trgs$strata)
#> $prop
#>       ref   Not-ref 
#> 0.4064171 0.7379679 
#> 
#> $prop_ci
#> $prop_ci$ref
#> [1] 0.2649224 0.5479118
#> 
#> $prop_ci$`Not-ref`
#> [1] 0.5918892 0.8840466
#> 
#> 
#> $diff
#> [1] 0.3315508
#> 
#> $diff_ci
#> [1] 0.1281798 0.5349218
#> 
#> $se_diff
#> [1] 0.1037626
#> 
#> $weights
#>      a1.x      a2.x      a1.y      a2.y 
#> 0.2245989 0.2887701 0.2620321 0.2245989 
#> 
#> $n1
#> a1.x a2.x a1.y a2.y 
#>    2    4    2    2 
#> 
#> $n2
#> a1.x a2.x a1.y a2.y 
#>    4    3    7    4 
#> 
prop_cmh(trgs$tbl)
#> [1] 0.114052
#> attr(,"z_stat")
#> [1] 1.58024

# The FALSE/TRUE levels are retained even when only one outcome is observed.
dta2 <- dta
dta2$rsp <- TRUE
h_prepare_rsp_table(
  df = subset(dta2, grp == "X"),
  df_ref = subset(dta2, grp == "Placebo"),
  var = "rsp",
)$tbl
#>          rsp
#> grp       TRUE FALSE
#>   ref       10     0
#>   Not-ref   18     0

# Handling missing values.
if (FALSE) { # \dontrun{
dta_missing <- dta
dta_missing[1, "rsp"] <- NA

# By default, the function fails when missing values are present.
h_prepare_rsp_table(
  df = subset(dta_missing, grp == "X"),
  df_ref = subset(dta_missing, grp == "Placebo"),
  var = "rsp",
  strata_vars = c("f1", "f2")
)

# Set complete_cases = TRUE to remove incomplete observations.
h_prepare_rsp_table(
  df = subset(dta_missing, grp == "X"),
  df_ref = subset(dta_missing, grp == "Placebo"),
  var = "rsp",
  strata_vars = c("f1", "f2"),
  complete_cases = TRUE
)
} # }
```
