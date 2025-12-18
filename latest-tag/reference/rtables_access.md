# Helper functions for accessing information from `rtables`

**\[stable\]**

These are a couple of functions that help with accessing the data in
`rtables` objects. Currently these work for occurrence tables, which are
defined as having a count as the first element and a fraction as the
second element in each cell.

## Usage

``` r
h_row_first_values(table_row, col_names = NULL, col_indices = NULL)

h_row_counts(table_row, col_names = NULL, col_indices = NULL)

h_row_fractions(table_row, col_names = NULL, col_indices = NULL)

h_col_counts(table, col_names = NULL, col_indices = NULL)

h_content_first_row(table)

is_leaf_table(table)

check_names_indices(table_row, col_names = NULL, col_indices = NULL)
```

## Arguments

- table_row:

  (`TableRow`)  
  an analysis row in a occurrence table.

- col_names:

  (`character`)  
  the names of the columns to extract from.

- col_indices:

  (`integer`)  
  the indices of the columns to extract from. If `col_names` are
  provided, then these are inferred from the names of `table_row`. Note
  that this currently only works well with a single column split.

- table:

  (`VTableNodeInfo`)  
  an occurrence table or row.

## Value

- `h_row_first_values()` returns a `vector` of numeric values.

&nbsp;

- `h_row_counts()` returns a `vector` of numeric values.

&nbsp;

- `h_row_fractions()` returns a `vector` of proportions.

&nbsp;

- `h_col_counts()` returns a `vector` of column counts.

&nbsp;

- `h_content_first_row()` returns a row from an `rtables` table.

&nbsp;

- `is_leaf_table()` returns a `logical` value indicating whether current
  table is a leaf.

&nbsp;

- `check_names_indices` returns column indices.

## Functions

- `h_row_first_values()`: Helper function to extract the first values
  from each content cell and from specified columns in a `TableRow`.
  Defaults to all columns.

- `h_row_counts()`: Helper function that extracts row values and checks
  if they are convertible to integers (`integerish` values).

- `h_row_fractions()`: Helper function to extract fractions from
  specified columns in a `TableRow`. More specifically it extracts the
  second values from each content cell and checks it is a fraction.

- `h_col_counts()`: Helper function to extract column counts from
  specified columns in a table.

- `h_content_first_row()`: Helper function to get first row of content
  table of current table.

- `is_leaf_table()`: Helper function which says whether current table is
  a leaf in the tree.

- `check_names_indices()`: Internal helper function that tests standard
  inputs for column indices.

## See also

[prune_occurrences](https://insightsengineering.github.io/tern/reference/prune_occurrences.md)
for usage of these functions.

## Examples

``` r
tbl <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  analyze("AGE", function(x) {
    list(
      "mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.x (xx.x)"),
      "n" = length(x),
      "frac" = rcell(c(0.1, 0.1), format = "xx (xx)")
    )
  }) %>%
  build_table(tern_ex_adsl) %>%
  prune_table()
tree_row_elem <- collect_leaves(tbl[2, ])[[1]]
result <- max(h_row_first_values(tree_row_elem))
result
#> [1] 35.31214

# Row counts (integer values)
# h_row_counts(tree_row_elem) # Fails because there are no integers
# Using values with integers
tree_row_elem <- collect_leaves(tbl[3, ])[[1]]
result <- h_row_counts(tree_row_elem)
# result

# Row fractions
tree_row_elem <- collect_leaves(tbl[4, ])[[1]]
h_row_fractions(tree_row_elem)
#>      A: Drug X     B: Placebo C: Combination 
#>            0.1            0.1            0.1 
```
