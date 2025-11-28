# Create a viewport tree for the forest plot

**\[deprecated\]**

## Usage

``` r
forest_viewport(
  tbl,
  width_row_names = NULL,
  width_columns = NULL,
  width_forest = grid::unit(1, "null"),
  gap_column = grid::unit(1, "lines"),
  gap_header = grid::unit(1, "lines"),
  mat_form = NULL
)
```

## Arguments

- tbl:

  (`VTableTree`)  
  `rtables` table object.

- width_row_names:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width of row names.

- width_columns:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width of column spans.

- width_forest:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  width of the forest plot.

- gap_column:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  gap width between the columns.

- gap_header:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  gap width between the header.

- mat_form:

  (`MatrixPrintForm`)  
  matrix print form of the table.

## Value

A viewport tree.

## Examples

``` r
library(grid)

tbl <- rtable(
  header = rheader(
    rrow("", "E", rcell("CI", colspan = 2)),
    rrow("", "A", "B", "C")
  ),
  rrow("row 1", 1, 0.8, 1.1),
  rrow("row 2", 1.4, 0.8, 1.6),
  rrow("row 3", 1.2, 0.8, 1.2)
)

# \donttest{
v <- forest_viewport(tbl)
#> Warning: `forest_viewport()` was deprecated in tern 0.9.4.
#> ℹ `g_forest` now generates `ggplot` objects. This function is no longer used
#>   within `tern`.
#> Warning: `vp_forest_table_part()` was deprecated in tern 0.9.4.
#> ℹ `g_forest` now generates `ggplot` objects. This function is no longer used
#>   within `tern`.
#> ℹ The deprecated feature was likely used in the tern package.
#>   Please report the issue at
#>   <https://github.com/insightsengineering/tern/issues>.

grid::grid.newpage()
showViewport(v)

# }
```
