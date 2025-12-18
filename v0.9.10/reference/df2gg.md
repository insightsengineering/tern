# Convert `data.frame` object to `ggplot` object

**\[experimental\]**

Given a `data.frame` object, performs basic conversion to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object built using functions from the `ggplot2` package.

## Usage

``` r
df2gg(
  df,
  colwidths = NULL,
  font_size = 10,
  col_labels = TRUE,
  col_lab_fontface = "bold",
  hline = TRUE,
  bg_fill = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  a data frame.

- colwidths:

  (`numeric` or `NULL`)  
  a vector of column widths. Each element's position in `colwidths`
  corresponds to the column of `df` in the same position. If `NULL`,
  column widths are calculated according to maximum number of characters
  per column.

- font_size:

  (`numeric(1)`)  
  font size.

- col_labels:

  (`flag`)  
  whether the column names (labels) of `df` should be used as the first
  row of the output table.

- col_lab_fontface:

  (`string`)  
  font face to apply to the first row (of column labels if
  `col_labels = TRUE`). Defaults to `"bold"`.

- hline:

  (`flag`)  
  whether a horizontal line should be printed below the first row of the
  table.

- bg_fill:

  (`string`)  
  table background fill color.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
df2gg(head(iris, 5))

df2gg(head(iris, 5), font_size = 15, colwidths = c(1, 1, 1, 1, 1))
} # }
```
