# Convert `rtable` objects to `ggplot` objects

**\[experimental\]**

Given a
[`rtables::rtable()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rtable.html)
object, performs basic conversion to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object built using functions from the `ggplot2` package. Any table
titles and/or footnotes are ignored.

## Usage

``` r
rtable2gg(tbl, fontsize = 12, colwidths = NULL, lbl_col_padding = 0)
```

## Arguments

- tbl:

  (`VTableTree`)  
  `rtables` table object.

- fontsize:

  (`numeric(1)`)  
  font size.

- colwidths:

  (`numeric` or `NULL`)  
  a vector of column widths. Each element's position in `colwidths`
  corresponds to the column of `tbl` in the same position. If `NULL`,
  column widths are calculated according to maximum number of characters
  per column.

- lbl_col_padding:

  (`numeric`)  
  additional padding to use when calculating spacing between the first
  (label) column and the second column of `tbl`. If `colwidths` is
  specified, the width of the first column becomes
  `colwidths[1] + lbl_col_padding`. Defaults to 0.

## Value

A `ggplot` object.

## Examples

``` r
dta <- data.frame(
  ARM     = rep(LETTERS[1:3], rep(6, 3)),
  AVISIT  = rep(paste0("V", 1:3), 6),
  AVAL    = c(9:1, rep(NA, 9))
)

lyt <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by(var = "AVISIT") %>%
  analyze_vars(vars = "AVAL")

tbl <- build_table(lyt, df = dta)

rtable2gg(tbl)


rtable2gg(tbl, fontsize = 15, colwidths = c(2, 1, 1, 1))

```
