# Convert table into matrix of strings

**\[stable\]**

Helper function to use mostly within tests. `with_spaces`parameter
allows to test not only for content but also indentation and table
structure. `print_txt_to_copy` instead facilitate the testing
development by returning a well formatted text that needs only to be
copied and pasted in the expected output.

## Usage

``` r
to_string_matrix(
  x,
  widths = NULL,
  max_width = NULL,
  hsep = formatters::default_hsep(),
  with_spaces = TRUE,
  print_txt_to_copy = FALSE
)
```

## Arguments

- x:

  (`VTableTree`)  
  `rtables` table object.

- widths:

  (`numeric` or `NULL`)  
  Proposed widths for the columns of `x`. The expected length of this
  numeric vector can be retrieved with `ncol(x) + 1` as the column of
  row names must also be considered.

- max_width:

  (`integer(1)`, `string` or `NULL`)  
  width that title and footer (including footnotes) materials should be
  word-wrapped to. If `NULL`, it is set to the current print width of
  the session (`getOption("width")`). If set to `"auto"`, the width of
  the table (plus any table inset) is used. Parameter is ignored if
  `tf_wrap = FALSE`.

- hsep:

  (`string`)  
  character to repeat to create header/body separator line. If `NULL`,
  the object value will be used. If `" "`, an empty separator will be
  printed. See
  [`default_hsep()`](https://insightsengineering.github.io/formatters/latest-tag/reference/default_horizontal_sep.html)
  for more information.

- with_spaces:

  (`flag`)  
  whether the tested table should keep the indentation and other
  relevant spaces.

- print_txt_to_copy:

  (`flag`)  
  utility to have a way to copy the input table directly into the
  expected variable instead of copying it too manually.

## Value

A `matrix` of `string`s. If `print_txt_to_copy = TRUE` the well
formatted printout of the table will be printed to console, ready to be
copied as a expected value.

## Examples

``` r
tbl <- basic_table() %>%
  split_rows_by("SEX") %>%
  split_cols_by("ARM") %>%
  analyze("AGE") %>%
  build_table(tern_ex_adsl)

to_string_matrix(tbl, widths = ceiling(propose_column_widths(tbl) / 2))
#>  [1] "       A:                    " "      Drug    B: Pl   C: Comb"
#>  [3] "        X     acebo   ination" "—————————————————————————————"
#>  [5] "F                            " "  M   33.68   35.98    36.02 "
#>  [7] "  e                          " "  a                          "
#>  [9] "  n                          " "M                            "
#> [11] "  M   34.70   35.68    36.28 " "  e                          "
#> [13] "  a                          " "  n                          "
```
