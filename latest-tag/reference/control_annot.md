# Control functions for Kaplan-Meier plot annotation tables

**\[stable\]**

Auxiliary functions for controlling arguments for formatting the
annotation tables that can be added to plots generated via
[`g_km()`](https://insightsengineering.github.io/tern/reference/g_km.md).

## Usage

``` r
control_surv_med_annot(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE)

control_coxph_annot(
  x = 0.29,
  y = 0.51,
  w = 0.4,
  h = 0.125,
  fill = TRUE,
  ref_lbls = FALSE
)
```

## Arguments

- x:

  (`proportion`)  
  x-coordinate for center of annotation table.

- y:

  (`proportion`)  
  y-coordinate for center of annotation table.

- w:

  (`proportion`)  
  relative width of the annotation table.

- h:

  (`proportion`)  
  relative height of the annotation table.

- fill:

  (`flag` or `character`)  
  whether the annotation table should have a background fill color. Can
  also be a color code to use as the background fill color. If `TRUE`,
  color code defaults to `"#00000020"`.

- ref_lbls:

  (`flag`)  
  whether the reference group should be explicitly printed in labels for
  the annotation table. If `FALSE` (default), only comparison groups
  will be printed in the table labels.

## Value

A list of components with the same names as the arguments.

## Functions

- `control_surv_med_annot()`: Control function for formatting the median
  survival time annotation table. This annotation table can be added in
  [`g_km()`](https://insightsengineering.github.io/tern/reference/g_km.md)
  by setting `annot_surv_med=TRUE`, and can be configured using the
  `control_surv_med_annot()` function by setting it as the
  `control_annot_surv_med` argument.

- `control_coxph_annot()`: Control function for formatting the Cox-PH
  annotation table. This annotation table can be added in
  [`g_km()`](https://insightsengineering.github.io/tern/reference/g_km.md)
  by setting `annot_coxph=TRUE`, and can be configured using the
  `control_coxph_annot()` function by setting it as the
  `control_annot_coxph` argument.

## See also

[`g_km()`](https://insightsengineering.github.io/tern/reference/g_km.md)

## Examples

``` r
control_surv_med_annot()
#> $x
#> [1] 0.8
#> 
#> $y
#> [1] 0.85
#> 
#> $w
#> [1] 0.32
#> 
#> $h
#> [1] 0.16
#> 
#> $fill
#> [1] TRUE
#> 

control_coxph_annot()
#> $x
#> [1] 0.29
#> 
#> $y
#> [1] 0.51
#> 
#> $w
#> [1] 0.4
#> 
#> $h
#> [1] 0.125
#> 
#> $fill
#> [1] TRUE
#> 
#> $ref_lbls
#> [1] FALSE
#> 
```
