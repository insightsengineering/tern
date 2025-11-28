# Control function for `g_lineplot()`

**\[stable\]**

Default values for `variables` parameter in `g_lineplot` function. A
variable's default value can be overwritten for any variable.

## Usage

``` r
control_lineplot_vars(
  x = "AVISIT",
  y = "AVAL",
  group_var = "ARM",
  facet_var = NA,
  paramcd = "PARAMCD",
  y_unit = "AVALU",
  subject_var = "USUBJID"
)
```

## Arguments

- x:

  (`string`)  
  x-variable name.

- y:

  (`string`)  
  y-variable name.

- group_var:

  (`string` or `NA`)  
  group variable name.

- facet_var:

  (`string` or `NA`)  
  faceting variable name.

- paramcd:

  (`string` or `NA`)  
  parameter code variable name.

- y_unit:

  (`string` or `NA`)  
  y-axis unit variable name.

- subject_var:

  (`string` or `NA`)  
  subject variable name.

## Value

A named character vector of variable names.

## Examples

``` r
control_lineplot_vars()
#>           x           y   group_var     paramcd      y_unit subject_var 
#>    "AVISIT"      "AVAL"       "ARM"   "PARAMCD"     "AVALU"   "USUBJID" 
#>   facet_var 
#>          NA 
control_lineplot_vars(group_var = NA)
#>           x           y   group_var     paramcd      y_unit subject_var 
#>    "AVISIT"      "AVAL"          NA   "PARAMCD"     "AVALU"   "USUBJID" 
#>   facet_var 
#>          NA 
```
