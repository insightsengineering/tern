# Description of standard oncology response

**\[stable\]**

Describe the oncology response in a standard way.

## Usage

``` r
d_onco_rsp_label(x)
```

## Arguments

- x:

  (`character`)  
  the standard oncology codes to be described.

## Value

Response labels.

## See also

[`estimate_multinomial_rsp()`](https://insightsengineering.github.io/tern/reference/estimate_multinomial_rsp.md)

## Examples

``` r
d_onco_rsp_label(
  c("CR", "PR", "SD", "NON CR/PD", "PD", "NE", "Missing", "<Missing>", "NE/Missing")
)
#>                           CR                           PR 
#>       Complete Response (CR)        Partial Response (PR) 
#>                           SD                    NON CR/PD 
#>          Stable Disease (SD) Non-CR or Non-PD (NON CR/PD) 
#>                           PD                           NE 
#>     Progressive Disease (PD)           Not Evaluable (NE) 
#>                      Missing                    <Missing> 
#>                      Missing                    <Missing> 
#>                   NE/Missing 
#>       Missing or unevaluable 
#> 9 Levels: Complete Response (CR) Partial Response (PR) ... <Missing>

# Adding some values not considered in d_onco_rsp_label

d_onco_rsp_label(
  c("CR", "PR", "hello", "hi")
)
#>                     CR                     PR                  hello 
#> Complete Response (CR)  Partial Response (PR)                  hello 
#>                     hi 
#>                     hi 
#> Levels: Complete Response (CR) Partial Response (PR) hello hi
```
