# Sort pharmacokinetic data by `PARAM` variable

**\[stable\]**

## Usage

``` r
h_pkparam_sort(pk_data, key_var = "PARAMCD")
```

## Arguments

- pk_data:

  (`data.frame`)  
  pharmacokinetic data frame.

- key_var:

  (`string`)  
  key variable used to merge pk_data and metadata created by
  [`d_pkparam()`](https://insightsengineering.github.io/tern/reference/d_pkparam.md).

## Value

A pharmacokinetic `data.frame` sorted by a `PARAM` variable.

## Examples

``` r
library(dplyr)

adpp <- tern_ex_adpp %>% mutate(PKPARAM = factor(paste0(PARAM, " (", AVALU, ")")))
pk_ordered_data <- h_pkparam_sort(adpp)
```
