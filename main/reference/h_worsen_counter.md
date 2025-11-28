# Helper function to analyze patients for `s_count_abnormal_lab_worsen_by_baseline()`

**\[stable\]**

Helper function to count the number of patients and the fraction of
patients according to highest post-baseline lab grade variable `.var`,
baseline lab grade variable `baseline_var`, and the direction of
interest specified in `direction_var`.

## Usage

``` r
h_worsen_counter(df, id, .var, baseline_var, direction_var)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- id:

  (`string`)  
  subject variable name.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- baseline_var:

  (`string`)  
  name of the baseline lab grade variable.

- direction_var:

  (`string`)  
  name of the direction variable specifying the direction of the shift
  table of interest. Only lab records flagged by `L`, `H` or `B` are
  included in the shift table.

  - `L`: low direction only

  - `H`: high direction only

  - `B`: both low and high directions

## Value

The counts and fraction of patients whose worst post-baseline lab grades
are worse than their baseline grades, for post-baseline worst grades
"1", "2", "3", "4" and "Any".

## See also

[abnormal_lab_worsen_by_baseline](https://insightsengineering.github.io/tern/reference/abnormal_lab_worsen_by_baseline.md)

## Examples

``` r
library(dplyr)

# The direction variable, GRADDR, is based on metadata
adlb <- tern_ex_adlb %>%
  mutate(
    GRADDR = case_when(
      PARAMCD == "ALT" ~ "B",
      PARAMCD == "CRP" ~ "L",
      PARAMCD == "IGA" ~ "H"
    )
  ) %>%
  filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

df <- h_adlb_worsen(
  adlb,
  worst_flag_low = c("WGRLOFL" = "Y"),
  worst_flag_high = c("WGRHIFL" = "Y"),
  direction_var = "GRADDR"
)

# `h_worsen_counter`
h_worsen_counter(
  df %>% filter(PARAMCD == "CRP" & GRADDR == "Low"),
  id = "USUBJID",
  .var = "ATOXGR",
  baseline_var = "BTOXGR",
  direction_var = "GRADDR"
)
#> $fraction
#> $fraction$`1`
#>   num denom 
#>    25   180 
#> 
#> $fraction$`2`
#>   num denom 
#>    15   186 
#> 
#> $fraction$`3`
#>   num denom 
#>    18   191 
#> 
#> $fraction$`4`
#>   num denom 
#>    17   196 
#> 
#> $fraction$Any
#>   num denom 
#>    75   196 
#> 
#> 
```
