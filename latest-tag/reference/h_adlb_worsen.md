# Helper function to prepare ADLB with worst labs

**\[stable\]**

Helper function to prepare a `df` for generate the patient count shift
table.

## Usage

``` r
h_adlb_worsen(
  adlb,
  worst_flag_low = NULL,
  worst_flag_high = NULL,
  direction_var
)
```

## Arguments

- adlb:

  (`data.frame`)  
  ADLB data frame.

- worst_flag_low:

  (named `vector`)  
  worst low post-baseline lab grade flag variable. See how this is
  implemented in the following examples.

- worst_flag_high:

  (named `vector`)  
  worst high post-baseline lab grade flag variable. See how this is
  implemented in the following examples.

- direction_var:

  (`string`)  
  name of the direction variable specifying the direction of the shift
  table of interest. Only lab records flagged by `L`, `H` or `B` are
  included in the shift table.

  - `L`: low direction only

  - `H`: high direction only

  - `B`: both low and high directions

## Value

`h_adlb_worsen()` returns the `adlb` `data.frame` containing only the
worst labs specified according to `worst_flag_low` or `worst_flag_high`
for the direction specified according to `direction_var`. For instance,
for a lab that is needed for the low direction only, only records
flagged by `worst_flag_low` are selected. For a lab that is needed for
both low and high directions, the worst low records are selected for the
low direction, and the worst high record are selected for the high
direction.

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
```
