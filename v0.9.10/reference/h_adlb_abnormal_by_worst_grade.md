# Helper function to prepare ADLB for `count_abnormal_by_worst_grade()`

**\[stable\]**

Helper function to prepare an ADLB data frame to be used as input in
[`count_abnormal_by_worst_grade()`](https://insightsengineering.github.io/tern/reference/abnormal_by_worst_grade.md).
The following pre-processing steps are applied:

1.  `adlb` is filtered on variable `avisit` to only include
    post-baseline visits.

2.  `adlb` is filtered on variables `worst_flag_low` and
    `worst_flag_high` so that only worst grades (in either direction)
    are included.

3.  From the standard lab grade variable `atoxgr`, the following two
    variables are derived and added to `adlb`:

- A grade direction variable (e.g. `GRADE_DIR`). The variable takes
  value `"HIGH"` when `atoxgr > 0`, `"LOW"` when `atoxgr < 0`, and
  `"ZERO"` otherwise.

- A toxicity grade variable (e.g. `GRADE_ANL`) where all negative values
  from `atoxgr` are replaced by their absolute values.

1.  Unused factor levels are dropped from `adlb` via
    [`droplevels()`](https://rdrr.io/r/base/droplevels.html).

## Usage

``` r
h_adlb_abnormal_by_worst_grade(
  adlb,
  atoxgr = "ATOXGR",
  avisit = "AVISIT",
  worst_flag_low = "WGRLOFL",
  worst_flag_high = "WGRHIFL"
)
```

## Arguments

- adlb:

  (`data.frame`)  
  ADLB data frame.

- atoxgr:

  (`string`)  
  name of the analysis toxicity grade variable. This must be a `factor`
  variable.

- avisit:

  (`string`)  
  name of the analysis visit variable.

- worst_flag_low:

  (`string`)  
  name of the worst low lab grade flag variable. This variable is set to
  `"Y"` when indicating records of worst low lab grades.

- worst_flag_high:

  (`string`)  
  name of the worst high lab grade flag variable. This variable is set
  to `"Y"` when indicating records of worst high lab grades.

## Value

`h_adlb_abnormal_by_worst_grade()` returns the `adlb` data frame with
two new variables: `GRADE_DIR` and `GRADE_ANL`.

## See also

[abnormal_by_worst_grade](https://insightsengineering.github.io/tern/reference/abnormal_by_worst_grade.md)

## Examples

``` r
h_adlb_abnormal_by_worst_grade(tern_ex_adlb) %>%
  dplyr::select(ATOXGR, GRADE_DIR, GRADE_ANL) %>%
  head(10)
#> # A tibble: 10 × 3
#>    ATOXGR GRADE_DIR GRADE_ANL
#>    <fct>  <fct>     <fct>    
#>  1 -3     LOW       3        
#>  2 0      ZERO      0        
#>  3 0      ZERO      0        
#>  4 2      HIGH      2        
#>  5 0      ZERO      0        
#>  6 0      ZERO      0        
#>  7 -4     LOW       4        
#>  8 1      HIGH      1        
#>  9 -1     LOW       1        
#> 10 0      ZERO      0        
```
