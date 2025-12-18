# Helper function for deriving analysis datasets for select laboratory tables

**\[stable\]**

Helper function that merges ADSL and ADLB datasets so that missing lab
test records are inserted in the output dataset. Remember that
`na_level` must match the needed pre-processing done with
[`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md)
to have the desired output.

## Usage

``` r
h_adsl_adlb_merge_using_worst_flag(
  adsl,
  adlb,
  worst_flag = c(WGRHIFL = "Y"),
  by_visit = FALSE,
  no_fillin_visits = c("SCREENING", "BASELINE")
)
```

## Arguments

- adsl:

  (`data.frame`)  
  ADSL data frame.

- adlb:

  (`data.frame`)  
  ADLB data frame.

- worst_flag:

  (named `character`)  
  worst post-baseline lab flag variable. See how this is implemented in
  the following examples.

- by_visit:

  (`flag`)  
  defaults to `FALSE` to generate worst grade per patient. If worst
  grade per patient per visit is specified for `worst_flag`, then
  `by_visit` should be `TRUE` to generate worst grade patient per visit.

- no_fillin_visits:

  (named `character`)  
  visits that are not considered for post-baseline worst toxicity grade.
  Defaults to `c("SCREENING", "BASELINE")`.

## Value

`df` containing variables shared between `adlb` and `adsl` along with
variables `PARAM`, `PARAMCD`, `ATOXGR`, and `BTOXGR` relevant for
analysis. Optionally, `AVISIT` are `AVISITN` are included when
`by_visit = TRUE` and `no_fillin_visits = c("SCREENING", "BASELINE")`.

## Details

In the result data missing records will be created for the following
situations:

- Patients who are present in `adsl` but have no lab data in `adlb`
  (both baseline and post-baseline).

- Patients who do not have any post-baseline lab values.

- Patients without any post-baseline values flagged as the worst.

## Examples

``` r
# `h_adsl_adlb_merge_using_worst_flag`
adlb_out <- h_adsl_adlb_merge_using_worst_flag(
  tern_ex_adsl,
  tern_ex_adlb,
  worst_flag = c("WGRHIFL" = "Y")
)

# `h_adsl_adlb_merge_using_worst_flag` by visit example
adlb_out_by_visit <- h_adsl_adlb_merge_using_worst_flag(
  tern_ex_adsl,
  tern_ex_adlb,
  worst_flag = c("WGRLOVFL" = "Y"),
  by_visit = TRUE
)
```
