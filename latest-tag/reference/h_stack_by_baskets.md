# Helper function to create a new SMQ variable in ADAE by stacking SMQ and/or CQ records.

**\[stable\]**

Helper function to create a new SMQ variable in ADAE that consists of
all adverse events belonging to selected Standardized/Customized
queries. The new dataset will only contain records of the adverse events
belonging to any of the selected baskets. Remember that `na_str` must
match the needed pre-processing done with
[`df_explicit_na()`](https://insightsengineering.github.io/tern/reference/df_explicit_na.md)
to have the desired output.

## Usage

``` r
h_stack_by_baskets(
  df,
  baskets = grep("^(SMQ|CQ).+NAM$", names(df), value = TRUE),
  smq_varlabel = "Standardized MedDRA Query",
  keys = c("STUDYID", "USUBJID", "ASTDTM", "AEDECOD", "AESEQ"),
  aag_summary = NULL,
  na_str = "<Missing>"
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- baskets:

  (`character`)  
  variable names of the selected Standardized/Customized queries.

- smq_varlabel:

  (`string`)  
  a label for the new variable created.

- keys:

  (`character`)  
  names of the key variables to be returned along with the new variable
  created.

- aag_summary:

  (`data.frame`)  
  containing the SMQ baskets and the levels of interest for the final
  SMQ variable. This is useful when there are some levels of interest
  that are not observed in the `df` dataset. The two columns of this
  dataset should be named `basket` and `basket_name`.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

## Value

A `data.frame` with variables in `keys` taken from `df` and new variable
SMQ containing records belonging to the baskets selected via the
`baskets` argument.

## Examples

``` r
adae <- tern_ex_adae[1:20, ] %>% df_explicit_na()
h_stack_by_baskets(df = adae)
#> # A tibble: 8 × 6
#>   STUDYID USUBJID               ASTDTM              AEDECOD       AESEQ SMQ     
#>   <fct>   <fct>                 <dttm>              <fct>         <int> <fct>   
#> 1 AB12345 AB12345-BRA-11-id-8   2021-12-05 02:02:07 dcd D.2.1.5.3     2 D.2.1.5…
#> 2 AB12345 AB12345-BRA-12-id-120 2020-02-05 01:42:29 dcd D.2.1.5.3     2 D.2.1.5…
#> 3 AB12345 AB12345-BRA-1-id-171  2022-11-29 12:18:31 dcd C.1.1.1.3     2 C.1.1.1…
#> 4 AB12345 AB12345-BRA-1-id-23   2020-07-10 07:32:49 dcd B.2.2.3.1     3 C.1.1.1…
#> 5 AB12345 AB12345-BRA-1-id-59   2021-10-10 23:54:46 dcd C.1.1.1.3     4 C.1.1.1…
#> 6 AB12345 AB12345-BRA-1-id-9    2021-06-01 14:39:09 dcd C.1.1.1.3     1 C.1.1.1…
#> 7 AB12345 AB12345-BRA-11-id-8   2021-12-21 02:02:07 dcd C.1.1.1.3     3 C.1.1.1…
#> 8 AB12345 AB12345-BRA-12-id-120 2020-10-01 01:42:29 dcd C.1.1.1.3     3 C.1.1.1…

aag <- data.frame(
  NAMVAR = c("CQ01NAM", "CQ02NAM", "SMQ01NAM", "SMQ02NAM"),
  REFNAME = c(
    "D.2.1.5.3/A.1.1.1.1 aesi", "X.9.9.9.9/Y.8.8.8.8 aesi",
    "C.1.1.1.3/B.2.2.3.1 aesi", "C.1.1.1.3/B.3.3.3.3 aesi"
  ),
  SCOPE = c("", "", "BROAD", "BROAD"),
  stringsAsFactors = FALSE
)

basket_name <- character(nrow(aag))
cq_pos <- grep("^(CQ).+NAM$", aag$NAMVAR)
smq_pos <- grep("^(SMQ).+NAM$", aag$NAMVAR)
basket_name[cq_pos] <- aag$REFNAME[cq_pos]
basket_name[smq_pos] <- paste0(
  aag$REFNAME[smq_pos], "(", aag$SCOPE[smq_pos], ")"
)

aag_summary <- data.frame(
  basket = aag$NAMVAR,
  basket_name = basket_name,
  stringsAsFactors = TRUE
)

result <- h_stack_by_baskets(df = adae, aag_summary = aag_summary)
all(levels(aag_summary$basket_name) %in% levels(result$SMQ))
#> [1] TRUE

h_stack_by_baskets(
  df = adae,
  aag_summary = NULL,
  keys = c("STUDYID", "USUBJID", "AEDECOD", "ARM"),
  baskets = "SMQ01NAM"
)
#> # A tibble: 6 × 5
#>   STUDYID USUBJID               AEDECOD       ARM            SMQ                
#>   <fct>   <fct>                 <fct>         <fct>          <fct>              
#> 1 AB12345 AB12345-BRA-1-id-171  dcd C.1.1.1.3 B: Placebo     C.1.1.1.3/B.2.2.3.…
#> 2 AB12345 AB12345-BRA-1-id-23   dcd B.2.2.3.1 A: Drug X      C.1.1.1.3/B.2.2.3.…
#> 3 AB12345 AB12345-BRA-1-id-59   dcd C.1.1.1.3 A: Drug X      C.1.1.1.3/B.2.2.3.…
#> 4 AB12345 AB12345-BRA-1-id-9    dcd C.1.1.1.3 C: Combination C.1.1.1.3/B.2.2.3.…
#> 5 AB12345 AB12345-BRA-11-id-8   dcd C.1.1.1.3 A: Drug X      C.1.1.1.3/B.2.2.3.…
#> 6 AB12345 AB12345-BRA-12-id-120 dcd C.1.1.1.3 A: Drug X      C.1.1.1.3/B.2.2.3.…
```
