# Wrapper function of survival::clogit

When model fitting failed, a more useful message would show.

## Usage

``` r
clogit_with_tryCatch(formula, data, ...)
```

## Arguments

- formula:

  Model formula.

- data:

  data frame.

- ...:

  further parameters to be added to survival::clogit.

## Value

When model fitting is successful, an object of class "clogit".  
When model fitting failed, an error message is shown.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
adrs_local <- tern_ex_adrs %>%
  dplyr::filter(ARMCD %in% c("ARM A", "ARM B")) %>%
  dplyr::mutate(
    RSP = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
    ARMBIN = droplevels(ARMCD)
  )
dta <- adrs_local
dta <- dta[sample(nrow(dta)), ]
mod <- clogit_with_tryCatch(formula = RSP ~ ARMBIN * AGE + strata(STRATA1), data = dta)
} # }
```
