# Multivariate Cox model - summarized results

Analyses based on multivariate Cox model are usually not performed for
the Controlled Substance Reporting or regulatory documents but serve
exploratory purposes only (e.g., for publication). In practice, the
model usually includes only the main effects (without interaction
terms). It produces the hazard ratio estimates for each of the
covariates included in the model. The analysis follows the same
principles (e.g., stratified vs. unstratified analysis and tie handling)
as the usual Cox model analysis. Since there is usually no pre-specified
hypothesis testing for such analysis, the p.values need to be
interpreted with caution. (**Statistical Analysis of Clinical Trials
Data with R**, `NEST's bookdown`)

## Usage

``` r
s_cox_multivariate(
  formula,
  data,
  conf_level = 0.95,
  pval_method = c("wald", "likelihood"),
  ...
)
```

## Arguments

- formula:

  (`formula`)  
  a formula corresponding to the investigated
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  survival model including covariates.

- data:

  (`data.frame`)  
  a data frame which includes the variable in formula and covariates.

- conf_level:

  (`proportion`)  
  the confidence level for the hazard ratio interval estimations.
  Default is 0.95.

- pval_method:

  (`string`)  
  the method used for the estimation of p-values, should be one of
  `"wald"` (default) or `"likelihood"`.

- ...:

  optional parameters passed to
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).
  Can include `ties`, a character string specifying the method for tie
  handling, one of `exact` (default), `efron`, `breslow`.

## Value

A `list` with elements `mod`, `msum`, `aov`, and `coef_inter`.

## Details

The output is limited to single effect terms. Work in ongoing for
estimation of interaction terms but is out of scope as defined by the
Global Data Standards Repository
(**`GDS_Standard_TLG_Specs_Tables_2.doc`**).

## See also

[`estimate_coef()`](https://insightsengineering.github.io/tern/reference/estimate_coef.md).

## Examples

``` r
library(dplyr)

adtte <- tern_ex_adtte
adtte_f <- subset(adtte, PARAMCD == "OS") # _f: filtered
adtte_f <- filter(
  adtte_f,
  PARAMCD == "OS" &
    SEX %in% c("F", "M") &
    RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
)
adtte_f$SEX <- droplevels(adtte_f$SEX)
adtte_f$RACE <- droplevels(adtte_f$RACE)
```
