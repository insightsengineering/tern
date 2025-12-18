# Helper function for generating a pairwise Cox-PH table

**\[stable\]**

Create a `data.frame` of pairwise stratified or unstratified Cox-PH
analysis results.

## Usage

``` r
h_tbl_coxph_pairwise(
  df,
  variables,
  ref_group_coxph = NULL,
  control_coxph_pw = control_coxph(),
  annot_coxph_ref_lbls = FALSE
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- variables:

  (named `list`)  
  variable names. Details are:

  - `tte` (`numeric`)  
    variable indicating time-to-event duration values.

  - `is_event` (`logical`)  
    event variable. `TRUE` if event, `FALSE` if time to event is
    censored.

  - `arm` (`factor`)  
    the treatment group variable.

  - `strata` (`character` or `NULL`)  
    variable names indicating stratification factors.

- ref_group_coxph:

  (`string` or `NULL`)  
  level of arm variable to use as reference group in calculations for
  `annot_coxph` table. If `NULL` (default), uses the first level of the
  arm variable.

- control_coxph_pw:

  (`list`)  
  parameters for comparison details, specified using the helper function
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).
  Some possible parameter options are:

  - `pval_method` (`string`)  
    p-value method for testing hazard ratio = 1. Default method is
    `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.

  - `ties` (`string`)  
    method for tie handling. Default is `"efron"`, can also be set to
    `"breslow"` or `"exact"`. See more in
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)

  - `conf_level` (`proportion`)  
    confidence level of the interval for HR.

- annot_coxph_ref_lbls:

  (`flag`)  
  whether the reference group should be explicitly printed in labels for
  the `annot_coxph` table. If `FALSE` (default), only comparison groups
  will be printed in `annot_coxph` table labels.

## Value

A `data.frame` containing statistics `HR`, `XX% CI` (`XX` taken from
`control_coxph_pw`), and `p-value (log-rank)`.

## Examples

``` r
library(dplyr)

adtte <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(is_event = CNSR == 0)

h_tbl_coxph_pairwise(
  df = adtte,
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
  control_coxph_pw = control_coxph(conf_level = 0.9)
)
#>                  HR       90% CI p-value (log-rank)
#> B: Placebo     1.41 (1.01, 1.96)             0.0905
#> C: Combination 1.81 (1.24, 2.64)             0.0086
```
