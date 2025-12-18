# Helper functions for tabulating binary response by subgroup

**\[stable\]**

Helper functions that tabulate in a data frame statistics such as
response rate and odds ratio for population subgroups.

## Usage

``` r
h_proportion_df(rsp, arm)

h_proportion_subgroups_df(
  variables,
  data,
  groups_lists = list(),
  label_all = "All Patients"
)

h_odds_ratio_df(rsp, arm, strata_data = NULL, conf_level = 0.95, method = NULL)

h_odds_ratio_subgroups_df(
  variables,
  data,
  groups_lists = list(),
  conf_level = 0.95,
  method = NULL,
  label_all = "All Patients"
)
```

## Arguments

- rsp:

  (`logical`)  
  vector indicating whether each subject is a responder or not.

- arm:

  (`factor`)  
  the treatment group variable.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- label_all:

  (`string`)  
  label for the total population analysis.

- strata_data:

  (`factor`, `data.frame`, or `NULL`)  
  required if stratified analysis is performed.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string` or `NULL`)  
  specifies the test used to calculate the p-value for the difference
  between two proportions. For options, see
  [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md).
  Default is `NULL` so no test is performed.

## Value

- `h_proportion_df()` returns a `data.frame` with columns `arm`, `n`,
  `n_rsp`, and `prop`.

&nbsp;

- `h_proportion_subgroups_df()` returns a `data.frame` with columns
  `arm`, `n`, `n_rsp`, `prop`, `subgroup`, `var`, `var_label`, and
  `row_type`.

&nbsp;

- `h_odds_ratio_df()` returns a `data.frame` with columns `arm`,
  `n_tot`, `or`, `lcl`, `ucl`, `conf_level`, and optionally `pval` and
  `pval_label`.

&nbsp;

- `h_odds_ratio_subgroups_df()` returns a `data.frame` with columns
  `arm`, `n_tot`, `or`, `lcl`, `ucl`, `conf_level`, `subgroup`, `var`,
  `var_label`, and `row_type`.

## Details

Main functionality is to prepare data for use in a layout-creating
function.

## Functions

- `h_proportion_df()`: Helper to prepare a data frame of binary
  responses by arm.

- `h_proportion_subgroups_df()`: Summarizes proportion of binary
  responses by arm and across subgroups in a data frame. `variables`
  corresponds to the names of variables found in `data`, passed as a
  named list and requires elements `rsp`, `arm` and optionally
  `subgroups`. `groups_lists` optionally specifies groupings for
  `subgroups` variables.

- `h_odds_ratio_df()`: Helper to prepare a data frame with estimates of
  the odds ratio between a treatment and a control arm.

- `h_odds_ratio_subgroups_df()`: Summarizes estimates of the odds ratio
  between a treatment and a control arm across subgroups in a data
  frame. `variables` corresponds to the names of variables found in
  `data`, passed as a named list and requires elements `rsp`, `arm` and
  optionally `subgroups` and `strata`. `groups_lists` optionally
  specifies groupings for `subgroups` variables.

## Examples

``` r
library(dplyr)
library(forcats)

adrs <- tern_ex_adrs
adrs_labels <- formatters::var_labels(adrs)

adrs_f <- adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
  droplevels() %>%
  mutate(
    # Reorder levels of factor to make the placebo group the reference arm.
    ARM = fct_relevel(ARM, "B: Placebo"),
    rsp = AVALC == "CR"
  )
formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")

h_proportion_df(
  c(TRUE, FALSE, FALSE),
  arm = factor(c("A", "A", "B"), levels = c("A", "B"))
)
#>   arm n n_rsp prop
#> 1   A 2     1  0.5
#> 2   B 1     0  0.0

h_proportion_subgroups_df(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
  data = adrs_f
)
#>           arm  n n_rsp      prop     subgroup    var
#> 1  B: Placebo 73    50 0.6849315 All Patients    ALL
#> 2   A: Drug X 69    59 0.8550725 All Patients    ALL
#> 3  B: Placebo 40    25 0.6250000            F    SEX
#> 4   A: Drug X 38    36 0.9473684            F    SEX
#> 5  B: Placebo 33    25 0.7575758            M    SEX
#> 6   A: Drug X 31    23 0.7419355            M    SEX
#> 7  B: Placebo 24    13 0.5416667          LOW BMRKR2
#> 8   A: Drug X 26    21 0.8076923          LOW BMRKR2
#> 9  B: Placebo 23    17 0.7391304       MEDIUM BMRKR2
#> 10  A: Drug X 26    23 0.8846154       MEDIUM BMRKR2
#> 11 B: Placebo 26    20 0.7692308         HIGH BMRKR2
#> 12  A: Drug X 17    15 0.8823529         HIGH BMRKR2
#>                       var_label row_type
#> 1                  All Patients  content
#> 2                  All Patients  content
#> 3                           Sex analysis
#> 4                           Sex analysis
#> 5                           Sex analysis
#> 6                           Sex analysis
#> 7  Continuous Level Biomarker 2 analysis
#> 8  Continuous Level Biomarker 2 analysis
#> 9  Continuous Level Biomarker 2 analysis
#> 10 Continuous Level Biomarker 2 analysis
#> 11 Continuous Level Biomarker 2 analysis
#> 12 Continuous Level Biomarker 2 analysis

# Define groupings for BMRKR2 levels.
h_proportion_subgroups_df(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
  data = adrs_f,
  groups_lists = list(
    BMRKR2 = list(
      "low" = "LOW",
      "low/medium" = c("LOW", "MEDIUM"),
      "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
    )
  )
)
#>           arm  n n_rsp      prop        subgroup    var
#> 1  B: Placebo 73    50 0.6849315    All Patients    ALL
#> 2   A: Drug X 69    59 0.8550725    All Patients    ALL
#> 3  B: Placebo 40    25 0.6250000               F    SEX
#> 4   A: Drug X 38    36 0.9473684               F    SEX
#> 5  B: Placebo 33    25 0.7575758               M    SEX
#> 6   A: Drug X 31    23 0.7419355               M    SEX
#> 7  B: Placebo 24    13 0.5416667             low BMRKR2
#> 8   A: Drug X 26    21 0.8076923             low BMRKR2
#> 9  B: Placebo 47    30 0.6382979      low/medium BMRKR2
#> 10  A: Drug X 52    44 0.8461538      low/medium BMRKR2
#> 11 B: Placebo 73    50 0.6849315 low/medium/high BMRKR2
#> 12  A: Drug X 69    59 0.8550725 low/medium/high BMRKR2
#>                       var_label row_type
#> 1                  All Patients  content
#> 2                  All Patients  content
#> 3                           Sex analysis
#> 4                           Sex analysis
#> 5                           Sex analysis
#> 6                           Sex analysis
#> 7  Continuous Level Biomarker 2 analysis
#> 8  Continuous Level Biomarker 2 analysis
#> 9  Continuous Level Biomarker 2 analysis
#> 10 Continuous Level Biomarker 2 analysis
#> 11 Continuous Level Biomarker 2 analysis
#> 12 Continuous Level Biomarker 2 analysis

# Unstratatified analysis.
h_odds_ratio_df(
  c(TRUE, FALSE, FALSE, TRUE),
  arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
)
#>   arm n_tot or        lcl      ucl conf_level
#> 1         4  1 0.01984252 50.39681       0.95

# Include p-value.
h_odds_ratio_df(adrs_f$rsp, adrs_f$ARM, method = "chisq")
#>   arm n_tot    or      lcl      ucl conf_level       pval
#> 1       142 2.714 1.180449 6.239827       0.95 0.01643036
#>                   pval_label
#> 1 p-value (Chi-Squared Test)

# Stratatified analysis.
h_odds_ratio_df(
  rsp = adrs_f$rsp,
  arm = adrs_f$ARM,
  strata_data = adrs_f[, c("STRATA1", "STRATA2")],
  method = "cmh"
)
#>   arm n_tot       or      lcl      ucl conf_level       pval
#> 1       142 2.665586 1.146149 6.199324       0.95 0.02019665
#>                               pval_label
#> 1 p-value (Cochran-Mantel-Haenszel Test)

# Unstratified analysis.
h_odds_ratio_subgroups_df(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
  data = adrs_f
)
#>   arm n_tot        or       lcl       ucl conf_level     subgroup    var
#> 1       142  2.714000 1.1804488  6.239827       0.95 All Patients    ALL
#> 2        78 10.800000 2.2669576 51.452218       0.95            F    SEX
#> 3        64  0.920000 0.2966470  2.853223       0.95            M    SEX
#> 4        50  3.553846 1.0047370 12.570277       0.95          LOW BMRKR2
#> 5        49  2.705882 0.5911718 12.385232       0.95       MEDIUM BMRKR2
#> 6        43  2.250000 0.3970298 12.750933       0.95         HIGH BMRKR2
#>                      var_label row_type
#> 1                 All Patients  content
#> 2                          Sex analysis
#> 3                          Sex analysis
#> 4 Continuous Level Biomarker 2 analysis
#> 5 Continuous Level Biomarker 2 analysis
#> 6 Continuous Level Biomarker 2 analysis

# Stratified analysis.
h_odds_ratio_subgroups_df(
  variables = list(
    rsp = "rsp",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2"),
    strata = c("STRATA1", "STRATA2")
  ),
  data = adrs_f
)
#>   arm n_tot        or       lcl       ucl conf_level     subgroup    var
#> 1       142 2.6655860 1.1461490  6.199324       0.95 All Patients    ALL
#> 2        78 7.7065093 1.5817529 37.547132       0.95            F    SEX
#> 3        64 0.9572284 0.2990954  3.063525       0.95            M    SEX
#> 4        50 3.0323726 0.8833232 10.409875       0.95          LOW BMRKR2
#> 5        49 2.1264996 0.4312008 10.486995       0.95       MEDIUM BMRKR2
#> 6        43 2.5134820 0.4351747 14.517370       0.95         HIGH BMRKR2
#>                      var_label row_type
#> 1                 All Patients  content
#> 2                          Sex analysis
#> 3                          Sex analysis
#> 4 Continuous Level Biomarker 2 analysis
#> 5 Continuous Level Biomarker 2 analysis
#> 6 Continuous Level Biomarker 2 analysis

# Define groupings of BMRKR2 levels.
h_odds_ratio_subgroups_df(
  variables = list(
    rsp = "rsp",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2")
  ),
  data = adrs_f,
  groups_lists = list(
    BMRKR2 = list(
      "low" = "LOW",
      "low/medium" = c("LOW", "MEDIUM"),
      "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
    )
  )
)
#>   arm n_tot        or      lcl       ucl conf_level        subgroup    var
#> 1       142  2.714000 1.180449  6.239827       0.95    All Patients    ALL
#> 2        78 10.800000 2.266958 51.452218       0.95               F    SEX
#> 3        64  0.920000 0.296647  2.853223       0.95               M    SEX
#> 4        50  3.553846 1.004737 12.570277       0.95             low BMRKR2
#> 5        99  3.116667 1.193409  8.139385       0.95      low/medium BMRKR2
#> 6       142  2.714000 1.180449  6.239827       0.95 low/medium/high BMRKR2
#>                      var_label row_type
#> 1                 All Patients  content
#> 2                          Sex analysis
#> 3                          Sex analysis
#> 4 Continuous Level Biomarker 2 analysis
#> 5 Continuous Level Biomarker 2 analysis
#> 6 Continuous Level Biomarker 2 analysis
```
