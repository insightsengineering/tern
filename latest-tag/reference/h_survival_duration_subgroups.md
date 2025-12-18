# Helper functions for tabulating survival duration by subgroup

**\[stable\]**

Helper functions that tabulate in a data frame statistics such as median
survival time and hazard ratio for population subgroups.

## Usage

``` r
h_survtime_df(tte, is_event, arm)

h_survtime_subgroups_df(
  variables,
  data,
  groups_lists = list(),
  label_all = "All Patients"
)

h_coxph_df(tte, is_event, arm, strata_data = NULL, control = control_coxph())

h_coxph_subgroups_df(
  variables,
  data,
  groups_lists = list(),
  control = control_coxph(),
  label_all = "All Patients"
)
```

## Arguments

- tte:

  (`numeric`)  
  vector of time-to-event duration values.

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

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

- control:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).
  Some possible parameter options are:

  - `pval_method` (`string`)  
    p-value method for testing the null hypothesis that hazard ratio
    = 1. Default method is `"log-rank"` which comes from
    [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html),
    can also be set to `"wald"` or `"likelihood"` (from
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)).

  - `ties` (`string`)  
    specifying the method for tie handling. Default is `"efron"`, can
    also be set to `"breslow"` or `"exact"`. See more in
    [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

  - `conf_level` (`proportion`)  
    confidence level of the interval for HR.

## Value

- `h_survtime_df()` returns a `data.frame` with columns `arm`, `n`,
  `n_events`, and `median`.

&nbsp;

- `h_survtime_subgroups_df()` returns a `data.frame` with columns `arm`,
  `n`, `n_events`, `median`, `subgroup`, `var`, `var_label`, and
  `row_type`.

&nbsp;

- `h_coxph_df()` returns a `data.frame` with columns `arm`, `n_tot`,
  `n_tot_events`, `hr`, `lcl`, `ucl`, `conf_level`, `pval` and
  `pval_label`.

&nbsp;

- `h_coxph_subgroups_df()` returns a `data.frame` with columns `arm`,
  `n_tot`, `n_tot_events`, `hr`, `lcl`, `ucl`, `conf_level`, `pval`,
  `pval_label`, `subgroup`, `var`, `var_label`, and `row_type`.

## Details

Main functionality is to prepare data for use in a layout-creating
function.

## Functions

- `h_survtime_df()`: Helper to prepare a data frame of median survival
  times by arm.

- `h_survtime_subgroups_df()`: Summarizes median survival times by arm
  and across subgroups in a data frame. `variables` corresponds to the
  names of variables found in `data`, passed as a named list and
  requires elements `tte`, `is_event`, `arm` and optionally `subgroups`.
  `groups_lists` optionally specifies groupings for `subgroups`
  variables.

- `h_coxph_df()`: Helper to prepare a data frame with estimates of
  treatment hazard ratio.

- `h_coxph_subgroups_df()`: Summarizes estimates of the treatment hazard
  ratio across subgroups in a data frame. `variables` corresponds to the
  names of variables found in `data`, passed as a named list and
  requires elements `tte`, `is_event`, `arm` and optionally `subgroups`
  and `strata`. `groups_lists` optionally specifies groupings for
  `subgroups` variables.

## Examples

``` r
library(dplyr)
library(forcats)

adtte <- tern_ex_adtte

# Save variable labels before data processing steps.
adtte_labels <- formatters::var_labels(adtte)

adtte_f <- adtte %>%
  filter(
    PARAMCD == "OS",
    ARM %in% c("B: Placebo", "A: Drug X"),
    SEX %in% c("M", "F")
  ) %>%
  mutate(
    # Reorder levels of ARM to display reference arm before treatment arm.
    ARM = droplevels(fct_relevel(ARM, "B: Placebo")),
    SEX = droplevels(SEX),
    is_event = CNSR == 0
  )
labels <- c("ARM" = adtte_labels[["ARM"]], "SEX" = adtte_labels[["SEX"]], "is_event" = "Event Flag")
formatters::var_labels(adtte_f)[names(labels)] <- labels

# Extract median survival time for one group.
h_survtime_df(
  tte = adtte_f$AVAL,
  is_event = adtte_f$is_event,
  arm = adtte_f$ARM
)
#>          arm  n n_events   median
#> 1 B: Placebo 73       57 727.8043
#> 2  A: Drug X 69       44 974.6402

# Extract median survival time for multiple groups.
h_survtime_subgroups_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2")
  ),
  data = adtte_f
)
#>           arm  n n_events    median     subgroup    var
#> 1  B: Placebo 73       57  727.8043 All Patients    ALL
#> 2   A: Drug X 69       44  974.6402 All Patients    ALL
#> 3  B: Placebo 40       31  599.1772            F    SEX
#> 4   A: Drug X 38       24 1016.2982            F    SEX
#> 5  B: Placebo 33       26  888.4916            M    SEX
#> 6   A: Drug X 31       20  974.6402            M    SEX
#> 7  B: Placebo 24       21  735.4722          LOW BMRKR2
#> 8   A: Drug X 26       15  974.6402          LOW BMRKR2
#> 9  B: Placebo 23       14  731.8352       MEDIUM BMRKR2
#> 10  A: Drug X 26       17  964.2197       MEDIUM BMRKR2
#> 11 B: Placebo 26       22  654.8245         HIGH BMRKR2
#> 12  A: Drug X 17       12 1016.2982         HIGH BMRKR2
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
h_survtime_subgroups_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2")
  ),
  data = adtte_f,
  groups_lists = list(
    BMRKR2 = list(
      "low" = "LOW",
      "low/medium" = c("LOW", "MEDIUM"),
      "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
    )
  )
)
#>           arm  n n_events    median        subgroup    var
#> 1  B: Placebo 73       57  727.8043    All Patients    ALL
#> 2   A: Drug X 69       44  974.6402    All Patients    ALL
#> 3  B: Placebo 40       31  599.1772               F    SEX
#> 4   A: Drug X 38       24 1016.2982               F    SEX
#> 5  B: Placebo 33       26  888.4916               M    SEX
#> 6   A: Drug X 31       20  974.6402               M    SEX
#> 7  B: Placebo 24       21  735.4722             low BMRKR2
#> 8   A: Drug X 26       15  974.6402             low BMRKR2
#> 9  B: Placebo 47       35  735.4722      low/medium BMRKR2
#> 10  A: Drug X 52       32  964.2197      low/medium BMRKR2
#> 11 B: Placebo 73       57  727.8043 low/medium/high BMRKR2
#> 12  A: Drug X 69       44  974.6402 low/medium/high BMRKR2
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

# Extract hazard ratio for one group.
h_coxph_df(adtte_f$AVAL, adtte_f$is_event, adtte_f$ARM)
#>   arm n_tot n_tot_events        hr       lcl      ucl conf_level       pval
#> 1       142          101 0.7108557 0.4779138 1.057337       0.95 0.09049511
#>           pval_label
#> 1 p-value (log-rank)

# Extract hazard ratio for one group with stratification factor.
h_coxph_df(adtte_f$AVAL, adtte_f$is_event, adtte_f$ARM, strata_data = adtte_f$STRATA1)
#>   arm n_tot n_tot_events        hr       lcl     ucl conf_level       pval
#> 1       142          101 0.6646586 0.4399495 1.00414       0.95 0.05089188
#>           pval_label
#> 1 p-value (log-rank)

# Extract hazard ratio for multiple groups.
h_coxph_subgroups_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2")
  ),
  data = adtte_f
)
#>   arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
#> 1       142          101 0.7108557 0.4779138 1.0573368       0.95 0.09049511
#> 2        78           55 0.5595391 0.3246658 0.9643271       0.95 0.03411759
#> 3        64           46 0.9102874 0.5032732 1.6464678       0.95 0.75582028
#> 4        50           36 0.7617717 0.3854349 1.5055617       0.95 0.43236030
#> 5        49           31 0.7651261 0.3641277 1.6077269       0.95 0.47860004
#> 6        43           34 0.6662356 0.3257413 1.3626456       0.95 0.26285846
#>           pval_label     subgroup    var                    var_label row_type
#> 1 p-value (log-rank) All Patients    ALL                 All Patients  content
#> 2 p-value (log-rank)            F    SEX                          Sex analysis
#> 3 p-value (log-rank)            M    SEX                          Sex analysis
#> 4 p-value (log-rank)          LOW BMRKR2 Continuous Level Biomarker 2 analysis
#> 5 p-value (log-rank)       MEDIUM BMRKR2 Continuous Level Biomarker 2 analysis
#> 6 p-value (log-rank)         HIGH BMRKR2 Continuous Level Biomarker 2 analysis

# Define groupings of BMRKR2 levels.
h_coxph_subgroups_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2")
  ),
  data = adtte_f,
  groups_lists = list(
    BMRKR2 = list(
      "low" = "LOW",
      "low/medium" = c("LOW", "MEDIUM"),
      "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
    )
  )
)
#>   arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
#> 1       142          101 0.7108557 0.4779138 1.0573368       0.95 0.09049511
#> 2        78           55 0.5595391 0.3246658 0.9643271       0.95 0.03411759
#> 3        64           46 0.9102874 0.5032732 1.6464678       0.95 0.75582028
#> 4        50           36 0.7617717 0.3854349 1.5055617       0.95 0.43236030
#> 5        99           67 0.7472958 0.4600419 1.2139136       0.95 0.23764314
#> 6       142          101 0.7108557 0.4779138 1.0573368       0.95 0.09049511
#>           pval_label        subgroup    var                    var_label
#> 1 p-value (log-rank)    All Patients    ALL                 All Patients
#> 2 p-value (log-rank)               F    SEX                          Sex
#> 3 p-value (log-rank)               M    SEX                          Sex
#> 4 p-value (log-rank)             low BMRKR2 Continuous Level Biomarker 2
#> 5 p-value (log-rank)      low/medium BMRKR2 Continuous Level Biomarker 2
#> 6 p-value (log-rank) low/medium/high BMRKR2 Continuous Level Biomarker 2
#>   row_type
#> 1  content
#> 2 analysis
#> 3 analysis
#> 4 analysis
#> 5 analysis
#> 6 analysis

# Extract hazard ratio for multiple groups with stratification factors.
h_coxph_subgroups_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    arm = "ARM",
    subgroups = c("SEX", "BMRKR2"),
    strata = c("STRATA1", "STRATA2")
  ),
  data = adtte_f
)
#>   arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
#> 1       142          101 0.6126133 0.3913507 0.9589739       0.95 0.03086774
#> 2        78           55 0.3934024 0.2027682 0.7632630       0.95 0.00469167
#> 3        64           46 0.9501768 0.4730073 1.9087145       0.95 0.88580522
#> 4        50           36 0.7378635 0.3140465 1.7336363       0.95 0.48408079
#> 5        49           31 0.9408062 0.4172095 2.1215148       0.95 0.88305965
#> 6        43           34 0.5125617 0.2125140 1.2362459       0.95 0.13124382
#>           pval_label     subgroup    var                    var_label row_type
#> 1 p-value (log-rank) All Patients    ALL                 All Patients  content
#> 2 p-value (log-rank)            F    SEX                          Sex analysis
#> 3 p-value (log-rank)            M    SEX                          Sex analysis
#> 4 p-value (log-rank)          LOW BMRKR2 Continuous Level Biomarker 2 analysis
#> 5 p-value (log-rank)       MEDIUM BMRKR2 Continuous Level Biomarker 2 analysis
#> 6 p-value (log-rank)         HIGH BMRKR2 Continuous Level Biomarker 2 analysis
```
