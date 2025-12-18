# Tabulate binary response by subgroup

**\[stable\]**

The `tabulate_rsp_subgroups()` function creates a layout element to
tabulate binary response by subgroup, returning statistics including
response rate and odds ratio for each population subgroup. The table is
created from `df`, a list of data frames returned by
[`extract_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/extract_rsp_subgroups.md),
with the statistics to include specified via the `vars` parameter.

A forest plot can be created from the resulting table using the
[`g_forest()`](https://insightsengineering.github.io/tern/reference/g_forest.md)
function.

## Usage

``` r
tabulate_rsp_subgroups(
  lyt,
  df,
  vars = c("n_tot", "n", "prop", "or", "ci"),
  groups_lists = list(),
  label_all = lifecycle::deprecated(),
  riskdiff = NULL,
  na_str = default_na_str(),
  ...,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

a_response_subgroups(
  df,
  labelstr = "",
  ...,
  .stats = NULL,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- df:

  (`list`)  
  a list of data frames containing all analysis variables. List should
  be created using
  [`extract_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/extract_rsp_subgroups.md).

- vars:

  (`character`)  
  the names of statistics to be reported among:

  - `n`: Total number of observations per group.

  - `n_rsp`: Number of responders per group.

  - `prop`: Proportion of responders.

  - `n_tot`: Total number of observations.

  - `or`: Odds ratio.

  - `ci` : Confidence interval of odds ratio.

  - `pval`: p-value of the effect. Note, the statistics `n_tot`, `or`,
    and `ci` are required.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- label_all:

  (`string`)  
  label for the total population analysis.

- riskdiff:

  (`list`)  
  if a risk (proportion) difference column should be added, a list of
  settings to apply within the column. See
  [`control_riskdiff()`](https://insightsengineering.github.io/tern/reference/control_riskdiff.md)
  for details. If `NULL`, no risk difference column will be added. If
  `riskdiff$arm_x` and `riskdiff$arm_y` are `NULL`, the first level of
  `df$prop$arm` will be used as `arm_x` and the second level as `arm_y`.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- ...:

  additional arguments for the lower level functions.

- .stat_names:

  (`character`)  
  names of the statistics that are passed directly to name single
  statistics (`.stats`). This option is visible when producing
  [`rtables::as_result_df()`](https://insightsengineering.github.io/rtables/latest-tag/reference/data.frame_export.html)
  with `make_ard = TRUE`.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .stats:

  (`character`)  
  statistics to select for the table.

## Value

An `rtables` table summarizing binary response by subgroup.

- `a_response_subgroups()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Details

These functions create a layout starting from a data frame which
contains the required statistics. Tables typically used as part of
forest plot.

## Functions

- `tabulate_rsp_subgroups()`: Table-creating function which creates a
  table summarizing binary response by subgroup. This function is a
  wrapper for
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html)
  and
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `a_response_subgroups()`: Formatted analysis function which is used as
  `afun` in `tabulate_rsp_subgroups()`.

## See also

[`extract_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/extract_rsp_subgroups.md)

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

# Unstratified analysis.
df <- extract_rsp_subgroups(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
  data = adrs_f
)
df
#> $prop
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
#> 
#> $or
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
#> 

# Stratified analysis.
df_strat <- extract_rsp_subgroups(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2"), strata = "STRATA1"),
  data = adrs_f
)
df_strat
#> $prop
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
#> 
#> $or
#>   arm n_tot        or       lcl       ucl conf_level     subgroup    var
#> 1       142 2.6343899 1.1537821  6.015009       0.95 All Patients    ALL
#> 2        78 9.5946605 2.0379337 45.171985       0.95            F    SEX
#> 3        64 0.8947158 0.2936803  2.725809       0.95            M    SEX
#> 4        50 3.5976656 1.0101319 12.813374       0.95          LOW BMRKR2
#> 5        49 2.6242168 0.5162572 13.339308       0.95       MEDIUM BMRKR2
#> 6        43 2.2816865 0.4116391 12.647228       0.95         HIGH BMRKR2
#>                      var_label row_type
#> 1                 All Patients  content
#> 2                          Sex analysis
#> 3                          Sex analysis
#> 4 Continuous Level Biomarker 2 analysis
#> 5 Continuous Level Biomarker 2 analysis
#> 6 Continuous Level Biomarker 2 analysis
#> 

# Grouping of the BMRKR2 levels.
df_grouped <- extract_rsp_subgroups(
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
df_grouped
#> $prop
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
#> 
#> $or
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
#> 

# Table with default columns
basic_table() %>%
  tabulate_rsp_subgroups(df)
#>                                             B: Placebo           A: Drug X                                 
#> Baseline Risk Factors          Total n   n    Response (%)   n    Response (%)   Odds Ratio      95% CI    
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> All Patients                     142     73      68.5%       69      85.5%          2.71      (1.18, 6.24) 
#> Sex                                                                                                        
#>   F                              78      40      62.5%       38      94.7%         10.80      (2.27, 51.45)
#>   M                              64      33      75.8%       31      74.2%          0.92      (0.30, 2.85) 
#> Continuous Level Biomarker 2                                                                               
#>   LOW                            50      24      54.2%       26      80.8%          3.55      (1.00, 12.57)
#>   MEDIUM                         49      23      73.9%       26      88.5%          2.71      (0.59, 12.39)
#>   HIGH                           43      26      76.9%       17      88.2%          2.25      (0.40, 12.75)

# Table with selected columns
basic_table() %>%
  tabulate_rsp_subgroups(
    df = df,
    vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci")
  )
#>                                                    B: Placebo                       A: Drug X                                        
#> Baseline Risk Factors          Total n   n    Responders   Response (%)   n    Responders   Response (%)   Odds Ratio      95% CI    
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> All Patients                     142     73       50          68.5%       69       59          85.5%          2.71      (1.18, 6.24) 
#> Sex                                                                                                                                  
#>   F                              78      40       25          62.5%       38       36          94.7%         10.80      (2.27, 51.45)
#>   M                              64      33       25          75.8%       31       23          74.2%          0.92      (0.30, 2.85) 
#> Continuous Level Biomarker 2                                                                                                         
#>   LOW                            50      24       13          54.2%       26       21          80.8%          3.55      (1.00, 12.57)
#>   MEDIUM                         49      23       17          73.9%       26       23          88.5%          2.71      (0.59, 12.39)
#>   HIGH                           43      26       20          76.9%       17       15          88.2%          2.25      (0.40, 12.75)

# Table with risk difference column added
basic_table() %>%
  tabulate_rsp_subgroups(
    df,
    riskdiff = control_riskdiff(
      arm_x = levels(df$prop$arm)[1],
      arm_y = levels(df$prop$arm)[2]
    )
  )
#>                                             B: Placebo           A: Drug X                                                                
#> Baseline Risk Factors          Total n   n    Response (%)   n    Response (%)   Odds Ratio      95% CI       Risk Difference (%) (95% CI)
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> All Patients                     142     73      68.5%       69      85.5%          2.71      (1.18, 6.24)        -17.0 (-30.5 - -3.5)    
#> Sex                                                                                                                                       
#>   F                              78      40      62.5%       38      94.7%         10.80      (2.27, 51.45)      -32.2 (-48.8 - -15.6)    
#>   M                              64      33      75.8%       31      74.2%          0.92      (0.30, 2.85)         1.6 (-19.7 - 22.8)     
#> Continuous Level Biomarker 2                                                                                                              
#>   LOW                            50      24      54.2%       26      80.8%          3.55      (1.00, 12.57)       -26.6 (-51.6 - -1.6)    
#>   MEDIUM                         49      23      73.9%       26      88.5%          2.71      (0.59, 12.39)       -14.5 (-36.3 - 7.2)     
#>   HIGH                           43      26      76.9%       17      88.2%          2.25      (0.40, 12.75)       -11.3 (-33.6 - 11.0)    
```
