# Summarize Poisson negative binomial regression

**\[experimental\]**

Summarize results of a Poisson negative binomial regression. This can be
used to analyze count and/or frequency data using a linear model. It is
specifically useful for analyzing count data (using the Poisson or
Negative Binomial distribution) that is result of a generalized linear
model of one (e.g. arm) or more covariates.

## Usage

``` r
summarize_glm_count(
  lyt,
  vars,
  variables,
  distribution,
  conf_level,
  rate_mean_method = c("emmeans", "ppmeans")[1],
  weights = stats::weights,
  scale = 1,
  var_labels,
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  show_labels = "visible",
  table_names = vars,
  .stats = c("n", "rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = list(rate_ci = 1L, rate_ratio_ci = 1L, pval = 1L)
)

s_glm_count(
  df,
  .var,
  .df_row,
  .ref_group,
  .in_ref_col,
  variables,
  distribution,
  conf_level,
  rate_mean_method,
  weights,
  scale = 1,
  ...
)

a_glm_count(
  df,
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

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables, with expected elements:

  - `arm` (`string`)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of `arm`
    variable is taken as the reference group.

  - `covariates` (`character`)  
    a vector that can contain single variable names (such as `"X1"`),
    and/or interaction terms indicated by `"X1 * X2"`.

  - `offset` (`numeric`)  
    a numeric vector or scalar adding an offset.

- distribution:

  (`character`)  
  a character value specifying the distribution used in the regression
  (Poisson, Quasi-Poisson, negative binomial).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- rate_mean_method:

  (`character(1)`)  
  method used to estimate the mean odds ratio. Defaults to `emmeans`.
  see details for more information.

- weights:

  (`character`)  
  a character vector specifying weights used in averaging predictions.
  Number of weights must equal the number of levels included in the
  covariates. Weights option passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- scale:

  (`numeric(1)`)  
  linear scaling factor for rate and confidence intervals. Defaults to
  `1`.

- var_labels:

  (`character`)  
  variable labels.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- ...:

  additional arguments for the lower level functions.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are:
  `'n', 'rate', 'rate_ci', 'rate_ratio', 'rate_ratio_ci', 'pval'`

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

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .df_row:

  (`data.frame`)  
  dataset that includes all the variables that are called in `.var` and
  `variables`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

## Value

- `summarize_glm_count()` returns a layout object suitable for passing
  to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_glm_count()` to the table layout.

&nbsp;

- `s_glm_count()` returns a named `list` of 5 statistics:

  - `n`: Count of complete sample size for the group.

  - `rate`: Estimated event rate per follow-up time.

  - `rate_ci`: Confidence level for estimated rate per follow-up time.

  - `rate_ratio`: Ratio of event rates in each treatment arm to the
    reference arm.

  - `rate_ratio_ci`: Confidence level for the rate ratio.

  - `pval`: p-value.

&nbsp;

- `a_glm_count()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Details

`summarize_glm_count()` uses `s_glm_count()` to calculate the statistics
for the table. This analysis function uses
[`h_glm_count()`](https://insightsengineering.github.io/tern/reference/h_glm_count.md)
to estimate the GLM with
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) for Poisson and
Quasi-Poisson distributions or
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) for
Negative Binomial distribution. All methods assume a logarithmic link
function.

At this point, rates and confidence intervals are estimated from the
model using either
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
when `rate_mean_method = "emmeans"` or
[`h_ppmeans()`](https://insightsengineering.github.io/tern/reference/h_ppmeans.md)
when `rate_mean_method = "ppmeans"`.

If a reference group is specified while building the table with
`split_cols_by(ref_group)`, no rate ratio or `p-value` are calculated.
Otherwise, we use
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
to calculate the rate ratio and `p-value` for the reference group.
Values are always estimated with `method = "trt.vs.ctrl"` and `ref`
equal to the first `arm` value.

## Functions

- `summarize_glm_count()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_glm_count()`: Statistics function that produces a named list of
  results of the investigated Poisson model.

- `a_glm_count()`: Formatted analysis function which is used as `afun`
  in `summarize_glm_count()`.

## Examples

``` r
library(dplyr)

anl <- tern_ex_adtte %>% filter(PARAMCD == "TNE")
anl$AVAL_f <- as.factor(anl$AVAL)

lyt <- basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  analyze_vars(
    "AVAL_f",
    var_labels = "Number of exacerbations per patient",
    .stats = c("count_fraction"),
    .formats = c("count_fraction" = "xx (xx.xx%)"),
    .labels = c("Number of exacerbations per patient")
  ) %>%
  summarize_glm_count(
    vars = "AVAL",
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
    conf_level = 0.95,
    distribution = "poisson",
    rate_mean_method = "emmeans",
    var_labels = "Adjusted (P) exacerbation rate (per year)",
    table_names = "adjP",
    .stats = c("rate"),
    .labels = c(rate = "Rate")
  ) %>%
  summarize_glm_count(
    vars = "AVAL",
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "quasipoisson",
    rate_mean_method = "ppmeans",
    var_labels = "Adjusted (QP) exacerbation rate (per year)",
    table_names = "adjQP",
    .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
    .labels = c(
      rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
      rate_ratio_ci = "Rate Ratio CI", pval = "p value"
    )
  ) %>%
  summarize_glm_count(
    vars = "AVAL",
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
    conf_level = 0.95,
    distribution = "negbin",
    rate_mean_method = "emmeans",
    var_labels = "Adjusted (NB) exacerbation rate (per year)",
    table_names = "adjNB",
    .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
    .labels = c(
      rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
      rate_ratio_ci = "Rate Ratio CI", pval = "p value"
    )
  )

build_table(lyt = lyt, df = anl)
#>                                                  A: Drug X            B: Placebo         C: Combination  
#>                                                    (N=69)               (N=73)               (N=58)      
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Number of exacerbations per patient                                                                      
#>   0                                              3 (4.35%)            8 (10.96%)           6 (10.34%)    
#>   1                                             11 (15.94%)           9 (12.33%)           6 (10.34%)    
#>   2                                             18 (26.09%)          15 (20.55%)           9 (15.52%)    
#>   3                                             14 (20.29%)          11 (15.07%)          15 (25.86%)    
#>   4                                             10 (14.49%)           9 (12.33%)           9 (15.52%)    
#>   5                                              7 (10.14%)           9 (12.33%)           8 (13.79%)    
#>   6                                              4 (5.80%)            4 (5.48%)            4 (6.90%)     
#>   7                                              2 (2.90%)            8 (10.96%)           0 (0.00%)     
#>   10                                             0 (0.00%)            0 (0.00%)            1 (1.72%)     
#> Adjusted (P) exacerbation rate (per year)                                                                
#>   Rate                                             8.2061               9.1554               7.8551      
#> Adjusted (QP) exacerbation rate (per year)                                                               
#>   Rate                                             3.1214               3.4860               2.6152      
#>     Rate CI                                   (1.7307, 5.6294)     (1.9833, 6.1272)     (1.3661, 5.0065) 
#>   Rate Ratio                                       0.8954                                    0.7502      
#>     Rate Ratio CI                             (0.3975, 2.0170)                          (0.3067, 1.8348) 
#>     p value                                        0.7897                                    0.5288      
#> Adjusted (NB) exacerbation rate (per year)                                                               
#>   Rate                                            33.0138              37.3569              34.9046      
#>     Rate CI                                  (21.6999, 50.2266)   (24.5906, 56.7507)   (23.0925, 52.7587)
#>   Rate Ratio                                       0.8837                                    0.9344      
#>     Rate Ratio CI                             (0.5872, 1.3300)                          (0.6063, 1.4400) 
#>     p value                                        0.5534                                    0.7583      
```
