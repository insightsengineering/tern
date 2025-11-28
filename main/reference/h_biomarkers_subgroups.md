# Helper functions for tabulation of a single biomarker result

**\[deprecated\]**

## Usage

``` r
h_tab_one_biomarker(
  df,
  afuns,
  colvars,
  na_str = default_na_str(),
  ...,
  .stats = NULL,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

h_tab_rsp_one_biomarker(
  df,
  vars,
  na_str = default_na_str(),
  .indent_mods = 0L,
  ...
)

h_tab_surv_one_biomarker(
  df,
  vars,
  time_unit,
  na_str = default_na_str(),
  .indent_mods = 0L,
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  results for a single biomarker. For `h_tab_rsp_one_biomarker()`, the
  results returned by
  [`extract_rsp_biomarkers()`](https://insightsengineering.github.io/tern/reference/extract_rsp_biomarkers.md).
  For `h_tab_surv_one_biomarker()`, the results returned by
  [`extract_survival_biomarkers()`](https://insightsengineering.github.io/tern/reference/extract_survival_biomarkers.md).

- afuns:

  (named `list` of `function`)  
  analysis functions.

- colvars:

  (named `list`)  
  named list with elements `vars` (variables to tabulate) and `labels`
  (their labels).

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

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

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- time_unit:

  (`string`)  
  label with unit of median survival time. Default `NULL` skips
  displaying unit.

## Value

An `rtables` table object with statistics in columns.

## Functions

- `h_tab_one_biomarker()`: Helper function to calculate statistics in
  columns for one biomarker.

- `h_tab_rsp_one_biomarker()`: Helper function that prepares a single
  response sub-table given the results for a single biomarker.

- `h_tab_surv_one_biomarker()`: Helper function that prepares a single
  survival sub-table given the results for a single biomarker.

## Examples

``` r
library(dplyr)
library(forcats)

adrs <- tern_ex_adrs
adrs_labels <- formatters::var_labels(adrs)

adrs_f <- adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  mutate(rsp = AVALC == "CR")
formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")

# For a single population, separately estimate the effects of two biomarkers.
df <- h_logistic_mult_cont_df(
  variables = list(
    rsp = "rsp",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX"
  ),
  data = adrs_f
)

# Starting from above `df`, zoom in on one biomarker and add required columns.
df1 <- df[1, ]
df1$subgroup <- "All patients"
df1$row_type <- "content"
df1$var <- "ALL"
df1$var_label <- "All patients"

h_tab_rsp_one_biomarker(
  df1,
  vars = c("n_tot", "n_rsp", "prop", "or", "ci", "pval")
)
#> Warning: `h_tab_rsp_one_biomarker()` was deprecated in tern 0.9.8.
#> ℹ This function is no longer used within `tern`.
#> Warning: `h_tab_one_biomarker()` was deprecated in tern 0.9.8.
#> ℹ This function is no longer used within `tern`.
#> ℹ The deprecated feature was likely used in the tern package.
#>   Please report the issue at
#>   <https://github.com/insightsengineering/tern/issues>.
#>                Total n   Responders   Response (%)   Odds Ratio      95% CI      p-value (Wald)
#> ———————————————————————————————————————————————————————————————————————————————————————————————
#> All patients     200        164          82.0%          0.98      (0.88, 1.08)       0.6353    

adtte <- tern_ex_adtte

# Save variable labels before data processing steps.
adtte_labels <- formatters::var_labels(adtte, fill = FALSE)

adtte_f <- adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVALU = as.character(AVALU),
    is_event = CNSR == 0
  )
labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
formatters::var_labels(adtte_f)[names(labels)] <- labels

# For a single population, separately estimate the effects of two biomarkers.
df <- h_coxreg_mult_cont_df(
  variables = list(
    tte = "AVAL",
    is_event = "is_event",
    biomarkers = c("BMRKR1", "AGE"),
    covariates = "SEX",
    strata = c("STRATA1", "STRATA2")
  ),
  data = adtte_f
)

# Starting from above `df`, zoom in on one biomarker and add required columns.
df1 <- df[1, ]
df1$subgroup <- "All patients"
df1$row_type <- "content"
df1$var <- "ALL"
df1$var_label <- "All patients"
h_tab_surv_one_biomarker(
  df1,
  vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
  time_unit = "days"
)
#> Warning: `h_tab_surv_one_biomarker()` was deprecated in tern 0.9.8.
#> ℹ This function is no longer used within `tern`.
#>                Total n   Total Events   Median (days)   Hazard Ratio   95% Wald CI    p-value (Wald)
#> ————————————————————————————————————————————————————————————————————————————————————————————————————
#> All patients     200         141            753.5           1.00       (0.95, 1.05)       0.9941    
```
