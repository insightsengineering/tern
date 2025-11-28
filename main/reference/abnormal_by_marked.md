# Count patients with marked laboratory abnormalities

**\[stable\]**

The analyze function `count_abnormal_by_marked()` creates a layout
element to count patients with marked laboratory abnormalities for each
direction of abnormality, categorized by parameter value.

This function analyzes primary analysis variable `var` which indicates
whether a single, replicated, or last marked laboratory abnormality was
observed. Levels of `var` to include for each marked lab abnormality
(`single` and `last_replicated`) can be supplied via the `category`
parameter. Additional analysis variables that can be supplied as a list
via the `variables` parameter are `id` (defaults to `USUBJID`), a
variable to indicate unique subject identifiers, `param` (defaults to
`PARAM`), a variable to indicate parameter values, and `direction`
(defaults to `abn_dir`), a variable to indicate abnormality directions.

For each combination of `param` and `direction` levels, marked lab
abnormality counts are calculated as follows:

- `Single, not last` & `Last or replicated`: The number of patients with
  `Single, not last` and `Last or replicated` values, respectively.

- `Any`: The number of patients with either single or replicated marked
  abnormalities.

Fractions are calculated by dividing the above counts by the number of
patients with at least one valid measurement recorded during the
analysis.

Prior to using this function in your table layout you must use
[`rtables::split_rows_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
to create two row splits, one on variable `param` and one on variable
`direction`.

## Usage

``` r
count_abnormal_by_marked(
  lyt,
  var,
  category = list(single = "SINGLE", last_replicated = c("LAST", "REPLICATED")),
  variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir"),
  na_str = default_na_str(),
  nested = TRUE,
  ...,
  .stats = "count_fraction",
  .stat_names = NULL,
  .formats = list(count_fraction = format_count_fraction),
  .labels = NULL,
  .indent_mods = NULL
)

s_count_abnormal_by_marked(
  df,
  .var = "AVALCAT1",
  .spl_context,
  category = list(single = "SINGLE", last_replicated = c("LAST", "REPLICATED")),
  variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir"),
  ...
)

a_count_abnormal_by_marked(
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

- category:

  (`list`)  
  a list with different marked category names for single and last or
  replicated.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

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

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'count_fraction', 'count_fraction_fixed_dp'`

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

- .var, var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

## Value

- `count_abnormal_by_marked()` returns a layout object suitable for
  passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted rows
  containing the statistics from `s_count_abnormal_by_marked()` to the
  table layout.

&nbsp;

- `s_count_abnormal_by_marked()` returns statistic `count_fraction` with
  `Single, not last`, `Last or replicated`, and `Any` results.

&nbsp;

- `a_count_abnormal_by_marked()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `count_abnormal_by_marked()`: Layout-creating function which can take
  statistics function arguments and additional format arguments. This
  function is a wrapper for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- `s_count_abnormal_by_marked()`: Statistics function for patients with
  marked lab abnormalities.

- `a_count_abnormal_by_marked()`: Formatted analysis function which is
  used as `afun` in `count_abnormal_by_marked()`.

## Note

`Single, not last` and `Last or replicated` levels are mutually
exclusive. If a patient has abnormalities that meet both the
`Single, not last` and `Last or replicated` criteria, then the patient
will be counted only under the `Last or replicated` category.

## Examples

``` r
library(dplyr)

df <- data.frame(
  USUBJID = as.character(c(rep(1, 5), rep(2, 5), rep(1, 5), rep(2, 5))),
  ARMCD = factor(c(rep("ARM A", 5), rep("ARM B", 5), rep("ARM A", 5), rep("ARM B", 5))),
  ANRIND = factor(c(
    "NORMAL", "HIGH", "HIGH", "HIGH HIGH", "HIGH",
    "HIGH", "HIGH", "HIGH HIGH", "NORMAL", "HIGH HIGH", "NORMAL", "LOW", "LOW", "LOW LOW", "LOW",
    "LOW", "LOW", "LOW LOW", "NORMAL", "LOW LOW"
  )),
  ONTRTFL = rep(c("", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), 2),
  PARAMCD = factor(c(rep("CRP", 10), rep("ALT", 10))),
  AVALCAT1 = factor(rep(c("", "", "", "SINGLE", "REPLICATED", "", "", "LAST", "", "SINGLE"), 2)),
  stringsAsFactors = FALSE
)

df <- df %>%
  mutate(abn_dir = factor(
    case_when(
      ANRIND == "LOW LOW" ~ "Low",
      ANRIND == "HIGH HIGH" ~ "High",
      TRUE ~ ""
    ),
    levels = c("Low", "High")
  ))

# Select only post-baseline records.
df <- df %>% filter(ONTRTFL == "Y")
df_crp <- df %>%
  filter(PARAMCD == "CRP") %>%
  droplevels()
full_parent_df <- list(df_crp, "not_needed")
cur_col_subset <- list(rep(TRUE, nrow(df_crp)), "not_needed")
spl_context <- data.frame(
  split = c("PARAMCD", "GRADE_DIR"),
  full_parent_df = I(full_parent_df),
  cur_col_subset = I(cur_col_subset)
)

map <- unique(
  df[df$abn_dir %in% c("Low", "High") & df$AVALCAT1 != "", c("PARAMCD", "abn_dir")]
) %>%
  lapply(as.character) %>%
  as.data.frame() %>%
  arrange(PARAMCD, abn_dir)

basic_table() %>%
  split_cols_by("ARMCD") %>%
  split_rows_by("PARAMCD") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique_count"
  ) %>%
  split_rows_by(
    "abn_dir",
    split_fun = trim_levels_to_map(map)
  ) %>%
  count_abnormal_by_marked(
    var = "AVALCAT1",
    variables = list(
      id = "USUBJID",
      param = "PARAMCD",
      direction = "abn_dir"
    )
  ) %>%
  build_table(df = df)
#>                           ARM A      ARM B  
#> ————————————————————————————————————————————
#> ALT (n)                     1          1    
#>   Low                                       
#>     Single, not last     1 (100%)      0    
#>     Last or replicated      0       1 (100%)
#>     Any Abnormality      1 (100%)   1 (100%)
#> CRP (n)                     1          1    
#>   High                                      
#>     Single, not last     1 (100%)      0    
#>     Last or replicated      0       1 (100%)
#>     Any Abnormality      1 (100%)   1 (100%)

basic_table() %>%
  split_cols_by("ARMCD") %>%
  split_rows_by("PARAMCD") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique_count"
  ) %>%
  split_rows_by(
    "abn_dir",
    split_fun = trim_levels_in_group("abn_dir")
  ) %>%
  count_abnormal_by_marked(
    var = "AVALCAT1",
    variables = list(
      id = "USUBJID",
      param = "PARAMCD",
      direction = "abn_dir"
    )
  ) %>%
  build_table(df = df)
#>                           ARM A      ARM B  
#> ————————————————————————————————————————————
#> ALT (n)                     1          1    
#>   Low                                       
#>     Single, not last     1 (100%)      0    
#>     Last or replicated      0       1 (100%)
#>     Any Abnormality      1 (100%)   1 (100%)
#> CRP (n)                     1          1    
#>   High                                      
#>     Single, not last     1 (100%)      0    
#>     Last or replicated      0       1 (100%)
#>     Any Abnormality      1 (100%)   1 (100%)
```
