# Count number of patients and sum exposure across all patients in columns

**\[stable\]**

The analyze function `analyze_patients_exposure_in_cols()` creates a
layout element to count total numbers of patients and sum an analysis
value (i.e. exposure) across all patients in columns.

The primary analysis variable `ex_var` is the exposure variable used to
calculate the `sum_exposure` statistic. The `id` variable is used to
uniquely identify patients in the data such that only unique patients
are counted in the `n_patients` statistic, and the `var` variable is
used to create a row split if needed. The percentage returned as part of
the `n_patients` statistic is the proportion of all records that
correspond to a unique patient.

The summarize function `summarize_patients_exposure_in_cols()` performs
the same function as `analyze_patients_exposure_in_cols()` except it
creates content rows, not data rows, to summarize the current table
row/column context and operates on the level of the latest row split or
the root of the table if no row splits have occurred.

If a column split has not yet been performed in the table, `col_split`
must be set to `TRUE` for the first call of
`analyze_patients_exposure_in_cols()` or
`summarize_patients_exposure_in_cols()`.

## Usage

``` r
analyze_patients_exposure_in_cols(
  lyt,
  var = NULL,
  ex_var = "AVAL",
  id = "USUBJID",
  add_total_level = FALSE,
  custom_label = NULL,
  col_split = TRUE,
  na_str = default_na_str(),
  .stats = c("n_patients", "sum_exposure"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
  .indent_mods = NULL,
  ...
)

summarize_patients_exposure_in_cols(
  lyt,
  var,
  ex_var = "AVAL",
  id = "USUBJID",
  add_total_level = FALSE,
  custom_label = NULL,
  col_split = TRUE,
  na_str = default_na_str(),
  ...,
  .stats = c("n_patients", "sum_exposure"),
  .stat_names = NULL,
  .formats = NULL,
  .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
  .indent_mods = NULL
)

s_count_patients_sum_exposure(
  df,
  labelstr = "",
  .stats = c("n_patients", "sum_exposure"),
  .N_col,
  ...,
  ex_var = "AVAL",
  id = "USUBJID",
  custom_label = NULL,
  var_level = NULL
)

a_count_patients_sum_exposure(
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

- var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- ex_var:

  (`string`)  
  name of the variable in `df` containing exposure values.

- id:

  (`string`)  
  subject variable name.

- add_total_level:

  (`flag`)  
  adds a "total" level after the others which includes all the levels
  that constitute the split. A custom label can be set for this level
  via the `custom_label` argument.

- custom_label:

  (`string` or `NULL`)  
  if provided and `labelstr` is empty, this will be used as label.

- col_split:

  (`flag`)  
  whether the columns should be split. Set to `FALSE` when the required
  column split has been done already earlier in the layout pipe.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options are: `'n_patients', 'sum_exposure'`

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

- ...:

  additional arguments for the lower level functions.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

## Value

- `analyze_patients_exposure_in_cols()` returns a layout object suitable
  for passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted data
  rows, with the statistics from `s_count_patients_sum_exposure()`
  arranged in columns, to the table layout.

&nbsp;

- `summarize_patients_exposure_in_cols()` returns a layout object
  suitable for passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted content
  rows, with the statistics from `s_count_patients_sum_exposure()`
  arranged in columns, to the table layout.

&nbsp;

- `s_count_patients_sum_exposure()` returns a named `list` with the
  statistics:

  - `n_patients`: Number of unique patients in `df`.

  - `sum_exposure`: Sum of `ex_var` across all patients in `df`.

&nbsp;

- `a_count_patients_sum_exposure()` returns formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `analyze_patients_exposure_in_cols()`: Layout-creating function which
  can take statistics function arguments and additional format
  arguments. This function is a wrapper for
  [`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)
  and
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html).

- `summarize_patients_exposure_in_cols()`: Layout-creating function
  which can take statistics function arguments and additional format
  arguments. This function is a wrapper for
  [`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html)
  and
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_count_patients_sum_exposure()`: Statistics function which counts
  numbers of patients and the sum of exposure across all patients.

- `a_count_patients_sum_exposure()`: Analysis function which is used as
  `afun` in
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html)
  within `analyze_patients_exposure_in_cols()` and as `cfun` in
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  within `summarize_patients_exposure_in_cols()`.

## Note

As opposed to `summarize_patients_exposure_in_cols()` which generates
content rows, `analyze_patients_exposure_in_cols()` generates data rows
which will *not* be repeated on multiple pages when pagination is used.

## Examples

``` r
set.seed(1)
df <- data.frame(
  USUBJID = c(paste("id", seq(1, 12), sep = "")),
  ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
  SEX = c(rep("Female", 6), rep("Male", 6)),
  AVAL = as.numeric(sample(seq(1, 20), 12)),
  stringsAsFactors = TRUE
)
adsl <- data.frame(
  USUBJID = c(paste("id", seq(1, 12), sep = "")),
  ARMCD = c(rep("ARM A", 2), rep("ARM B", 2)),
  SEX = c(rep("Female", 2), rep("Male", 2)),
  stringsAsFactors = TRUE
)

lyt <- basic_table() %>%
  split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
  summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE) %>%
  analyze_patients_exposure_in_cols(var = "SEX", col_split = FALSE)
result <- build_table(lyt, df = df, alt_counts_df = adsl)
result
#>                                               ARM A                      ARM B                       Total          
#>                                       Patients    Person time    Patients    Person time    Patients     Person time
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total patients numbers/person time   6 (100.0%)       46        6 (100.0%)       68        12 (100.0%)       114    
#>   Female                             6 (100.0%)       46         0 (0.0%)         0         6 (50.0%)        46     
#>   Male                                0 (0.0%)         0        6 (100.0%)       68         6 (50.0%)        68     

lyt2 <- basic_table() %>%
  split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
  summarize_patients_exposure_in_cols(
    var = "AVAL", col_split = TRUE,
    .stats = "n_patients", custom_label = "some custom label"
  ) %>%
  analyze_patients_exposure_in_cols(var = "SEX", col_split = FALSE, ex_var = "AVAL")
result2 <- build_table(lyt2, df = df, alt_counts_df = adsl)
result2
#>                       ARM A        ARM B         Total   
#>                      Patients     Patients     Patients  
#> —————————————————————————————————————————————————————————
#> some custom label   6 (100.0%)   6 (100.0%)   12 (100.0%)
#>   Female            6 (100.0%)    0 (0.0%)     6 (50.0%) 
#>   Male               0 (0.0%)    6 (100.0%)    6 (50.0%) 

lyt3 <- basic_table() %>%
  analyze_patients_exposure_in_cols(var = "SEX", col_split = TRUE, ex_var = "AVAL")
result3 <- build_table(lyt3, df = df, alt_counts_df = adsl)
result3
#>          Patients    Person time
#> ————————————————————————————————
#> Female   6 (50.0%)       46     
#> Male     6 (50.0%)       68     

# Adding total levels and custom label
lyt4 <- basic_table(
  show_colcounts = TRUE
) %>%
  analyze_patients_exposure_in_cols(
    var = "ARMCD",
    col_split = TRUE,
    add_total_level = TRUE,
    custom_label = "TOTAL"
  ) %>%
  append_topleft(c("", "Sex"))

result4 <- build_table(lyt4, df = df, alt_counts_df = adsl)
result4
#>          Patients     Person time
#> Sex       (N=12)        (N=12)   
#> —————————————————————————————————
#> ARM A    6 (50.0%)        46     
#> ARM B    6 (50.0%)        68     
#> TOTAL   12 (100.0%)       114    

lyt5 <- basic_table() %>%
  summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE)

result5 <- build_table(lyt5, df = df, alt_counts_df = adsl)
result5
#>                                       Patients     Person time
#> ——————————————————————————————————————————————————————————————
#> Total patients numbers/person time   12 (100.0%)       114    

lyt6 <- basic_table() %>%
  summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE, .stats = "sum_exposure")

result6 <- build_table(lyt6, df = df, alt_counts_df = adsl)
result6
#>                                      Person time
#> ————————————————————————————————————————————————
#> Total patients numbers/person time       114    
```
