# Count patient events in columns

**\[stable\]**

The summarize function `summarize_patients_events_in_cols()` creates a
layout element to summarize patient event counts in columns.

This function analyzes the elements (events) supplied via the
`filters_list` parameter and returns a row with counts of number of
patients for each event as well as the total numbers of patients and
events. The `id` variable is used to indicate unique subject identifiers
(defaults to `USUBJID`).

If there are multiple occurrences of the same event recorded for a
patient, the event is only counted once.

## Usage

``` r
summarize_patients_events_in_cols(
  lyt,
  id = "USUBJID",
  filters_list = list(),
  empty_stats = character(),
  na_str = default_na_str(),
  ...,
  .stats = c("unique", "all", names(filters_list)),
  .labels = c(unique = "Patients (All)", all = "Events (All)",
    labels_or_names(filters_list)),
  col_split = TRUE
)

s_count_patients_and_multiple_events(
  df,
  id,
  filters_list,
  empty_stats = character(),
  labelstr = "",
  custom_label = NULL
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- id:

  (`string`)  
  subject variable name.

- filters_list:

  (named `list` of `character`)  
  list where each element in this list describes one type of event
  describe by filters, in the same format as
  [`s_count_patients_with_event()`](https://insightsengineering.github.io/tern/reference/count_patients_with_event.md).
  If it has a label, then this will be used for the column title.

- empty_stats:

  (`character`)  
  optional names of the statistics that should be returned empty such
  that corresponding table cells will stay blank.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

  In addition to any statistics added using `filters_list`, statistic
  options are: `'unique', 'all'`

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- col_split:

  (`flag`)  
  whether the columns should be split. Set to `FALSE` when the required
  column split has been done already earlier in the layout pipe.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- custom_label:

  (`string` or `NULL`)  
  if provided and `labelstr` is empty then this will be used as label.

## Value

- `summarize_patients_events_in_cols()` returns a layout object suitable
  for passing to further layouting functions, or to
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).
  Adding this function to an `rtable` layout will add formatted content
  rows containing the statistics from
  `s_count_patients_and_multiple_events()` to the table layout.

&nbsp;

- `s_count_patients_and_multiple_events()` returns a list with the
  statistics:

  - `unique`: number of unique patients in `df`.

  - `all`: number of rows in `df`.

  - one element with the same name as in `filters_list`: number of rows
    in `df`, i.e. events, fulfilling the filter condition.

## Functions

- `summarize_patients_events_in_cols()`: Layout-creating function which
  can take statistics function arguments and additional format
  arguments. This function is a wrapper for
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

- `s_count_patients_and_multiple_events()`: Statistics function which
  counts numbers of patients and multiple events defined by filters.
  Used as analysis function `afun` in
  `summarize_patients_events_in_cols()`.

## Examples

``` r
df <- data.frame(
  USUBJID = rep(c("id1", "id2", "id3", "id4"), c(2, 3, 1, 1)),
  ARM = c("A", "A", "B", "B", "B", "B", "A"),
  AESER = rep("Y", 7),
  AESDTH = c("Y", "Y", "N", "Y", "Y", "N", "N"),
  AEREL = c("Y", "Y", "N", "Y", "Y", "N", "Y"),
  AEDECOD = c("A", "A", "A", "B", "B", "C", "D"),
  AEBODSYS = rep(c("SOC1", "SOC2", "SOC3"), c(3, 3, 1))
)

# `summarize_patients_events_in_cols()`
basic_table() %>%
  summarize_patients_events_in_cols(
    filters_list = list(
      related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
      fatal = c(AESDTH = "Y"),
      fatal_related = c(AEREL = "Y", AESDTH = "Y")
    ),
    custom_label = "%s Total number of patients and events"
  ) %>%
  build_table(df)
#>                                          Patients (All)   Events (All)   Events (Related)   fatal   fatal_related
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> %s Total number of patients and events         4               7                5             4           4      
```
