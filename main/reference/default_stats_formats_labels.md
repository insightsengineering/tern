# Get default statistical methods and their associated formats, labels, and indent modifiers

**\[experimental\]**

Utility functions to get valid statistic methods for different method
groups (`.stats`) and their associated formats (`.formats`), labels
(`.labels`), and indent modifiers (`.indent_mods`). This utility is used
across `tern`, but some of its working principles can be seen in
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).
See notes to understand why this is experimental.

## Usage

``` r
get_stats(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE
)

get_stat_names(stat_results, stat_names_in = NULL)

get_formats_from_stats(
  stats,
  formats_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = tern_default_formats
)

get_labels_from_stats(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL,
  tern_defaults = tern_default_labels
)

get_indents_from_stats(
  stats,
  indents_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = as.list(rep(0L, length(stats))) %>% setNames(stats),
  row_nms = lifecycle::deprecated()
)

tern_default_stats

tern_default_formats

tern_default_labels

summary_formats(type = "numeric", include_pval = FALSE)

summary_labels(type = "numeric", include_pval = FALSE)
```

## Format

- `tern_default_stats` is a named list of available statistics, with
  each element named for their corresponding statistical method group.

&nbsp;

- `tern_default_formats` is a named vector of available default formats,
  with each element named for their corresponding statistic.

&nbsp;

- `tern_default_labels` is a named `character` vector of available
  default labels, with each element named for their corresponding
  statistic.

## Arguments

- method_groups:

  (`character`)  
  indicates the statistical method group (`tern` analyze function) to
  retrieve default statistics for. A character vector can be used to
  specify more than one statistical method group.

- stats_in:

  (`character`)  
  statistics to retrieve for the selected method group. If custom
  statistical functions are used, `stats_in` needs to have them in too.

- custom_stats_in:

  (`character`)  
  custom statistics to add to the default statistics.

- add_pval:

  (`flag`)  
  should `"pval"` (or `"pval_counts"` if `method_groups` contains
  `"analyze_vars_counts"`) be added to the statistical methods?

- stat_results:

  (`list`)  
  list of statistical results. It should be used close to the end of a
  statistical function. See examples for a structure with two
  statistical results and two groups.

- stat_names_in:

  (`character`)  
  custom modification of statistical values.

- stats:

  (`character`)  
  statistical methods to return defaults for.

- formats_in:

  (named `vector`)  
  custom formats to use instead of defaults. Can be a character vector
  with values from
  [`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  or custom format functions. Defaults to `NULL` for any rows with no
  value is provided. See Details.

- levels_per_stats:

  (named `list` of `character` or `NULL`)  
  named list where the name of each element is a statistic from `stats`
  and each element is the levels of a `factor` or `character` variable
  (or variable name), each corresponding to a single row, for which the
  named statistic should be calculated for. If a statistic is only
  calculated once (one row), the element can be either `NULL` or the
  name of the statistic. Each list element will be flattened such that
  the names of the list elements returned by the function have the
  format `statistic.level` (or just `statistic` for statistics
  calculated for a single row). Defaults to `NULL`.

- tern_defaults:

  (`list` or `vector`)  
  defaults to use to fill in missing values if no user input is given.
  Must be of the same type as the values that are being filled in (e.g.
  indentation must be integers).

- labels_in:

  (named `character`)  
  custom labels to use instead of defaults. If no value is provided, the
  variable level (if rows correspond to levels of a variable) or
  statistic name will be used as label.

- label_attr_from_stats:

  (named `list`)  
  if `labels_in = NULL`, then this will be used instead. It is a list of
  values defined in statistical functions as default labels. Values are
  ignored if `labels_in` is provided or `""` values are provided.

- indents_in:

  (named `integer`)  
  custom row indent modifiers to use instead of defaults. Defaults to
  `0L` for all values.

- row_nms:

  **\[deprecated\]** Deprecation cycle started. See the
  `levels_per_stats` parameter for details.

- type:

  (`string`)  
  `"numeric"` or `"counts"`.

- include_pval:

  (`flag`)  
  same as the `add_pval` argument in `get_stats()`.

## Value

- `get_stats()` returns a `character` vector of statistical methods.

&nbsp;

- `get_stat_names()` returns a named list of `character` vectors,
  indicating the names of statistical outputs.

&nbsp;

- `get_formats_from_stats()` returns a named list of formats as strings
  or functions.

&nbsp;

- `get_labels_from_stats()` returns a named list of labels as strings.

&nbsp;

- `get_indents_from_stats()` returns a named list of indentation
  modifiers as integers.

&nbsp;

- `summary_formats()` returns a named `vector` of default statistic
  formats for the given data type.

&nbsp;

- `summary_labels` returns a named `vector` of default statistic labels
  for the given data type.

## Details

Current choices for `type` are `counts` and `numeric` for
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
and affect `get_stats()`.

if `formats_in` is `"default"`, instead of populating the return value
with tern defaults, the return value will specify the `"default"` format
for each element. This is useful primarily when formatting behavior
should be inherited from a format specified via the `format` or
`formats_var` argument to `analyze`.

`summary_*` quick get functions for labels or formats uses `get_stats`
and `get_labels_from_stats` or `get_formats_from_stats` respectively to
retrieve relevant information.

## Functions

- `get_stats()`: Get statistics available for a given method group
  (analyze function). To check available defaults see
  `tern::tern_default_stats` list.

- `get_stat_names()`: Get statistical *names* available for a given
  method group (analyze function). Please use the `s_*` functions to get
  the statistical names.

- `get_formats_from_stats()`: Get formats corresponding to a list of
  statistics. To check available defaults see list
  `tern::tern_default_formats`.

- `get_labels_from_stats()`: Get labels corresponding to a list of
  statistics. To check for available defaults see list
  `tern::tern_default_labels`.

- `get_indents_from_stats()`: Get row indent modifiers corresponding to
  a list of statistics/rows.

- `tern_default_stats`: Named list of available statistics by method
  group for `tern`.

- `tern_default_formats`: Named vector of default formats for `tern`.

- `tern_default_labels`: Named `character` vector of default labels for
  `tern`.

- `summary_formats()`: Quick function to retrieve default formats for
  summary statistics:
  [`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
  and
  [`analyze_vars_in_cols()`](https://insightsengineering.github.io/tern/reference/analyze_vars_in_cols.md)
  principally.

- `summary_labels()`: Quick function to retrieve default labels for
  summary statistics. Returns labels of descriptive statistics which are
  understood by `rtables`. Similar to `summary_formats`.

## Note

These defaults are experimental because we use the names of functions to
retrieve the default statistics. This should be generalized in groups of
methods according to more reasonable groupings.

Formats in `tern` and `rtables` can be functions that take in the table
cell value and return a string. This is well documented in
[`vignette("custom_appearance", package = "rtables")`](https://insightsengineering.github.io/rtables/latest-tag/articles/custom_appearance.html).

## See also

[formatting_functions](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
# analyze_vars is numeric
num_stats <- get_stats("analyze_vars_numeric") # also the default

# Other type
cnt_stats <- get_stats("analyze_vars_counts")

# Weirdly taking the pval from count_occurrences
only_pval <- get_stats("count_occurrences", add_pval = TRUE, stats_in = "pval")

# All count_occurrences
all_cnt_occ <- get_stats("count_occurrences")

# Multiple
get_stats(c("count_occurrences", "analyze_vars_counts"))
#> [1] "count"                   "count_fraction"         
#> [3] "count_fraction_fixed_dp" "fraction"               
#> [5] "n"                       "n_blq"                  

stat_results <- list(
  "n" = list("M" = 1, "F" = 2),
  "count_fraction" = list("M" = c(1, 0.2), "F" = c(2, 0.1))
)
get_stat_names(stat_results)
#> $n
#> [1] "M" "F"
#> 
#> $count_fraction
#> [1] "M" "F"
#> 
get_stat_names(stat_results, list("n" = "argh"))
#> $n
#> [1] "argh"
#> 
#> $count_fraction
#> [1] "M" "F"
#> 

# Defaults formats
get_formats_from_stats(num_stats)
#> $n
#> [1] "xx."
#> 
#> $sum
#> [1] "xx.x"
#> 
#> $mean
#> [1] "xx.x"
#> 
#> $sd
#> [1] "xx.x"
#> 
#> $se
#> [1] "xx.x"
#> 
#> $mean_sd
#> [1] "xx.x (xx.x)"
#> 
#> $mean_se
#> [1] "xx.x (xx.x)"
#> 
#> $mean_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_sei
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_sdi
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_pval
#> [1] "x.xxxx | (<0.0001)"
#> 
#> $median
#> [1] "xx.x"
#> 
#> $mad
#> [1] "xx.x"
#> 
#> $median_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $quantiles
#> [1] "xx.x - xx.x"
#> 
#> $iqr
#> [1] "xx.x"
#> 
#> $range
#> [1] "xx.x - xx.x"
#> 
#> $min
#> [1] "xx.x"
#> 
#> $max
#> [1] "xx.x"
#> 
#> $median_range
#> [1] "xx.x (xx.x - xx.x)"
#> 
#> $cv
#> [1] "xx.x"
#> 
#> $geom_mean
#> [1] "xx.x"
#> 
#> $geom_sd
#> [1] "xx.x"
#> 
#> $geom_mean_sd
#> [1] "xx.x (xx.x)"
#> 
#> $geom_mean_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $geom_cv
#> [1] "xx.x"
#> 
#> $median_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
#> $mean_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
#> $geom_mean_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
get_formats_from_stats(cnt_stats)
#> $n
#> [1] "xx."
#> 
#> $count
#> [1] "xx."
#> 
#> $count_fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else {
#>     paste0(x[1], " (", round(x[2] * 100, 1), "%)")
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $count_fraction_fixed_dp
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else if (.is_equal_float(x[2], 1)) {
#>     sprintf("%d (100%%)", x[1])
#>   } else {
#>     sprintf("%d (%.1f%%)", x[1], x[2] * 100)
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#>   checkmate::assert_vector(x)
#>   checkmate::assert_count(x["num"])
#>   checkmate::assert_count(x["denom"])
#> 
#>   result <- if (x["num"] == 0) {
#>     paste0(x["num"], "/", x["denom"])
#>   } else {
#>     paste0(
#>       x["num"], "/", x["denom"],
#>       " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
#>     )
#>   }
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $n_blq
#> [1] "xx."
#> 
get_formats_from_stats(only_pval)
#> $pval
#> [1] "x.xxxx | (<0.0001)"
#> 
get_formats_from_stats(all_cnt_occ)
#> $count
#> [1] "xx."
#> 
#> $count_fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else {
#>     paste0(x[1], " (", round(x[2] * 100, 1), "%)")
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $count_fraction_fixed_dp
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else if (.is_equal_float(x[2], 1)) {
#>     sprintf("%d (100%%)", x[1])
#>   } else {
#>     sprintf("%d (%.1f%%)", x[1], x[2] * 100)
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#>   checkmate::assert_vector(x)
#>   checkmate::assert_count(x["num"])
#>   checkmate::assert_count(x["denom"])
#> 
#>   result <- if (x["num"] == 0) {
#>     paste0(x["num"], "/", x["denom"])
#>   } else {
#>     paste0(
#>       x["num"], "/", x["denom"],
#>       " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
#>     )
#>   }
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 

# Addition of customs
get_formats_from_stats(all_cnt_occ, formats_in = c("fraction" = c("xx")))
#> $count
#> [1] "xx."
#> 
#> $count_fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else {
#>     paste0(x[1], " (", round(x[2] * 100, 1), "%)")
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $count_fraction_fixed_dp
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else if (.is_equal_float(x[2], 1)) {
#>     sprintf("%d (100%%)", x[1])
#>   } else {
#>     sprintf("%d (%.1f%%)", x[1], x[2] * 100)
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $fraction
#> [1] "xx"
#> 
get_formats_from_stats(all_cnt_occ, formats_in = list("fraction" = c("xx.xx", "xx")))
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> $count
#> [1] "xx."
#> 
#> $count_fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else {
#>     paste0(x[1], " (", round(x[2] * 100, 1), "%)")
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $count_fraction_fixed_dp
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else if (.is_equal_float(x[2], 1)) {
#>     sprintf("%d (100%%)", x[1])
#>   } else {
#>     sprintf("%d (%.1f%%)", x[1], x[2] * 100)
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $fraction
#> [1] "xx.xx" "xx"   
#> 
#> [[5]]
#> NULL
#> 

# Defaults labels
get_labels_from_stats(num_stats)
#> $n
#> [1] "n"
#> 
#> $sum
#> [1] "Sum"
#> 
#> $mean
#> [1] "Mean"
#> 
#> $sd
#> [1] "SD"
#> 
#> $se
#> [1] "SE"
#> 
#> $mean_sd
#> [1] "Mean (SD)"
#> 
#> $mean_se
#> [1] "Mean (SE)"
#> 
#> $mean_ci
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_pval
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> [1] "Median"
#> 
#> $mad
#> [1] "Median Absolute Deviation"
#> 
#> $median_ci
#> [1] "Median 95% CI"
#> 
#> $quantiles
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> [1] "IQR"
#> 
#> $range
#> [1] "Min - Max"
#> 
#> $min
#> [1] "Minimum"
#> 
#> $max
#> [1] "Maximum"
#> 
#> $median_range
#> [1] "Median (Min - Max)"
#> 
#> $cv
#> [1] "CV (%)"
#> 
#> $geom_mean
#> [1] "Geometric Mean"
#> 
#> $geom_sd
#> [1] "Geometric SD"
#> 
#> $geom_mean_sd
#> [1] "Geometric Mean (SD)"
#> 
#> $geom_mean_ci
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#> [1] "CV % Geometric Mean"
#> 
#> $median_ci_3d
#> [1] "Median (95% CI)"
#> 
#> $mean_ci_3d
#> [1] "Mean (95% CI)"
#> 
#> $geom_mean_ci_3d
#> [1] "Geometric Mean (95% CI)"
#> 
get_labels_from_stats(cnt_stats)
#> $n
#> [1] "n"
#> 
#> $count
#> [1] "count"
#> 
#> $count_fraction
#> [1] "count_fraction"
#> 
#> $count_fraction_fixed_dp
#> [1] "count_fraction_fixed_dp"
#> 
#> $fraction
#> [1] "fraction"
#> 
#> $n_blq
#> [1] "n_blq"
#> 
get_labels_from_stats(only_pval)
#> $pval
#> [1] "p-value (t-test)"
#> 
get_labels_from_stats(all_cnt_occ)
#> $count
#> [1] "count"
#> 
#> $count_fraction
#> [1] "count_fraction"
#> 
#> $count_fraction_fixed_dp
#> [1] "count_fraction_fixed_dp"
#> 
#> $fraction
#> [1] "fraction"
#> 

# Addition of customs
get_labels_from_stats(all_cnt_occ, labels_in = c("fraction" = "Fraction"))
#> $count
#> [1] "count"
#> 
#> $count_fraction
#> [1] "count_fraction"
#> 
#> $count_fraction_fixed_dp
#> [1] "count_fraction_fixed_dp"
#> 
#> $fraction
#> [1] "Fraction"
#> 
get_labels_from_stats(all_cnt_occ, labels_in = list("fraction" = c("Some more fractions")))
#> $count
#> [1] "count"
#> 
#> $count_fraction
#> [1] "count_fraction"
#> 
#> $count_fraction_fixed_dp
#> [1] "count_fraction_fixed_dp"
#> 
#> $fraction
#> [1] "Some more fractions"
#> 

get_indents_from_stats(all_cnt_occ, indents_in = 3L)
#> [1] 3 3 3 3
get_indents_from_stats(all_cnt_occ, indents_in = list(count = 2L, count_fraction = 5L))
#> $count
#> [1] 2
#> 
#> $count_fraction
#> [1] 5
#> 
#> $count_fraction_fixed_dp
#> [1] 0
#> 
#> $fraction
#> [1] 0
#> 
get_indents_from_stats(
  all_cnt_occ,
  indents_in = list(a = 2L, count.a = 1L, count.b = 5L)
)
#> $count
#> [1] 0
#> 
#> $count_fraction
#> [1] 0
#> 
#> $count_fraction_fixed_dp
#> [1] 0
#> 
#> $fraction
#> [1] 0
#> 

summary_formats()
#> $n
#> [1] "xx."
#> 
#> $sum
#> [1] "xx.x"
#> 
#> $mean
#> [1] "xx.x"
#> 
#> $sd
#> [1] "xx.x"
#> 
#> $se
#> [1] "xx.x"
#> 
#> $mean_sd
#> [1] "xx.x (xx.x)"
#> 
#> $mean_se
#> [1] "xx.x (xx.x)"
#> 
#> $mean_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_sei
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_sdi
#> [1] "(xx.xx, xx.xx)"
#> 
#> $mean_pval
#> [1] "x.xxxx | (<0.0001)"
#> 
#> $median
#> [1] "xx.x"
#> 
#> $mad
#> [1] "xx.x"
#> 
#> $median_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $quantiles
#> [1] "xx.x - xx.x"
#> 
#> $iqr
#> [1] "xx.x"
#> 
#> $range
#> [1] "xx.x - xx.x"
#> 
#> $min
#> [1] "xx.x"
#> 
#> $max
#> [1] "xx.x"
#> 
#> $median_range
#> [1] "xx.x (xx.x - xx.x)"
#> 
#> $cv
#> [1] "xx.x"
#> 
#> $geom_mean
#> [1] "xx.x"
#> 
#> $geom_sd
#> [1] "xx.x"
#> 
#> $geom_mean_sd
#> [1] "xx.x (xx.x)"
#> 
#> $geom_mean_ci
#> [1] "(xx.xx, xx.xx)"
#> 
#> $geom_cv
#> [1] "xx.x"
#> 
#> $median_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
#> $mean_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
#> $geom_mean_ci_3d
#> [1] "xx.xx (xx.xx - xx.xx)"
#> 
summary_formats(type = "counts", include_pval = TRUE)
#> $n
#> [1] "xx."
#> 
#> $count
#> [1] "xx."
#> 
#> $count_fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else {
#>     paste0(x[1], " (", round(x[2] * 100, 1), "%)")
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $count_fraction_fixed_dp
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#> 
#>   if (any(is.na(x))) {
#>     return("NA")
#>   }
#> 
#>   checkmate::assert_vector(x)
#>   checkmate::assert_integerish(x[1])
#>   assert_proportion_value(x[2], include_boundaries = TRUE)
#> 
#>   result <- if (x[1] == 0) {
#>     "0"
#>   } else if (.is_equal_float(x[2], 1)) {
#>     sprintf("%d (100%%)", x[1])
#>   } else {
#>     sprintf("%d (%.1f%%)", x[1], x[2] * 100)
#>   }
#> 
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $fraction
#> function(x, ...) {
#>   attr(x, "label") <- NULL
#>   checkmate::assert_vector(x)
#>   checkmate::assert_count(x["num"])
#>   checkmate::assert_count(x["denom"])
#> 
#>   result <- if (x["num"] == 0) {
#>     paste0(x["num"], "/", x["denom"])
#>   } else {
#>     paste0(
#>       x["num"], "/", x["denom"],
#>       " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
#>     )
#>   }
#>   return(result)
#> }
#> <environment: namespace:tern>
#> 
#> $n_blq
#> [1] "xx."
#> 
#> $pval_counts
#> [1] "x.xxxx | (<0.0001)"
#> 

summary_labels()
#> $n
#> [1] "n"
#> 
#> $sum
#> [1] "Sum"
#> 
#> $mean
#> [1] "Mean"
#> 
#> $sd
#> [1] "SD"
#> 
#> $se
#> [1] "SE"
#> 
#> $mean_sd
#> [1] "Mean (SD)"
#> 
#> $mean_se
#> [1] "Mean (SE)"
#> 
#> $mean_ci
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_pval
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> [1] "Median"
#> 
#> $mad
#> [1] "Median Absolute Deviation"
#> 
#> $median_ci
#> [1] "Median 95% CI"
#> 
#> $quantiles
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> [1] "IQR"
#> 
#> $range
#> [1] "Min - Max"
#> 
#> $min
#> [1] "Minimum"
#> 
#> $max
#> [1] "Maximum"
#> 
#> $median_range
#> [1] "Median (Min - Max)"
#> 
#> $cv
#> [1] "CV (%)"
#> 
#> $geom_mean
#> [1] "Geometric Mean"
#> 
#> $geom_sd
#> [1] "Geometric SD"
#> 
#> $geom_mean_sd
#> [1] "Geometric Mean (SD)"
#> 
#> $geom_mean_ci
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#> [1] "CV % Geometric Mean"
#> 
#> $median_ci_3d
#> [1] "Median (95% CI)"
#> 
#> $mean_ci_3d
#> [1] "Mean (95% CI)"
#> 
#> $geom_mean_ci_3d
#> [1] "Geometric Mean (95% CI)"
#> 
summary_labels(type = "counts", include_pval = TRUE)
#> $n
#> [1] "n"
#> 
#> $count
#> [1] "count"
#> 
#> $count_fraction
#> [1] "count_fraction"
#> 
#> $count_fraction_fixed_dp
#> [1] "count_fraction_fixed_dp"
#> 
#> $fraction
#> [1] "fraction"
#> 
#> $n_blq
#> [1] "n_blq"
#> 
#> $pval_counts
#> [1] "p-value (chi-squared test)"
#> 
```
