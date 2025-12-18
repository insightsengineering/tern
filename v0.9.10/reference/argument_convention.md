# Standard arguments

The documentation to this function lists all the arguments in `tern`
that are used repeatedly to express an analysis.

## Arguments

- ...:

  additional arguments for the lower level functions.

- .aligns:

  (`character` or `NULL`)  
  alignment for table contents (not including labels). When `NULL`,
  `"center"` is applied. See
  [`formatters::list_valid_aligns()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  for a list of all currently supported alignments.

- .all_col_counts:

  (`integer`)  
  vector where each value represents a global count for a column. Values
  are taken from `alt_counts_df` if specified (see
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)).

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .N_col:

  (`integer(1)`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .N_row:

  (`integer(1)`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

- .stat_names:

  (`character`)  
  names of the statistics that are passed directly to name single
  statistics (`.stats`). This option is visible when producing
  [`rtables::as_result_df()`](https://insightsengineering.github.io/rtables/latest-tag/reference/data.frame_export.html)
  with `make_ard = TRUE`.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- add_total_level:

  (`flag`)  
  adds a "total" level after the others which includes all the levels
  that constitute the split. A custom label can be set for this level
  via the `custom_label` argument.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- col_by:

  (`factor`)  
  defining column groups.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- id:

  (`string`)  
  subject variable name.

- is_event:

  (`flag`)  
  `TRUE` if event, `FALSE` if time to event is censored.

- label_all:

  (`string`)  
  label for the total population analysis.

- labelstr:

  (`string`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- method:

  (`string` or `NULL`)  
  specifies the test used to calculate the p-value for the difference
  between two proportions. For options, see
  [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md).
  Default is `NULL` so no test is performed.

- na.rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure \_if possible (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- prune_zero_rows:

  (`flag`)  
  whether to prune all zero rows.

- riskdiff:

  (`flag`)  
  whether a risk difference column is present. When set to `TRUE`,
  [`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
  must be used as `split_fun` in the prior column split of the table
  layout, specifying which columns should be compared. See
  [`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
  for details on risk difference calculation.

- rsp:

  (`logical`)  
  vector indicating whether each subject is a responder or not.

- show_labels:

  (`string`)  
  label visibility: one of "default", "visible" and "hidden".

- section_div:

  (`string`)  
  string which should be repeated as a section divider after each group
  defined by this split instruction, or `NA_character_` (the default)
  for no section divider.

- table_names:

  (`character`)  
  this can be customized in the case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- tte:

  (`numeric`)  
  vector of time-to-event duration values.

- var_labels:

  (`character`)  
  variable labels.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- var:

  (`string`)  
  single variable name for the primary analysis variable.

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- xlim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the x-axis, respectively.
  If `NULL` (default), the default scale range is used.

- ylim:

  (`numeric(2)`)  
  vector containing lower and upper limits for the y-axis, respectively.
  If `NULL` (default), the default scale range is used.

## Details

Although this function just returns `NULL` it has two uses, for the
`tern` users it provides a documentation of arguments that are commonly
and consistently used in the framework. For the developer it adds a
single reference point to import the `roxygen` argument description
with: `@inheritParams argument_convention`
