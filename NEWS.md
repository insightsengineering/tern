# tern 0.9.3.9008

### New Features
* Refactored `g_forest` to output a `ggplot` object instead of a `grob` object.
* Added `h_glm_negbin` to `h_glm_count` to enable count data analysis using a negative binomial model.
* Added argument `grade_groups_only` to `count_occurrences_by_grade` to allow users to only display rows for specified grade groups.

### Bug Fixes
* Fixed nested column split label overlay issue in `rtable2gg` to clean up appearance of text labels.
* Fixed bug in `s_ancova` causing incorrect difference calculations for arm variables with irregular levels.

### Miscellaneous
* Added function `expect_snapshot_ggplot` to test setup file to process plot snapshot tests and allow plot dimensions to be set.

# tern 0.9.3

### New Features
* Added `ref_group_position` function to place the reference group facet last, first or at a certain position.
* Added `keep_level_order` split function to retain original order of levels in a split.
* Added `level_order` split function to reorder manually the levels.
* Added function `get_indents_from_stats` to format and return indent modifiers for a given set of statistics.
* Added internal utility function `apply_auto_formatting` to check for `"auto"` formats and replace them with 
  implementation of `format_auto` in analyze functions.
* Added utility function `labels_use_control` to modify labels with control specifications.
* Added list containing default statistics for each method group, `tern_default_stats`.
* Added summarize function version of `count_occurrences` analyze function, `summarize_occurrences`.
* Added referential footnotes to `surv_time` for censored range observations, controlled via the `ref_fn_censor` parameter.
* Added helper function `h_adlb_abnormal_by_worst_grade` to prepare `ADLB` data to use as input in `count_abnormal_by_worst_grade`.
* Added `s_bland_altman` function to assess agreement between two numerical vectors. 
* Added function `rtable2gg` that converts `rtable` objects to `ggplot` objects.
* Added helper function to set default `na_str` globally with `set_default_na_str()` and added `default_na_str()` for all interested functions.

### Enhancements
* Added `ref_group_coxph` parameter to `g_km` to specify the reference group used for pairwise Cox-PH calculations when `annot_coxph = TRUE`.
* Added `annot_coxph_ref_lbls` parameter to `g_km` to enable printing the reference group in table labels when `annot_coxph = TRUE`.
* Added `x_lab` parameter to `g_lineplot` to customize x-axis label.
* Remove 25% padding of y-axis in `g_lineplot`.
* Added support for creating multiple risk difference columns, each comparing to a single comparison group. Multiple comparison groups can be specified as a vector via the `arm_y` argument.
* Allowed numeric vector as `count_by` input in `analyze_num_patients` and `summarize_num_patients`.
* Aligned plot and table vertically in `g_lineplot`.

### Bug Fixes
* Fixed bug in `decorate_grob` preventing text wrapping from accounting for font size.
* Fixed implementation of `na_str` argument in all column-wise analysis and tabulation functions.

### Miscellaneous
* Specified minimal version of package dependencies.
* Upgraded `to_string_matrix` to take into account `widths` and other printing parameters.

# tern 0.9.2

### New Features
* Added the `na_str` argument to `analyze` & `summarize_row_groups` wrapper functions `count_abnormal`, `count_abnormal_by_baseline`, `count_abnormal_by_marked`, `count_abnormal_by_worst_grade`, `count_abnormal_lab_worsen_by_baseline`, `count_cumulative`, `count_missed_doses`, `count_occurrences`, `count_occurrences_by_grade`, `summarize_occurrences_by_grade`, `summarize_patients_events_in_cols`, `count_patients_with_event`, `count_patients_with_flags`, `count_values`, `estimate_multinomial_response`, `estimate_proportion`, `h_tab_one_biomarker`, `estimate_incidence_rate`, `logistic_summary_by_flag`, `estimate_odds_ratio`, `estimate_proportion_diff`, `test_proportion_diff`, `summarize_ancova`, `summarize_change`, `summarize_glm_count`, `summarize_num_patients`, `analyze_num_patients`, `summarize_patients_exposure_in_cols`, `coxph_pairwise`, `tabulate_survival_subgroups`, `surv_time`, and `surv_timepoint`.

### Enhancements
* Added formatting function `format_count_fraction_lt10` for formatting `count_fraction` with special consideration when count is less than 10.
* Updated `s_summary.logical` output for `count_fraction` when denominator is zero to display as `NA` instead of `0` in tables.
* Updated `analyze_vars_in_cols` to allow character input to indicate whether nominal time point is post-dose or pre-dose when applying the 1/3 imputation rule.

### Bug Fixes
* Fixed bug in `g_km` causing an error when converting certain annotation width units.

### Miscellaneous
* Began deprecation of `na_level` argument in `s_count_abnormal_by_baseline`, `a_summary`, `analyze_vars`, `analyze_vars_in_cols`, `compare_vars`, `h_map_for_count_abnormal`, `h_stack_by_baskets`, `summarize_colvars`, `a_coxreg`, and `summarize_coxreg` and replaced it with the `na_str` argument.
* `strata` and `cohort_id` parameters renamed to `group_var` and `subject_var` respectively in `g_lineplot` and `control_lineplot_vars`  .

# tern 0.9.1

### New Features
* Added `imputation_rule` function to apply imputation rule to data.
* Added new format function `format_sigfig` to allow for numeric value formatting by a specified number of significant figures.
* Added vectors containing default statistic formats and labels as `tern_default_formats` and `tern_default_labels`, respectively.
* Added function `get_stats` to return methods from given statistical method groups.
* Added function `get_formats_from_stats` to return formats and `get_labels_from_stats` to return labels for a given set of statistics.
* Added `"auto"` option for `.formats`. It uses `format_auto` to determine automatically the number of digits.
* Added `title` argument to `h_grob_tbl_at_risk` and `annot_at_risk_title` argument to `g_km` and `h_km_layout` which allows user to add "Patients at Risk" title to Kaplan-Meier at risk annotation table.

### Enhancements
* Refactored `tabulate_rsp_subgroups` to pass sanitation checks by preventing creation of degenerate subtables.
* Updated `analyze_vars_in_cols` to use caching, allow implementation of imputation rule via the `imp_rule` argument, and allow user to specify cell alignment via the `.aligns` argument.
* Updated `add_rowcounts` to allow addition of row counts from `alt_counts_df` using the `alt_counts` argument.
* Added `gp` argument to `g_forest` to control graphical parameters such as font size.

### Miscellaneous
* Grouped functions relating to valid method names and their default formats and labels into new source file `utils_defaults_handling.R`.
* Started deprecation of `summary_custom()` and `a_summary()` as a `S3` method.
* Renamed statistical method for `p-value` in the discrete case to `pval_counts`.
* Removed `a_summary_internal()` in favor of only one main `a_summary()`.

# tern 0.9.0

### New Features
* Added `stat_propdiff_ci` function to calculate proportion/risk difference and CI.
* Added risk difference column functionality via the `riskdiff` argument to functions `count_occurrences`, `count_occurrences_by_grade`, `count_patients_with_event`, `count_patients_with_flags`, `analyze_num_patients`, and `summarize_num_patients`.

### Enhancements
* Refactored the function `a_summary` to no longer use the helper function `create_afun_summary`.
* Refactored functions `summarize_vars` and `compare_vars` to use the refactored `a_summary` function.
* Created new internal helper functions `ungroup_stats` to ungroup statistics calculated for factor variables, and `a_summary_internal` to perform calculations for `a_summary`.

### Bug Fixes
* Fixed bug in `s_count_occurrences_by_grade` so that "missing" grade always appears as the final level.
* Fixed bug in `analyze_vars_in_cols` when categorical data was used.
* Fixed bug in `s_count_occurrences_by_grade` so that levels are not relabeled when reordering to account for "missing" grades.

### Miscellaneous
* Fixed swapped descriptions for the `.N_row` and `.N_col` parameters.
* Removed internal calls to `df_explicit_na`. Changes in `NA` values should happen externally to `tern` functions, depending on users' needs.
* Reinstated correct soft deprecation for `create_afun_summary` and `create_afun_compare`.

# tern 0.8.5

### Enhancements
* Added `ylim` argument to `g_km` to allow the user to set custom limits for the y-axis.
* Added assertion to `g_km` which checks whether there is one arm present in the data when `annot_coxph` is true.
* Added `flag_labels` argument to `s_count_patients_with_flags` to enable more label handling options in `count_patients_by_flags`.
* Added the `nested` argument to `analyze` wrapper functions `count_abnormal`, `count_abnormal_by_baseline`, `count_abnormal_by_marked`, `count_abnormal_by_worst_grade`, `count_abnormal_lab_worsen_by_baseline`, `count_cumulative`, `count_missed_doses`, `count_occurrences`, `count_occurrences_by_grade`, `count_patients_with_event`, `count_patients_with_flags`, `count_values`, `estimate_multinomial_response`, `estimate_proportion`, `estimate_incidence_rate`, `estimate_odds_ratio`, `estimate_proportion_diff`, `test_proportion_diff`, `summarize_ancova`, `summarize_change`, `summarize_glm_count`, `analyze_num_patients`, `coxph_pairwise`, `surv_time`, and `surv_timepoint`.

## Miscellaneous
* Started deprecation cycle for `summarize_vars` and `control_summarize_vars`. Renamed into `analyze_vars` and `control_analyze_vars` to reflect underlying `rtables` machinery while keeping backward compatibility with aliases.

# tern 0.8.4

### Enhancements
* Added method for `character` class to `h_coxreg_inter_effect` enabling `character` covariates in `summarize_coxreg`.

### Miscellaneous
* Began deprecation of `time_unit_input` and `time_unit_output` arguments and replaced them with the `input_time_unit` and `num_pt_year`, respectively, in `control_incidence_rate`.
* Removed deprecated `pairwise` function.
* Deprecated `a_compare` and replaced it with `a_summary` with argument `compare = TRUE`.
* Deprecated helper functions `create_afun_summary` and `create_afun_compare` which are no longer used by `a_summary` and `a_compare` respectively.

### Bug Fixes
* Fixed long double assertion check in `sum(weights)` for `M1mac` installation.

# tern 0.8.3

### Enhancements
* Added explicit zero counts to `g_km` plot "at risk" annotation tables.
* Added a flag for total level split in `analyze_patients_exposure_in_cols`.
* Implemented `.indent_mods` argument in functions `h_tab_one_biomarker`, `h_tab_rsp_one_biomarker`, `h_tab_surv_one_biomarker`, `summarize_logistic`, `logistic_summary_by_flag`, `tabulate_rsp_biomarkers`, `a_coxreg`, `summarize_coxreg`, `tabulate_survival_biomarkers`, `surv_time`, `surv_timepoint`, and `cfun_by_flag`.
* Updated `summarize_coxreg` to print covariates in data rows for univariate Cox regression with no interactions and content rows otherwise.
* Removed "baseline status" text from `d_count_abnormal_by_baseline` labels.
* Improved default sizing of annotation tables in `g_km` and added dynamic scaling of the `surv_med` and `coxph` annotation tables, with customization via the `width_annots` argument.

### Bug Fixes
* Fixed bug in `split_text_grob` preventing titles and footnotes from being properly formatted and printed by `decorate_grob`.
* Fixed bug in `g_lineplot` preventing the addition of lines to the plot when midpoint statistic calculations result in `NA` value(s).
* Fixed `tern:::tidy.glm` formals to respect `broom:::tidy.default` formals.

### Miscellaneous
* Updated `README` to include installation instructions for CRAN.
* Remove examples for unexported functions.
* Export functions `has_count_in_cols`, `has_counts_difference`, `combine_counts`, `h_tab_rsp_one_biomarker`, `arrange_grobs`, `a_count_patients_sum_exposure`, `a_coxreg`, `groups_list_to_df`, `forest_viewport`.
* Updated `README` to include installation instructions for CRAN.
* Began deprecation of `indent_mod` argument and replaced it with the `.indent_mods` argument in `summarize_num_patients` and `analyze_num_patients`.

# tern 0.8.2

### Breaking Changes
* Refactored `s_coxreg` and `summarize_coxreg`to work with new analysis function `a_coxreg`.

### Enhancements
* Added `section_div` and `na_level` arguments to `summarize_vars`.
* Added `median_range` as a numeric variable statistic option for `summarize_vars`.
* Corrected ordering of factor levels by `d_onco_rsp_label`.
* Added new "Analyze Functions", "Summarize Functions", and "Analyze Functions on Columns" overview pages.
* Consolidated all KM plot documentation within the `g_km` function.
* Added `a_count_patients_sum_exposure` for `summarize_patients_exposure_in_cols` and new analyze function `analyze_patients_exposure_in_cols`.
* Added more informative error when the user selects an invalid method for unstratified analyses in `s_proportion_diff`.
* Updated `s_summary` and `s_compare` to allow `NA` values in input variables. For factor variables with `NA`s, if `na.rm = FALSE` an explicit `NA` level will be automatically added. `na.rm = TRUE` will also consider `"<Missing>"` values and exclude them.
* Updated purpose of `na_level` parameter in `s_summary` and `s_compare` to align with other `tern` functions. Instead of being a string to consider as `NA` when setting `na.rm = TRUE`, it now defines a string to print in place of `NA` values in the output table.

### Bug Fixes
* Fixed missing label for `TRTEDTM` in `tern` datasets.
* Fixed improper implementation of `na_level` argument in `summarize_vars` preventing it from having an effect.

### Miscellaneous
* Implemented the `lubridate` package for date variables in `tern` datasets.
* Organized `.gitignore` and `.Rbuildignore` files.
* Removed deprecated `footnotes` functions and all related files.
* Started deprecation cycle for `pairwise` function.
* Moved `count_patients_with_flags` functions from `count_patients_with_event.R` to `count_patients_with_flags.R`.

# tern 0.8.0

### Enhancements
* Added `summarize_glm_count` function to analyze count data using a linear model.
* Added legend to `g_step`.
* Added formatting functions `format_fraction_fixed_dp` and
  `format_count_fraction_fixed_dp` with fixed single decimal place in percentages.
* Added `na_level` and `labelstr` arguments to `summarize_vars_in_cols`.
* Added `analyze_num_patients` to include summary at the beginning that does not
  repeat when paginating.
* Added `h_row_first_values` function as a more general helper function to retrieve
  first values from specific rows.
* Added option to remove `"(n)"` suffix from `unique_count` labels for `s_num_patients`.
* Added options to `g_km` to annotate with statistics (`annot_stats`) and add corresponding
  vertical lines (`annot_stats_lines`).

### Bug Fixes
* Fixed bug causing incorrect ordering of numeric grade levels when missing
  grades are present in `s_count_occurrences_by_grade`.
* Refactored `summarize_vars_in_cols` to work with pagination machinery.
* Fixed bug to allow passing of `conf_level` argument to `emmeans::contrast()` in `s_ancova`.
* Fixed bugs in `rtables_access.R` caused by not checking for specific combinations
  (also the standard values that were never used) of column indices and names.
* Fixed single applicable record bug in `count_abnormal_by_grade`.
* Fixed bug in `add_rowcounts` that caused all row count row values to count as zero.
* Fixed bug in `h_col_indices` causing an error when pruning with combination columns.
* Fixed bug in `test_proportion_diff` missing argument for `var_labels`.

### Documentation and Tests
* Added more tests to increase code coverage.
* Created separate documentation files for functions in different sections of `pkgdown` reference.
* Created separate `.R` files for logistic regression and cox regression helper functions.
* Fixed table tests using `analyze_num_patients` to generate an initial summary so there is no
  repetition when paginating.
* Updated tests to use `testthat` 3rd edition and replaced applicable tests with snapshot testing.
* Updated `summarize_ancova` examples to use `iris` dataset instead of `scda` data.
* Created vignette which saves cached synthetic `CDISC` dataset files to the `data/` folder and
  generated cached synthetic datasets.
* Updated all examples/tests to use datasets from the `data/` folder instead of `scda` datasets.
* Removed all template tests from `tern`. These tests are in internal repo `scda.test`.

### Miscellaneous
* Renamed `summarize_vars_in_cols` to `analyze_vars_in_cols` to reflect the appropriate `analyze` logic.
* Removed redundant `summary_in_cols` helper functions.
* Exported function `format_xx`.
* Replaced deprecated `ggplot2` functions/arguments to fix warnings.
* Replaced deprecated function `forcats::fct_explicit_na` with `forcats::fct_na_value_to_level`.
* Removed deprecated `wrap_text` function and related files.
* Started deprecation cycle for `footnotes` functions.

# tern 0.7.10

### New Features
* Added stratified `Newcombe` and stratified Wilson statistics to `estimate_proportion` and
  `estimate_proportion_diff` with relative tests.
* Added `stat_mean_pval`, a new summary statistic to calculate the p-value of
  the mean.
* Added statistic `mean_se` (mean with standard error) for `summarize_variables`
  and related functions.
* Introduced again `Rdpack` for references.

### Enhancements
* Redesign of data handling in tests by removing repetitive data loads and
  library calls.
* Added `DescTools::BinomDiffCI` function within `tern`.
* Added new parameter to `summarize_logistic` to specify which pivoted value
  to use during analysis.
* Updated `s_coxph_pairwise` to generate log-rank p-value using original
  log-rank test instead of Cox Proportional-Hazards Model.
* Implemented `nestcolor` in all examples by adapting `g_km`, `g_ipp`,
  `g_waterfall`, `g_step`, `g_lineplot`, and `g_forest`.
* Added parameters `interaction_y` and `interaction_item` in `ANCOVA` to make the
  interaction calculations available.
* Added new parameter `footnotes` to add footnotes to `g_km`.

### Migration from `assertthat` to `checkmate`
* Implemented `checkmate::assert_vector`, `checkmate::assert_set_equal`, and
  `checkmate::assert_int` to check vector type, length, and values.
* Replaced with standard assertions from `checkmate` the following functions:
  `all_elements_in_ref`, `is_df_with_nlevels_factor`, `is_df_with_no_na_level`,
  `is_proportion_vector`, `is_quantiles_vector`, `is_character_or_factor`,
  `is_nonnegative_count`, `is_valid_character`, `assert_character_or_factor`,
  `assert_equal_length` and `has_tabletree_colnames`.
* Modified `is_proportion`, `is_equal_length`, `is_df_with_no_na_level`,
  `is_df_with_nlevels_factor`, `is_variables`, `is_df_with_variables`,
  `is_df_with_factors`, `is_valid_factor` to use assertion logic.
* Added more verbose warnings from `as_factor_keep_attributes`.
* Made `assert_df_with_factors` and `assert_proportion_value` internal functions.
* Renamed `assertthat.R` and `test-assertthat.R` to `utils_checkmate.R` and
  `test-utils_checkmate.R`.

### Documentation and NAMESPACE Polishing
* Added stable badges for:
   - `count_abnormal_by_marked` (reference to `abnormal_by_marked`),
   `count_abnormal_lab_worsen_by_baseline` and `h_adlb_worsen` (reference to
   `abnormal_by_worst_grade_worsen_from_baseline`), `count_abnormal_by_worst_grade`
   (reference to `abnormal_by_worst_grade`), `to_string_matrix`, `tidy.summary.coxph`, `tidy.step`,
   `surv_timepoint`, (reference to `survival_timepoint`), `surv_time` (reference to `survival_time`),
   `coxph_pairwise` (reference to `survival_coxph_pairwise`),
   `extract_survival_subgroups` and `tabulate_survival_subgroups` (reference to `survival_duration_subgroups`),
   `extract_survival_biomarkers` and `tabulate_survival_biomarkers` (reference to
   `survival_biomarkers_subgroups`), `control_summarize_vars`, `s_summary` and
   `a_summary` (reference to `summarize_variables`) and kept the S3 method tree.
   - `summarize_patients_exposure_in_cols`, `summarize_num_patients` with
   `s_num_patients`, `s_num_patients_content`, `summarize_num_patients`.
   - `count_cumulative`, `count_missed_doses`, `count_patients_events_in_cols`, `summarize_colvars`, `summarize_change`, `summarize_ancova`,`as.rtable`, `color_palette`, `add_footnotes`.
   - (statistical function controls)  `control_coxreg`, `control_coxph`,
   `control_incidence_rate`, `control_lineplot_vars`, `control_surv_time`,
   `control_surv_timepoint`, `control_logisitic`, `control_step`.
   - `stat_mean_ci`, `stat_median_ci`, `split_cols_by_groups`, `explicit_na`, `sas_na`, `extract_rsp_subgroups`, `tabulate_rsp_subgroups`, `extract_rsp_biomarkers`,
   `tabulate_rsp_biomarkers`, `keep_rows`, `keep_content_rows`, `has_count_in_any_col`,
   `has_fraction_in_cols`, `has_fraction_in_any_col`, `has_fractions_difference`,
   `test_proportion_diff`, `pairwise`, `logistic_regression`,
   `estimate_incidence_rate`, `control_incidence_rate` (reference to
   `incidence_rate`), `cut_quantile_bins`,
   `estimate_multinomial_rsp`, `decorate_grob_set`, `extreme_format`, `fit_rsp_step`,
   `fit_survival_step`, `footnotes`, `footnotes-set`,
   `format_count_fraction`, `format_fraction_threshold`, `formatting_functions`,
   `format_fraction`, `combination_function` (S4 method), `compare_variables` (S3 method),
   `kaplan_meier`.
* Internal keywords added, export removed, `_pkgdown.yml` updated, and `tern:::` added for
   tests/examples/vignettes where present for the following functions:
   - (chain functions, reference to `abnormal_by_marked`) `s_count_abnormal_by_marked`,
   `a_count_abnormal_by_marked`.
   - (chain functions, reference to `abnormal_by_worst_grade_worsen_from_baseline`)
   `a_count_abnormal_lab_worsen_by_baseline`, `s_count_abnormal_lab_worsen_by_baseline`.
   - (chain functions, reference to `abnormal_by_worst_grade`) `s_count_abnormal_by_worst_grade`,
   `a_count_abnormal_by_worst_grade`.
   - (chain functions, reference to `survival_timepoint`) `s_surv_timepoint`, `s_surv_timepoint_diff`,
   `a_surv_timepoint`, `a_surv_timepoint_diff`.
   - (chain functions, reference to `survival_time`) `s_surv_time`, `a_surv_time`.
   - (chain functions, reference to `survival_coxph_pairwise`) `s_coxph_pairwise`, `a_coxph_pairwise`.
   - (chain functions, reference to `survival_duration_subgroups`) `a_survival_subgroups`.
   - (chain functions, reference to `count_cumulative`) `s_count_cumulative`, `a_count_cumulative`.
   - (chain functions, reference to `count_missed_doses`) `s_count_nonmissing`, `s_count_missed_doses`, `a_count_missed_doses`.
   - (chain functions, reference to `count_patients_events_in_cols`) `s_count_patients_and_multiple_events`, `summarize_patients_events_in_cols`.
   - (chain functions, reference to `incidence_rate`) `s_incidence_rate`, `a_incidence_rate`.
   - (cox regression helper) `cox_regression_inter`, `decorate_grob_factory`, `draw_grob`, `estimate_coef`.
   - `summary_labels`, `summary_formats`, `s_count_patients_sum_exposure`,
   `a_change_from_baseline` `s_change_from_baseline`, `a_ancova`, `s_ancova`, `arrange_grobs`, `as_factor_keep_attributes`, `combine_levels`, `split_text_grob`,
   `groups_list_to_df`, `s_cox_multivariate`, `is_leaf_table`, `a_response_subgroups`, `range_noinf`, `has_count_in_cols`,
   `has_counts_difference`, `prop_chisq`, `prop_cmh`, `prop_schouten`, `prop_fisher`,
   `s_test_proportion_diff`, `a_test_proportion_diff`, `fct_discard`, `fct_explicit_na_if`.

### Bug Fixes
* Fixed retrieval method of `stats::ancova` output due to version inconsistency.
* Fixed tests to respect the new standard print for `NA` coming from `rtables`.
* Fixed error in tests coming from changes in `formatters::var_labels`.
* Fixed `prop_diff` functions to respect success responses (`TRUE` values).
* Fixed error coming from comparing factors vector to characters vector.
* Fixed empty vector exception for `cut_quantile_bins`.
* Fixed exception error when empty strings are present in pivoted columns (`rtables` split
  functions)
* Fixed bug in `s_ancova` causing an error when the first level of the arm
  factor is not the control arm.
* Fixed bug in `s_abnormal_by_worst_grade` when there is one `PARAM` level.
* Fixed bug in `prop_diff_wald` when selecting all responders, updated tests accordingly.
* Fixed bug in `h_ancova` that caused an error when deselecting all covariates.

### Miscellaneous
* Added deprecated badge to `g_mmrm`.
* Removed internal function calls in examples (`tern:::`) and added `dontrun`
  to internal function examples.
* Removed warnings and messages to console occurring in examples and tests.
* Deprecated functions `color_palette` and `h_set_nest_theme` in favor of
  `nestcolor::color_palette` and `nestcolor::theme_nest`, respectively.
* Removed deprecated functions: `color_palette`, `color_palette_core`,
  `h_set_nest_theme`, `s_cox_univariate`.
* Removed deprecated `mmrm` functions: `fit_mmrm`, `g_mmrm_diagnostic`,
  `g_mmrm_lsmeans`, `as.rtable.mmrm`, `h_mmrm_fixed`, `h_mmrm_cov`,
  `h_mmrm_diagnostic`, `tidy.mmrm`, `s_mmrm_lsmeans`, `s_mmrm_lsmeans_single`,
  `summarize_lsmeans`.
* Renamed functions `arm` to `study_arm` and `extract` to `extract_by_name`.
* Renamed `rtables.R` to `utils_rtables.R`.
* Extracted `cox_regression_inter` into a separate file from `cox_regression`.
* Renamed `estimate_incidence_rate.R` to `incidence_rate.R` to match the documentation grouping name.
* Extracted `control_incidence_rate` into a separate file because it produces a separate documentation file.
* Added `@md` and removed `@order` from `incidence_rate.R`. Modified examples accordingly.
* Removed hyperlink from `prop_schouten` function documentation.
* Exported `draw_grob` function.

# tern 0.7.8

### Fix

* `h_split_by_subgroups` documentation warning fix for wrong placing of example block

### Documentation and NAMESPACE polishing

*  Adopting the standard of badges for `@description` instead of every
   `@descriptionIn` function. Corrected accordingly `summarize_variables_in_cols`
*  Added stable badge for `g_lineplot`, `g_step`, `g_waterfall`, `cox_regression`,
   `score_occurrences`, `add_rowcounts`, `odds_ratio`, `count_occurrences`,
   `count_occurrences_by_grade`, `explicit_na`, `df_explicit_na`,
   `count_patients_with_event`, `decorate_grob`, `combine_groups`,
   `append_varlabels`, `univariate`, `stack_grobs`, `count_abnormal` (reference
   to `abnormal`), `count_abnormal_by_baseline` (reference to
   `abnormal_by_baseline`).
*  Internal keywords added, export removed, `_pkgdown.yml` polished and `tern:::` for
   tests, examples, and vignettes when present for the following functions:
   - (helper functions) `h_format_row`, `h_map_for_count_abnormal`
   - (utils functions) `make_names`, `month2day`, `day2month`
     `empty_vector_if_na`, `aesi_label`,
     `n_available`, `format_xx`, `arm`.
   - `count_values_funs`, `prop_difference`, `combine_counts`.
   - (chain functions) `s_count_abnormal`, `a_count_abnormal`.
   - (chain functions) `s_count_abnormal_by_baseline`, `a_count_abnormal_by_baseline`,
     `d_count_abnormal_by_baseline`.
*  Deprecated `s_cox_univariate` function has now deprecated badge.

### Enhancements

* Enhanced `g_lineplot` with table to automatically scale the table height and return a `ggplot` object.
* Enhanced `g_ipp` with caption argument and adjust the position.
* Enhanced `prop_diff`, `tern` function and related functions to be able to apply a continuity correction in the `Newcombe` method.
* Enhanced `summarize_numeric_in_columns` and `summarize_variables` to allow factor/character summary and to be able to summarize the number of `BLQs` in `AVALC` from `ADPC` dataset.
* Updated order of summarize variables stats in manual for order consistency.
* Added a `sum` option to `summarize_variables`.
* Use consistent color palette for plotting (`stream` by default).
* Enhanced `h_pkparam_sort` function with argument `key_var` to allow data with different column names.

### Miscellaneous
* Updated `test-table_aet02.R` variant 12.
* Changed the `scda` data version to '2022-02-28'.
* Added a template to the `pkgdown` site.
* Removed package dependencies of `grDevices`, `stringr`, and `viridisLite`.
* Renaming `summarize_numeric_in_columns` to `summarize_variables_in_columns`.
* Renaming `summarize_vars_numeric_in_cols` to `summarize_vars_in_cols`.
* Fixed a bug where points on the `g_lineplot` plot were not connected when missing values.
* Updated the package authors.

# tern 0.7.7

### Breaking changes

* Move `MMRM` into a separate package `tern.mmrm`.

### New features

* Added `h_pkparam_sort` to order `PK PARAM` value based on the order of the dataset generated by `d_pkparam()`.
* Added `d_pkparam` to generate PK parameter map for sorting.

### Enhancements

* Changed the `nudge_y` argument of `h_g_ipp` to be dependent on the data, fixing an issue whereby the baseline labels were offset incorrectly.
* Enhanced `stat_mean_ci` and `s_summary.numeric` to calculate the geometric mean with its confidence intervals.

### Miscellaneous

* Updated dependencies and internal adjustments after the `rtables` package refactor.
* Removed `with_label`, `var_labels`, and `var_labels<-` to resolve conflict with the `formatters` package, a new dependency.
* Added new "Introduction to `tern`" and "`tern` tabulation" vignettes.

# tern 0.7.6

### New features
* Added `h_map_for_count_abnormal` to create the map used in `trim_levels_to_map` split function by calling this helper function. It supports two methods: one with all observed mapping, one with at least low limit above zero and at least one non missing high limit.
* Added `s_summary_numeric_in_cols` and `summarize_vars_numeric_in_cols` functions to generate summary statistics in columns, mainly used for PK datasets.
* Added five statistics to `s_summary.numeric` to use in `s_summary_numeric_in_cols`.

### Enhancements
* Enhanced functions `tabulate_survival_subgroups` and `tabulate_rsp_subgroups` (Survival Duration and Best Response analyses) to calculate `N`-s based on the records considered to create the model.
* Enhanced the function `estimate_proportion` and related functions to be able to apply a continuity correction in the Wilson method.
* Refactored `count_abnormal_by_marked` and related statistics and formatting functions to use a more efficient layout with `.spl_context` argument used for determining denominators and with `trim_levels_to_map` split function under `split_rows_by` to show the desired levels in the table. This is a breaking change.
* Refactored `count_abnormal_by_worst_grade` and related statistics and formatting functions to use a more efficient layout with `.spl_context` argument used for determining denominators and with `trim_levels_to_map` split function under `split_rows_by` to show the desired levels in the table. This is a breaking change.
* Refactored `count_abnormal` function and related statistics and formatting functions to use a more efficient layout with `trim_levels_to_map` split function under `split_rows_by` to show the desired levels in the table. Also updated `abnormal` argument to be able to consider more than one level for each direction. This is a breaking change.
* Enhanced the function `estimate_incidence_rate` and related functions to consider the week as time unit for data input.

### Bug fixes
* Fixed bug in `assertthat` functions that output wrong data frame names and limited length of failure message outputs.

### Miscellaneous
* Removed dependency on `utils.nest` by using the `checkmate` and `purrr` packages for validation and moved `get_free_cores` and `skip_if_too_deep` functions from `utils.nest` into `tern`.

# tern 0.7.5

### New features
* Added functions to estimate continuous biomarker effects across subgroups for survival and binary response endpoints, used to produce corresponding forest plots, see `survival_biomarkers_subgroups` and `response_biomarkers_subgroups`.
* Added `g_lineplot` plot function, including new `h_format_row` helper function and `control_lineplot_vars` function. Removed `g_summary_by`.
* Added new safety helper function `h_stack_by_baskets` to stack events in `SMQ` and/or `CQ` basket flag in `ADAE` data set.

### Enhancements
* Added a couple of new statistics to `s_summary.numeric`. Added `names` attribute to each element of the final list returned by the `s_summary.numeric` function. Added `summary_formats` and `summary_labels` helper functions.
* Added option to also convert logical variables to factor variables in `df_explicit_na`.
* Refactored `h_append_grade_groups` to improve its flexibility, robustness and clearness, and to make sure the result is ordering according to the order of `grade_groups`. Also, added `remove_single` argument which controls whether the elements of one-element grade groups are in the output or removed.
* Added `var_labels` and `show_labels` arguments to `count_occurrences` and `count_patients_with_flags` to allow for creation of a title row.
* Added `na_level` argument to `count_abnormal_by_baseline`.
* Updated `h_append_grade_groups` to no longer fill-in empty grade groups with zeros.

### Bug Fixes
* Fixed `prop_diff_cmh` to handle edge case of no FALSE (or TRUE) responses.
* Enhanced `g_mmrm_diagnostic` to improve error handling when data is not amenable to the Locally Weighted Scatterplot Smoothing.
* Fixes in `g_km`:
  * Plot can now display any combination of the annotation tables for number of patients at risk, median survival time, and `CoxPH` summary.
  * Function will return a warning instead of an error if the `arm` variable includes a single level and `annot_coxph = TRUE`.
  * Lines in the plot now start at time 0 and probability 1.
  * Category labels can include the equals sign.

### Miscellaneous
* Fixed `day2month` and `month2day` to work with NA data.
* Added parameters for `stat_mean_ci` and `stat_median_ci` so that they may return different outputs.
* Added functionality in `h_row_counts` to handle analysis rows with `NULL` cells.
* Updated `LICENCE` and `README` with new package references.
* Added `error_on_lint: TRUE` to `.lintr`.

# tern 0.7.4

### New features
* Added new safety layout functions:
  * `count_abnormal_by_marked` tabulates marked laboratory abnormalities.
  * `summarize_patients_exposure_in_cols` tabulates patient counts and sum of exposure across all patients.

### Enhancements
* Enhanced `mmrm` related functions for fitting models without `arm` variable.
* Updated `cox_regression` to work without covariates. Also in case of interaction model summary, p-values for main effect coefficients are no longer displayed.
* Descriptive statistics returned by `summarize_vars` now include quantiles. `summarize_vars` now accepts the control function `control_summarize_vars` to specify details about confidence level for mean and median and quantile details. The `control` argument replaces `conf_level`.
* Added `var_labels` and `show_labels` arguments to `count_occurrences_by_grade`.
* Changed `indent` argument in `append_varlabels` to accept non-negative integer to represent the indent space defined by user. Previous calls with Boolean `indent` will do an integer conversion and produce a warning.

### Bug Fixes
* Corrected `tabulate_survival_subgroups` and related survival forest plot functions to use total number of events, instead of observations, as default for scaling the symbol sizes in the plot. (The user might still use total number of observations manually if they wish to do so.)
* Helper function `h_adsl_adlb_merge_using_worst_flag` will now impute `BTOXGR` for missing visits.

### Miscellaneous
* Deleted `count_abnormal_by_worst_grade_by_baseline` and its related statistic and analysis functions as a simpler design will create lab abnormality tables.
* Switched data in examples to use `scda` instead of `random.cdisc.data` package.

# tern 0.7.3

### New Features
* Added Subgroup Treatment Effect Pattern (STEP) model fitting functions `fit_rsp_step` and `fit_survival_step`, the corresponding tidy method `tidy.step` as well as the graph function `g_step`.
* Added new layout function `compare_vars` which compares variables of different types between columns and produces a p-value for the comparison to the reference column. Function built on top of the `summarize_vars` functionality.
* Added utility functions:
  * `cut_quantile_bins` cuts a numeric vector into quantile bins.
  * `fct_collapse_only` collapses levels of a factor and keeps those in the order provided.
  * `fct_explicit_na_if` inserts explicit missings in a factor based on a condition.
  * `range_noinf` is a kind of a wrapper function of `base::range`. It returns `c(NA, NA)` instead of `c(-Inf, Inf)` for zero-length data.

### Enhancements
* Cox regression via `fit_coxreg_univar` and `fit_coxreg_multivar` is now also possible without treatment arm. In the univariate case this means that it fits separate univariate models for the provided covariates and tabulation of corresponding effect estimates can later occur.
* Added `fraction` in result returned by `s_count_occurrences`. It contains a list of numerators and denominators with one element per occurrence.
* Updated `sum_num_patients` and `count_occurrences` for the result `unique` and `count_fraction` to return (0, 0) when input is empty.
* Added a new argument `groups_lists` to `extract_survival_subgroups`, `extract_rsp_subgroups` and associated helper functions which allows to group factor levels of subgroup variables into manually defined groups, enhancing the flexibility of the resulting forest graphs.
* Forest graph function `g_forest` now extracts default arguments from attributes of the input table produced by `tabulate_rsp_subgroups` and `tabulate_survival_subgroups` so that the user does not have to do this manually anymore.
* In `g_km`:
  * Remove arm variable name from arm labels in plot legend and annotation tables.
  * Show symbol used to mark censored cases and match order of arm variable factor levels in the legend.
  * Display hazard ratio and its confidence interval to two decimal places.
  * Updated default position of hazard ratio table to stay on the left bottom corner but above x-axis.
  * Use arm value as strata name in when treatment arm variable has a single level.
* Updated `s_surv_time` function to use a newly created function `range_noinf` instead of `base::range`.
* New argument `no_fillin_visits` added to `h_adsl_adlb_merge_using_worst_flag` to specify excluded visits from the post-baseline worst toxicity grade output. Improved `h_adsl_adlb_merge_using_worst_flag` to include variables shared between `adsl` and `adlb`, along with `PARAM`, `PARAMCD`, `ATOXGR`, `BTOXGR` and optionally `AVISIT`, `AVISITN` when `by_visit = TRUE`. Prior output contained `USUBJID`, `ARMCD`, `PARAMCD`, `ATOXGR`, and `BTOXGR`.

### Bug Fixes
* Fix bug in `s_surv_timepoint` for cases when there are zero patients at risk.
* Modified `stat_median_ci` function so that when passing empty var with empty name, no `row names contain missing values` error would show.

### Miscellaneous
* Deprecated `s_cox_univariate` function, use `fit_coxreg_univar` function instead.
* Updated default formats of `hr` and `hr_ci` in `a_coxph_pairwise` and median in `s_surv_time` to align with STREAM.
* Updated the pre-processing code in the files `test-table_ttet01.R` and `test-table_dort01.R` to make sure the analysis variable `EVNT1` has both levels of the factor defined.
* Improved error message when number of levels in a factor variable in a data frame is not as expected.

# tern 0.7.2
* Fixed column label internal test errors.

# tern 0.7.1
* New argument `position_surv_med` added to `g_km` to move position of the annotation table with median survival times.
* Fixed bug in `g_km` related to the ignored arguments `pch` and `size` which were not passed on to helper function `h_ggkm`.
* Updated `xticks` and `max_time` arguments in `g_km` for greater functionality. `max_time` added as an argument in `h_xticks` to allow this.
* Fixed bug in `prop_diff_cmh` that led to `NaN` weighted proportion difference estimates and missing confidence intervals. Before this change, when including no patients from one treatment arm for at least one stratum the estimation did not lead to numeric results.
* Fixed bug in `prop_cmh` giving an error in case of at least one stratum containing less than two observations.
* New argument `n_events` added to `estimate_incidence_rate`.
* New argument `denom` added to `count_occurrences`.
* New arguments `yval` and `ci_ribbon` added to `g_km`.
* Add new individual patient plot function `g_ipp` along with helpers `h_g_ipp` and `h_set_nest_theme`.
* Fixed bug in `count_patients_with_events`, now shows zero counts without percentage.
* Fixed bug in `get_mmrm_lsmeans` which did not allow `MMRM` analysis of more than 3000 observations.
* Updated `stat_mean_ci` and `stat_median_ci` to handle edge cases with number of elements in input series equal to 1. For such cases, `NA_real_` is now returned, instead of `NA` or `+/-Inf` for confidence interval (CI) estimates.
* Rename `n_lim` argument of `stat_mean_ci` to `n_min` to better reflect its desired meaning.

# tern 0.7.0
This version of `tern` introduces a major rewriting of `tern` due to the change to layout based tabulation in `rtables`. `tern` now does not build tables directly anymore, instead it provides analysis functions to build tables, see the examples.
* Counting patients with abnormal values post-baseline with `count_abnormal`.
* Counting patients with graded abnormal values with `count_abnormal_by_worst_grade`.
* Counting patients with abnormal values by baseline status with `count_abnormal_by_baseline`.
* Counting patients with missed doses with `s_count_missed_doses` and `count_missed_doses`.
* Counting patients with event flags with `count_patients_with_event` and `count_patients_with_flags`.
* Summarizing variables with `summarize_vars` (supports numeric, factor, character and logical variables). Note that factors need to have `NA`s converted to `na_level` before use.
* Summarizing change from baseline with `summarize_change`.
* Summarizing variables in columns with `summarize_colvars`.
* Estimating difference for responder proportions with `estimate_proportion_diff`.
* Estimating difference for Odds Ratio with `estimate_odds_ratio`.
* Testing the difference in responder proportions with `test_proportion_diff`.
* Estimating the responder proportion for the level of a factor with `estimate_multinomial_response`.
* Fitting and tabulating the results of Cox regressions with `fit_coxreg_univar`, `fit_coxreg_multivar` and `summarize_coxreg`, respectively.
* Pruning occurrence tables (or tables with counts and fractions) with flexible rules, see `?prune_occurrences` for details.
* Sorting occurrence tables using different options, see `?score_occurrences` for details.
* Fitting and tabulating `MMRM` models with `fit_mmrm` and `as.rtable` and `summarize_lsmeans`, see `?tabulate_mmrm` for details.
* Counting the number of unique and non-unique patients with `summarize_num_patients`.
* Counting occurrences with `count_occurrences`.
* Counting occurrences by grade with `summarize_occurrences_by_grade` and `count_occurrences_by_grade`.
* Counting patients and events in columns with `summarize_patients_events_in_cols`.
* Tabulating the binary outcome response by subgroup with `extract_rsp_subgroups` and `tabulate_rsp_subgroups`.
* Tabulating the survival duration by subgroup with `extract_survival_subgroups` and `tabulate_survival_subgroups`.

# tern 0.6.9
* Removing not used imports.

# tern 0.6.8
* Improved handling of missing data in aggregation functions: `a_mean_sd`, `a_median`,  `a_n_true_and_freq`, `a_count`, `a_q1q3`, `a_iqr`, `a_range`.
* New default p-value method in `s_test_proportion_diff`: Chi-Squared Test with Schouten Correction.
* Add new function `t_contingency` for contingency tables.
* Renamed the class `splitText` to `dynamicSplitText` to resolve the name conflict with the package `ggpubr`.
* Add `rreplace_format` for tabulation post-processing.
* Add new tern function `t_ancova` to create `ANCOVA` tables, as well as corresponding elementary table function `t_el_ancova` and summary function `s_ancova`.
* Add new tern function `s_odds_ratio` to estimate Odds Ratio of response between categories, as well as the corresponding elementary table function `t_el_odds_ratio`.
* Added new CI methods (`Agresti-Coull`, `Jeffreys`) for `s_proportion`.
* Added new CI methods `anderson-hauck` and `newcombe` to `s_proportion_diff`.
* Added new p-value methods (Fisher's Exact, Chi-Squared Test with Schouten Correction) for `s_test_proportion_diff`.
* The binary summary table function `t_binary_outcome` takes now lists (instead of character vectors) specified by the helper function `control_binary_comparison` as the arguments `strat_analysis` and `unstrat_analysis`. Odds Ratio estimates and CIs are now removable and included by default, similarly to the other subsections of the arm comparison analyses. Also added argument `rsp_multinomial`.
* Add new table function `t_el_multinomial_proportion`.
* Add new table function `t_abn_shift`.
* Add new `MMRM` analysis function `s_mmrm`, as well as corresponding table functions `t_mmrm_lsmeans`, `t_mmrm_cov`, `t_mmrm_diagnostic`, `t_mmrm_fixed`, and plot functions `g_mmrm_lsmeans`, `g_mmrm_diagnostic`. The results of these match SAS results (up to numeric precision).
* Deprecated old `MMRM` functions `a_mmrm` and `t_mmrm` (they give a deprecation warning but still work) to remove in the next release. The reason is that the results of these functions don't match SAS results.
* Fix bug in `g_km` related to numbers in patients at risk table to correct numbers for integer time-to-event variable inputs.

# tern 0.6.7

* For functions with `row_by` argument, inputs no longer require use of `nested_by`.
* Add `stat_mean_ci` and `stat_median_ci` for error bars in `ggplot2`.
* Add new tern function `t_coxreg` as single interface for diverse cox regression types.
* Add compound table for binary endpoint: `t_binary_endpoint` and elementary functions: `t_el_proportion`, `t_el_proportion_diff` and `t_el_test_proportion_diff`. The supporting summary functions added are: `s_proportion`, `s_adj_proportion_diff`, `s_proportion_diff` and `s_test_proportion_diff`.
* Added new tern function `t_events_patyear` to create event table adjusted person-years.
* Added new tern function `t_abnormality` and the elementary table function `t_el_abnormality`.
* Removed `grade_levels` argument from `t_events_term_grade_id` functions. Post-processing by reordering the leaves of the table tree creates a different ordering of rows if required. Creating a helper function will occur at a later time.
* Added `prune_zero_rows` argument to `t_events_per_term_grade_id` and `t_max_grade_per_id` to not show rows of all zeros as they can clutter the visualization in the Shiny app and make it slower.
* Fixed position of (N=xx) in `t_summary_by` output when summarizing numeric columns in parallel with `compare_in_header`.
* Rename `t_coxph` to `t_coxph_pairwise` to reflect the model process, add details in documentation.
* Remove `test.nest` dependency.
* Keep column labels when splitting data into tree.

# tern 0.6.6

* Remove `test.nest` dependency.

# tern 0.6.5

* Change default option for denominator to be `N` in `t_summary`.
* Fix IQR bug: IQR as Q3 minus Q1.
* Add new function `t_logistic` for multi-variable logistic regression table.
* Add new function `df_explicit_na` to replace `NA` by explicit values.
* Added possibility in `t_tte` to specify confidence level independent for `survfit`, `coxph`, and `ztest`, see the manual.
* Fixed bug in `t_rsp` of not showing p-value, odds ratio and CIs when `strata_data` is not `NULL`.
* Added stratified analysis for `t_forest_rsp` and `t_forest_tte`, with footnotes in `g_forest`.
* Added `footnotes`, `footnotes<-` and `add_footnotes<-` functions to deal with footnotes.
* Added argument `conf_int` for confidence interval level to `t_el_forest_rps`, `t_forest_rsp`, `t_el_forest_tte`, `t_forest_tte`.
* Added argument `col_symbol_size` to `g_forest` to control the relative size of symbols used in the plot.
* Added `s_coxph_pairwise` function to perform pairwise testing, used by `t_tte` and `t_coxph`.
* Added methods for `t_count_true` replacing `t_summary_true`.
* Updated `t_count_unique` to create analysis subsets, added `t_el_count_unique` for vectors.
* Fixed bug in `t_events_term_id` so that table sort order is by decreasing frequency instead of alphabetical.
* Added function `color_palette` and a new nest color palette.

# tern 0.6.4

* Refactored after renaming functions in `utils.nest`.
* Added `event_type` argument to `t_events_per_term_grade_id`.
* Added one/two-arm t-test functions.
* Improvements in `t_summary_by`.
* Internal code refactoring.

# tern 0.6.3

* Added `node` S4 class to create trees:
    - For all related tree functions see the reference under trees.

* Removed functions:
    - Moved all the label functions to `rtables`.
    - Deleted `keys` and `keys<-` functions.

* New helper functions:
    - Exported `tabulate_pairwise`.
    - `get_N`, `col_N_add_total`, `check_id`.
    - `na_as_level`.
    - `as_factor_keep_attributes`.
    - `r_by`.

* New TLGs:
    - Disposition elementary table `t_el_disposition`.
    - `t_el_forest_tte`, `t_el_forest_rsp`.

* Changed Arguments:
    - All compound tables:
        - Added `table_tree` argument which returns a `node` object.
    - `t_summary.numeric`:
        - Added `f_numeric` to choose which statistics to calculate.
    - `t_summary.factor`:
        - `denominator` now also allows for `omit` if wanting to omit percentages.
    - `t_summary_by`:
        - Renamed `by` to `row_by`.
    - `t_forest_rsp`, `t_forest_tte`:
        - Changed functionality of `group_data` using `row_by_list`.
        - Removed `na_omit_group`.
    - `t_count_unique`:
        - Removed `indent` argument, use the `indent` function in `rtables`. instead

# tern 0.6.2

* Use cached data from `random.cdisc.data` to speed up testing.
* Added `t_summary.Date` method.
* Added `save_join`.

# tern 0.6.1

* Fixed colors in Kaplan-Meier Plot
* Refactor of all functions to pass `test.nest` tests:
    * Changed `width_row.names` argument of `g_forest` function into `width_row_names`.
    * Changed `censor.show` argument of `g_km` function into `censor_show`.
    * Changed `col.legend.title` argument of `g_waterfall` function into `col_legend_title`.
    * Changed `na.rm` argument of `t_count_unique` function into `na_rm`.
    * Changed `row.name` argument of `t_count_unique` function into `row_name`.
    * Changed `na.omit.group` argument of `t_forest_rsp` function into `na_omit_group`.
    * Changed `na.omit.group` argument of `t_forest_tte` function into `na_omit_group`.
    * Changed `row.name.TRUE` and `row.name.FALSE` arguments of `t_summary.logical` into `row_name_true` and `row_name_false` respectively.
    * Rename `splotTextGrob` into `split_text_grob`.
    * Fix examples.
    * Refactor of internal functions code.

# tern 0.6.0

* Removed functions `addTable`, `t_summarize_by_visit`, `t_summarize_variables`.
* Added `t_summary_by` function.
* Refactor of `g_km` function, renamed `kmGrob` into `kmCurveGrob`.
* Refactor `t_events_*` family of functions.
* Updated examples.

# tern 0.5.0.3

#### New TLGs

* `t_summary` and methods for `data.frame`, `numeric`, `logical`, `character`, `factor`, and `Date` objects.
* `t_events_per_term_id`, `t_events_per_term_grade_id`: Adverse Events & Concomitant Treatment Tables.
* `t_max_grade_per_id`, `t_count_unique`, `t_events_summary` elementary tables used for the Adverse Events & Concomitant Treatment Tables.
* `g_waterfall`: Horizontal Waterfall Plot.

#### New Helper Functions

* `decorate_grob`, `decorate_grob_set`, `decorate_grob_factory`, `splitTextGrob`.
* `stack_grobs`, `arrange_grobs`, `draw_grob`.

#### TLG changes

* `t_tte` now shows two rows with ranges for event and censored times, respectively.
* `g_km` works with one arm `survfit` objects.
* In forest plot functions, added formatting to display extreme values to ">999.9".
* `t_summarise_variables` uses now `n` instead of `N` as a denominator for calculating percentages for factors by default.
* `t_rsp` now works when all response values are `TRUE` or `FALSE`.

#### Deprecated Functions

* Deprecated `t_summarize_variables` as `t_summary` is more powerful.
* Replacing `t_summarize_by_visit` with `t_summary_by` will occur in an upcoming release.

# tern 0.5.0

* First version with harmonized analysis functions names and arguments.
