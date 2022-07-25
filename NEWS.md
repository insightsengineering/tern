# tern 0.7.8.9023

### Enhancements 
* exported `draw_grob` function (needed by `enableRF`).
* `checkmate::assert()`, `checkmate::assert_true()`, and `checkmate::assert_false()`
   use is kept to a minimum due to its ambiguous messages. 
* `checkmate::assert_set_equal()` and `checkmate::assert_int()` have been changed
  when checking for the length of vectors, either by checking the right input and
  its length or by using `checkmate::assert_vector()`.
* Removed `assert_equal_length` as comparing lengths of multiple vectors can be 
  done without the need of a custom function.
* Implemented `nestcolor` in all examples with slight refactoring to `g_km`, `g_ipp`, 
  `g_waterfall`, `g_step`, `g_lineplot`, and `g_forest`.

### Migration from `assertthat` to `checkmate`
* complete substitution of `assertthat` calls with `checkmate`.
* `assert_df_with_factors`, `assert_equal_length`, and `assert_proportion_value` 
  made internals.
* removed `has_tabletree_colnames`.
* removed `is_quantiles_vector` and `all_elements_in_ref` (substituted by 
  `checkmate::subset()`).
* removed `is_proportion_vector` and adapted `is_proportion` to `checkmate` style 
  with `assert_proportion_value`.
* `is_equal_length` is now `assert_equal_length`.
* removed `is_df_with_no_na_level` by adding parameters to `assert_df_with_variables`.
* removed `is_df_with_nlevels_factor` by adding parameters to `assert_df_with_factors`.
* `is_character_or_factor` replaced by `checkmate::assert_multi_class()`.
* `is_nonnegative_count` removed and substituted by `checkmate::assert_count()`.
* `assert_list_of_variables` instead of `is_variables` (made internal).
* `assert_df_with_variables` instead of `is_df_with_variables` (made internal).
* `assert_df_with_factors` instead of `is_df_with_factors` (made internal).
* `assert_valid_factor` instead of `is_valid_factor`.
* removed `is_valid_character`.
* renamed `assertthat.R` into a more appropriate `utils_checkmate.R`.
* renamed `test-assertthat.R` into `test-utils_checkmate.R`.

### Fix
* Fixing error coming from comparing factors vector to characters vector.
* `fct_collapse_only`, `fct_collapse_only`, and `month2day`/`day2month` reverted
  to export.
* Fixed test for `cut_quantile_bins` with empty vector. This is not a good standard
  input.
* Warnings from `as_factor_keep_attributes` are now in verbose for better 
  `test_examples()` readability
* Renaming `rtables.R` as confusing file name due to the package dependence.
* Renamed files to respect the main documented function and fixed file in `R/`
  folder that does not comply with the standard (starting with the word test).
* Extracted `cox_regression_inter` from `cox_regression`.
* `d_` and `h_` functions are all reverted or confirmed as export functions to
  allow future users to utilize their added flexibility.
* Fixing bug related to error flag for empty strings coming from `rtables` split
  functions. Creation of `replace_emptys_with_na` to replace empty strings with
  custom strings across data.frame (this can be merged with `df_explicit_na`).
* Add of new parameter for `summarize_logistic` that specify which pivoted value
  is meant to be removed during the analysis.
* Renamed `estimate_incidence_rate.R` into `incidence_rate.R` to match the
  documentation grouping name.
* Extracted `control_incidence_rate` into a separated file as it was exported and
  documented separately.
* Added `@md` and removed `@order` from `incidence_rate.R`. The presented order
  is automatically the final order in the documentation. There is no specific
  need to add this tag. Modified examples accordingly.
* Fixed warnings occurring in example tests
* Removed `tern:::` prefix and added `dontrun` to internal function examples.
* Enhanced `s_coxph_pairwise` with generating log-rank p value by original 
  log-rank test, instead of Cox Proportional-Hazards Model.
* Fixed bug in `s_ancova`, avoiding error when the first level of the arm factor 
  is not the control arm.

### Documentation and NAMESPACE polishing
*  Added stable badge for:
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
   - `count_cumulative`, `count_missed_doses`, `count_patients_events_in_cols`, `summarize_colvars`, `summarize_change`, `summarize_ancova`,`as.rtable`, `color_palette`, `add_footnotes` (note, this function is defined in two different files: footnotes and g_forest).
   - (statistical function controls)  `control_coxreg`, `control_coxph`,
   `control_incidence_rate`, `control_lineplot_vars`, `control_surv_time`,
   `control_surv_timepoint`, `control_logisitic`, `control_step`.
   - `stat_mean_ci`, `stat_median_ci`, `split_cols_by_groups`, `explicit_na`, `sas_na`, `extract_rsp_subgroups`, `tabulate_rsp_subgroups`, `extract_rsp_biomarkers`,
   `tabulate_rsp_biomarkers`, `keep_rows`, `keep_content_rows`, `has_count_in_any_col`,
   `has_fraction_in_cols`, `has_fraction_in_any_col`, `has_fractions_difference`,
   `test_proportion_diff`, `pairwise`, `logistic_regression`,
   `estimate_incidence_rate`, `control_incidence_rate` (another file), reference to
   `incidence_rate`, `cut_quantile_bins`, `d_pkparam` (note: d function but required by tern_ex file),
   `estimate_multinomial_rsp`, `decorate_grob_set`, `extreme_format`, `fit_rsp_step`,
   `fit_survival_step`, `footnotes`, `footnotes-set` (note: this function is defined twice in the footnotes file),
   `format_count_fraction`, `format_fraction_threshold`, `formatting_functions`,
   `format_fraction`, `combination_function` (S4 method), `compare_variables` (S3 method),
   `h_stack_by_baskets`, `h_pkparam_sort`, `h_adsl_adlb_merge_using_worst_flag`, `kaplan_meier`.

*  Internal keywords added, export removed, `_pkgdown.yml` polished and `tern:::` for
   tests, examples, and vignettes when present for the following functions:
   - (chain functions, reference to `abnormal_by_marked`) `s_count_abnormal_by_marked`,
   `a_count_abnormal_by_marked`.
   - (chain functions, reference to `abnormal_by_worst_grade_worsen_from_baseline`)
   `a_count_abnormal_lab_worsen_by_baseline`, `h_worsen_counter` (same file),
   `s_count_abnormal_lab_worsen_by_baseline`.
   - (chain functions, reference to `abnormal_by_worst_grade`) `s_count_abnormal_by_worst_grade`,
   `a_count_abnormal_by_worst_grade`.
   - (chain functions, reference to `survival_timepoint`) `s_surv_timepoint`, `s_surv_timepoint_diff`,
   `a_surv_timepoint`, `a_surv_timepoint_diff`.
   - (chain functions, reference to `survival_time`) `s_surv_time`, `a_surv_time`.
   - (chain functions, reference to `survival_coxph_pairwise`) `s_coxph_pairwise`, `a_coxph_pairwise`.
   - (chain functions, reference to `survival_duration_subgroups`) `a_survival_subgroups`.
   - (chain functions, reference to `count_cumulative`) `h_count_cumulative`,
   `d_count_cumulative`, `s_count_cumulative`, `a_count_cumulative`.
   - (chain functions, reference to `count_missed_doses`) `s_count_nonmissing`, `d_count_missed_doses`, `s_count_missed_doses`, `a_count_missed_doses`.
   - (chain functions, reference to `count_patients_events_in_cols`) `s_count_patients_and_multiple_events`, `summarize_patients_events_in_cols`.
   - (chain functions, reference to `incidence_rate`) `h_incidence_rate_normal`,
   `h_incidence_rate_normal_log`, `h_incidence_rate_exact`, `h_incidence_rate_byar`,
   `h_incidence_rate`, `s_incidence_rate`, `a_incidence_rate`.
   - (cox regression helper) `cox_regression_inter`, `decorate_grob_factory`, `draw_grob`, `estimate_coef`.
   - (reference to `h_survival_duration_subgroups`) `h_survtime_df`, `h_survtime_subgroups_df`, `h_coxph_df`, `h_coxph_subgroups_df`
   - (reference to `h_survival_biomarkers_subgroups`) `h_surv_to_coxreg_variables`,
   `h_coxreg_mult_cont_df`, `h_tab_surv_one_biomarker`.
   - (reference to `h_step`) `h_step_window`, `h_step_trt_effect`, `h_step_survival_formula`,
   `h_step_survival_est`, `h_step_rsp_formula`, `h_step_rsp_est`.
   - (reference to `h_response_biomarkers_subgroups`) `h_rsp_to_logistic_variables`,
   `h_logistic_mult_cont_df`, `h_tab_rsp_one_biomarker`.
   - `summary_labels`, `summary_formats`, `s_count_patients_sum_exposure`,
   `a_change_from_baseline` `s_change_from_baseline`, `a_ancova`, `s_ancova`, `h_ancova`,
   `arrange_grobs`, `as_factor_keep_attributes`, `combine_levels`, `split_text_grob`,
   `groups_list_to_df`, `s_cox_multivariate`, `h_row_counts`, `h_row_fractions`, `h_col_counts`, `is_leaf_table`,
   `h_content_first_row`, `a_response_subgroups`, `range_noinf`, `has_count_in_cols`,
   `has_counts_difference`, `prop_chisq`, `prop_cmh`, `prop_schouten`, `prop_fisher`,
   `s_test_proportion_diff`, `a_test_proportion_diff`,`h_split_param`,
   `fct_collapse_only`, `fct_discard`, `fct_explicit_na_if`.

* Deprecated badge added to `g_mmrm`
* Removed `tern:::` prefix from internal function uses in tests

### Miscellaneous
* Deprecated the `color_palette` function with `nestcolor::color_palette` and removed `color_palette_core`.
* Deprecated `h_set_nest_theme()` in favor of `nestcolor::theme_nest()`.
* Removed deprecated `mmrm` functions: `fit_mmrm`, `g_mmrm_diagnostic`, `g_mmrm_lsmeans`, `as.rtable.mmrm`, `h_mmrm_fixed`,
  `h_mmrm_cov`, `h_mmrm_diagnostic`, `tidy.mmrm`, `s_mmrm_lsmeans`, `s_mmrm_lsmeans_single`, `summarize_lsmeans`.
* Changed function names `arm` to `study_arm` and `extract` to `extract_by_name`.


# tern 0.7.8

### Fix

* `h_split_by_subgroups` documentation warning fix for wrong placing of example block

### Documentation and NAMESPACE polishing

*  Adopting the standard of badges only for `@description` instead of every
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
     `empty_vector_if_na`, `combine_vectors`, `aesi_label`,
     `n_available`, `format_xx`, `arm`.
   - `count_values_funs`, `prop_difference`, `combine_counts`.
   - (chain functions) `s_count_abnormal`, `a_count_abnormal`.
   - (chain functions) `s_count_abnormal_by_baseline`, `a_count_abnormal_by_baseline`,
     `d_count_abnormal_by_baseline`.
*  Deprecated `s_cox_univariate` function has now deprecated badge.

### Enhancements

* Enhanced `g_lineplot` with additional table to automatically scale the table height and return a `ggplot` object.
* Enhanced `g_ipp` with additional caption argument and adjust the position.
* Enhanced `prop_diff`, `tern` function and related functions to be able to apply a continuity correction in the Newcombe method.
* Enhanced `summarize_numeric_in_columns` and `summarize_variables` to allow factor/character summary and to be able to summarize the number of `BLQs` in `AVALC` from `ADPC` dataset.
* Updated order of summarize variables stats in manual for order consistency.
* Added a `sum` option to `summarize_variables`.
* Use consistent color palette for plotting (`stream` by default).
* Enhanced `h_pkparam_sort` function with additional argument `key_var` to allow data with different column names.

### Miscellaneous
* Updated `test-table_aet02.R` variant 12.
* Changed the `scda` data version to '2022-02-28'.
* Added a template to the `pkgdown` site.
* Removed package dependencies of `grDevices`, `stringr`, and `viridisLite`.
* Renaming `summarize_numeric_in_columns` to `summarize_variables_in_columns`.
* Renaming `summarize_vars_numeric_in_cols` to `summarize_vars_in_cols`.
* Fixed a bug where points on the `g_lineplot` plot were not connected in the case of missing values.
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
* Added `h_map_for_count_abnormal` so that the map used in `trim_levels_to_map` split function can be created by calling this helper function. Currently it supports two methods: one with all observed mapping, one with at least low limit above zero and at least one non missing high limit.
* Added `s_summary_numeric_in_cols` and `summarize_vars_numeric_in_cols` functions to generate summary statistics in columns, mainly used for PK datasets.
* Added five statistics to `s_summary.numeric` to use in `s_summary_numeric_in_cols`.

### Enhancements
* Enhanced functions `tabulate_survival_subgroups` and `tabulate_rsp_subgroups` (Survival Duration and Best Overall Response analyses) to calculate `N`-s based on the records considered to create the model.
* Enhanced the function `estimate_proportion` and related functions to be able to apply a continuity correction in the Wilson method.
* Refactored `count_abnormal_by_marked` and related statistics and formatting functions to use a more efficient layout with `.spl_context` argument used for determining denominators and with `trim_levels_to_map` split function under `split_rows_by` to show only the desired levels in the table. This is a breaking change.
* Refactored `count_abnormal_by_worst_grade` and related statistics and formatting functions to use a more efficient layout with `.spl_context` argument used for determining denominators and with `trim_levels_to_map` split function under `split_rows_by` to show only the desired levels in the table. This is a breaking change.
* Refactored `count_abnormal` function and related statistics and formatting functions to use a more efficient layout with `trim_levels_to_map` split function under `split_rows_by` to show only the desired levels in the table. Also updated `abnormal` argument to be able to consider more than one level for each direction. This is a breaking change.
* Enhanced the function `estimate_incidence_rate` and related functions to consider the week as time unit for data input.

### Bug fixes
* Fixed bug in `assertthat` functions that output wrong data frame names and limited length of failure message outputs.

### Miscellaneous
* Removed dependency on `utils.nest` by using the `checkmate` and `purrr` packages for validation and moved `get_free_cores` and `skip_if_too_deep` functions from `utils.nest` into `tern`.

# tern 0.7.5

### New features
* Added functions to estimate continuous biomarker effects across multiple subgroups for survival and binary response endpoints, which can be used to produce corresponding forest plots, see `survival_biomarkers_subgroups` and `response_biomarkers_subgroups`.
* Added `g_lineplot` plot function, including new `h_format_row` helper function and `control_lineplot_vars` function. Removed `g_summary_by`.
* Added new safety helper function `h_stack_by_baskets` to stack events in `SMQ` and/or `CQ` basket flag in `ADAE` data set.

### Enhancements
* Added a couple of new statistics to `s_summary.numeric`. Added `names` attribute to each element of the final list returned by the `s_summary.numeric` function. Added `summary_formats` and `summary_labels` helper functions.
* Added option to also convert logical variables to factor variables in `df_explicit_na`.
* Refactored `h_append_grade_groups` to improve its flexibility, robustness and clearness as well as to make sure the result is ordered according to the order of `grade_groups`. Also, added `remove_single` argument which controls whether the elements of one-element grade groups will be kept in the output or removed.
* Added `var_labels` and `show_labels` arguments to `count_occurrences` and `count_patients_with_flags` to allow for creation of a title row.
* Added `na_level` argument to `count_abnormal_by_baseline`.
* Updated `h_append_grade_groups` to no longer fill-in empty grade groups with zeros.

### Bug Fixes
* Fixed `prop_diff_cmh` to handle edge case of no FALSE (or TRUE) responses.
* Enhanced `g_mmrm_diagnostic` to improve error handling when data is not amenable to the Locally Weighted Scatterplot Smoothing.
* Several fixes in `g_km`:
  * Plot can now display any combination of the annotation tables for number of patients at risk, median survival time, and CoxPH summary.
  * Function will return a warning instead of an error if the `arm` variable includes only one level and `annot_coxph = TRUE`.
  * Lines in the plot now start correctly at time 0 and probability 1.
  * The equals sign can now be used in category labels.

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
* Descriptive statistics returned by `summarize_vars` now include quantiles. In addition, `summarize_vars` now accepts the the control function `control_summarize_vars` to specify details about confidence level for mean and median and quantile details. The `control` argument replaces `conf_level`.
* Added `var_labels` and `show_labels` arguments to `count_occurrences_by_grade`.
* Changed `indent` argument in `append_varlabels` to accept non-negative integer to represent the indent space defined by user. Previous calls with Boolean `indent` will do an integer conversion and produce a warning.

### Bug Fixes
* Corrected `tabulate_survival_subgroups` and related survival forest plot functions to use total number of events, instead of observations, as default for scaling the symbol sizes in the plot. (The user might still use total number of observations manually if they wish to do so.)
* Helper function `h_adsl_adlb_merge_using_worst_flag` will now correctly impute `BTOXGR` for missing visits.

### Miscellaneous
* Deleted `count_abnormal_by_worst_grade_by_baseline` and its related statistic and analysis functions as lab abnormality tables can be achieved using a simpler design.
* Switched data in examples to use `scda` instead of `random.cdisc.data` package.

# tern 0.7.3

### New Features
* Added Subgroup Treatment Effect Pattern (STEP) model fitting functions `fit_rsp_step` and `fit_survival_step`, the corresponding tidy method `tidy.step` as well as the graph function `g_step`.
* Added new layout function `compare_vars` which compares variables of different types between columns and produces a p-value for the comparison to the reference column. It is built on top of the `summarize_vars` functionality.
* Added utility functions:
  * `cut_quantile_bins` cuts a numeric vector into quantile bins.
  * `fct_collapse_only` collapses levels of a factor and only keeps those in the order provided.
  * `fct_explicit_na_if` inserts explicit missings in a factor based on a condition.
  * `range_noinf` is a kind of a wrapper function of `base::range`. It returns `c(NA, NA)` instead of `c(-Inf, Inf)` for zero-length data.

### Enhancements
* Cox regression via `fit_coxreg_univar` and `fit_coxreg_multivar` is now also possible without treatment arm. In the univariate case this means that separate univariate models for the provided covariates are fitted and the corresponding effect estimates can later be tabulated.
* Added `fraction` in result returned by `s_count_occurrences`. It contains a list of numerators and denominators with one element per occurrence.
* Updated `sum_num_patients` and `count_occurrences` for the result `unique` and `count_fraction` to return (0, 0) when input is empty.
* Added a new argument `groups_lists` to `extract_survival_subgroups`, `extract_rsp_subgroups` and associated helper functions which allows to group factor levels of subgroup variables into manually defined groups, enhancing the flexibility of the resulting forest graphs.
* Forest graph function `g_forest` now extracts default arguments from attributes of the input table produced by `tabulate_rsp_subgroups` and `tabulate_survival_subgroups` so that the user does not have to do this manually anymore.
* In `g_km`:
  * Remove arm variable name from arm labels in plot legend and annotation tables.
  * Show symbol used to mark censored cases and match order of arm variable factor levels in the legend.
  * Display hazard ratio and its confidence interval to two decimal places.
  * Updated default position of hazard ratio table to stay on the left bottom corner but above x-axis.
  * Use arm value as strata name in when treatment arm variable only has one level.
* Updated `s_surv_time` function to use a newly created function `range_noinf` instead of `base::range`.
* New argument `no_fillin_visits` added to `h_adsl_adlb_merge_using_worst_flag` to specify the visits that are excluded from post-baseline worst toxicity grade output. Improved `h_adsl_adlb_merge_using_worst_flag` to include variables shared between `adsl` and `adlb`, along with `PARAM`, `PARAMCD`, `ATOXGR`, `BTOXGR` and optionally `AVISIT`, `AVISITN` when `by_visit = TRUE`. Previously, output only contains `USUBJID`, `ARMCD`, `PARAMCD`, `ATOXGR`, and `BTOXGR`.

### Bug Fixes
* Fix bug in `s_surv_timepoint` for cases when there are zero patients at risk.
* Modified `stat_median_ci` function so that when empty var with empty name is passed, no `row names contain missing values` error would show.

### Miscellaneous
* Deprecated `s_cox_univariate` function, use `fit_coxreg_univar` function instead.
* Updated default formats of `hr` and `hr_ci` in `a_coxph_pairwise` and median in `s_surv_time` to align with STREAM.
* Updated the pre-processing code in the files `test-table_ttet01.R` and `test-table_dort01.R` to make sure the analysis variable `EVNT1` has both levels of the factor defined.
* Improved error message when number of levels in a factor variable in a data frame is not as expected.

# tern 0.7.2
* Fixed internal test errors regarding column labels.

# tern 0.7.1
* New argument `position_surv_med` added to `g_km` to move position of the annotation table with median survival times.
* Fixed bug in `g_km` related to the arguments `pch` and `size`. Previously, these arguments were not passed on to helper function `h_ggkm` and so were effectively being ignored.
* Updated `xticks` and `max_time` arguments in `g_km` for greater functionality. `max_time` added as an argument in `h_xticks` to allow this.
* Fixed bug in `prop_diff_cmh` that led to `NaN` weighted proportion difference estimates and missing confidence intervals. Previously, when for at least one stratum no patients from one treatment arm were included, then the estimation did not lead to numeric results.
* Fixed bug in `prop_cmh` which previously gave an error in case of at least one stratum containing less than two observations.
* New argument `n_events` added to `estimate_incidence_rate`.
* New argument `denom` added to `count_occurrences`.
* New arguments `yval` and `ci_ribbon` added to `g_km`.
* Add new individual patient plot function `g_ipp` along with helpers `h_g_ipp` and `h_set_nest_theme`.
* Fixed bug in `count_patients_with_events`, now zero counts without percentage are shown.
* Fixed bug in `get_mmrm_lsmeans` which previously did not allow MMRM analysis of more than 3000 observations.
* Updated `stat_mean_ci` and `stat_median_ci` to correctly handle edge cases with number of elements in input series equal to 1. For such cases, `NA_real_` is now returned, instead of `NA` or `+/-Inf` for confidence interval (CI) estimates.
* Rename `n_lim` argument of `stat_mean_ci` to `n_min` to better reflect its desired meaning.

# tern 0.7.0
This version of `tern` introduces a major rewriting of `tern` due to the change to layout based tabulation in `rtables`. `tern` now does not build tables directly anymore, instead it provides analysis functions to easily build tables, see the examples.
* Counting patients with abnormal values post-baseline with `count_abnormal`.
* Counting patients with graded abnormal values with `count_abnormal_by_worst_grade`.
* Counting patients with abnormal values by baseline status with `count_abnormal_by_baseline`.
* Counting patients with missed doses with `s_count_missed_doses` and `count_missed_doses`.
* Counting patients with event flags with `count_patients_with_event` and `count_patients_with_flags`.
* Summarizing variables with `summarize_vars` (supports numeric, factor, character and logical variables). Note that factors need to have `NA`s converted to `na_level` before use.
* Summarizing change from baseline with `summarize_change`.
* Summarizing variables in columns with `summarize_colvars`.
* Estimating difference in terms of responder proportions with `estimate_proportion_diff`.
* Estimating difference in terms of Odds Ratio with `estimate_odds_ratio`.
* Testing the difference in responder proportions with `test_proportion_diff`.
* Estimating the responder proportion regarding the level of a factor with `estimate_multinomial_response`.
* Fitting and tabulating the results of Cox regressions with `fit_coxreg_univar`, `fit_coxreg_multivar` and `summarize_coxreg`, respectively.
* Pruning occurrence tables (or tables with counts and fractions) with flexible rules, see `?prune_occurrences` for details.
* Sorting occurrence tables using different options, see `?score_occurrences` for details.
* Fitting and tabulating MMRM models with `fit_mmrm` and `as.rtable` and `summarize_lsmeans`, see `?tabulate_mmrm` for details.
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
* The class `splitText` is renamed to `dynamicSplitText` in order to resolve the name conflict with the package `ggpubr`.
* Add `rreplace_format` for tabulation post-processing.
* Add new tern function `t_ancova` to create ANCOVA tables, as well as corresponding elementary table function `t_el_ancova` and summary function `s_ancova`.
* Add new tern function `s_odds_ratio` to estimate Odds Ratio of response between categories, as well as the corresponding elementary table function `t_el_odds_ratio`.
* Additional CI methods (Agresti-Coull, Jeffreys) for `s_proportion`.
* Added new CI methods `anderson-hauck` and `newcombe` to `s_proportion_diff`.
* Additional p-value methods (Fisher's Exact, Chi-Squared Test with Schouten Correction) for `s_test_proportion_diff`.
* The binary summary table function `t_binary_outcome` takes now lists (instead of character vectors) specified by the helper function `control_binary_comparison` as the arguments `strat_analysis` and `unstrat_analysis`. In addition, Odds Ratio estimates and CIs are now included by default and can be removed, similarly to the other subsections of the arm comparison analyses. Also added argument `rsp_multinomial`.
* Add new table function `t_el_multinomial_proportion`.
* Add new table function `t_abn_shift`.
* Add new MMRM analysis function `s_mmrm`, as well as corresponding table functions `t_mmrm_lsmeans`, `t_mmrm_cov`, `t_mmrm_diagnostic`, `t_mmrm_fixed`, and plot functions `g_mmrm_lsmeans`, `g_mmrm_diagnostic`. The results of these match SAS results (up to numeric precision).
* Deprecated old MMRM functions `a_mmrm` and `t_mmrm` (they give a deprecation warning but still work). These will be removed in the next release. The reason is that the results of these functions don't match SAS results.
* Fix bug in `g_km` related to numbers in patients at risk table. Previously, numbers were incorrect for integer time-to-event variable inputs.

# tern 0.6.7

* For functions with `row_by` argument, inputs no longer require use of `nested_by`.
* Add `stat_mean_ci` and `stat_median_ci` for error bars in `ggplot2`.
* Add new tern function `t_coxreg` as single interface for diverse cox regression types.
* Add compound table for binary endpoint: `t_binary_endpoint` and elementary functions: `t_el_proportion`, `t_el_proportion_diff` and `t_el_test_proportion_diff`. The supporting summary functions added are: `s_proportion`, `s_adj_proportion_diff`, `s_proportion_diff` and `s_test_proportion_diff`.
* Added new tern function `t_events_patyear` to create event table adjusted person-years.
* Added new tern function `t_abnormality` and the elementary table function `t_el_abnormality`.
* Removed `grade_levels` argument from `t_events_term_grade_id` functions. If a different ordering of the rows is needed, this must be done through post-processing by reordering the leaves of the table tree. Eventually, a helper function will be provided.
* Added `prune_zero_rows` argument to `t_events_per_term_grade_id` and `t_max_grade_per_id` to not show rows of all zeros as they can clutter the visualization in the Shiny app and make it slower.
* Fixed position of (N=xx) in `t_summary_by` output when numeric columns are summarized in parallel with `compare_in_header`.
* Rename t_coxph to t_coxph_pairwise to reflect the model process, add details in documentation.
* Remove `test.nest` dependency.
* Retain column labels when data is split into tree.

# tern 0.6.6

* Remove `test.nest` dependency.

# tern 0.6.5

* Change default option for denominator to be `N` in `t_summary`.
* Fix IQR bug: IQR as Q3 minus Q1.
* Add new function `t_logistic` for multi-variable logistic regression table.
* Add new function `df_explicit_na` to replace `NA` by explicit values.
* Added possibility in `t_tte` to specify confidence level independent for `survfit`, `coxph`, and `ztest`, see the manual.
* Fixed bug in `t_rsp` of not showing p-value, odds ratio and CIs when `strata_data` is not `NULL`.
* Added stratified analysis for `t_forest_rsp` and `t_forest_tte`, stratified analysis is footnoted in `g_forest`.
* Added `footnotes`, `footnotes<-` and `add_footnotes<-` functions to deal with footnotes.
* Added argument `conf_int` for confidence interval level to `t_el_forest_rps`, `t_forest_rsp`, `t_el_forest_tte`, `t_forest_tte`.
* Added argument `col_symbol_size` to `g_forest` to control the relative size of symbols used in the plot.
* Added `s_coxph_pairwise` function to perform pairwise testing which is used by `t_tte` and `t_coxph`.
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
        - Added `f_numeric` to choose which statistics should be calculated.
    - `t_summary.factor`:
        - `denominator` now also allows for `omit` if the percentages should be omitted.
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

* Fixed colors in Kaplan-Meyer-Plot
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

* `t_summarize_variables` is deprecated as `t_summary` is more powerful.
* `t_summarize_by_visit` will be replaced with `t_summary_by` in an upcoming release.

# tern 0.5.0

* First version where analysis functions names and arguments have been harmonized.
