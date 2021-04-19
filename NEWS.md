* Added `fraction` return in `s_count_occurrences` containing a list of numerators and denominators with one element per occurrence.

# tern 0.7.2.9000
* Updated default position of hazard ratio table in `g_km` to stay
on the left bottom corner but above x-axis for greater legibility.
* Updated `s_surv_time` function to use a newly created function `range_noinf` instead of `base::range`.
* Created `range_noinf` utils function. This is a kind of a wrapper function of `base::range`. It returns `c(NA, NA)` instead of `c(-Inf, Inf)` for zero-length data.
* Updated the pre-processing code in the files `test-table_ttet01.R` and `test-table_dort01.R` to make sure the analysis variable `EVNT1` has both levels of the factor defined.
* Added new layout function `compare_vars` which compares variables of different types between columns and produces a p-value for the comparison to the reference column. It is built on top of the `summarize_vars` functionality.
* Updated `g_km` to show legend for symbol used to mark censored cases on the KM plot.
* Updated `g_km`, `h_tbl_median_surv` and `h_grob_median_surv` to remove arm variable name from arm labels in plot legend and annotation tables.
* `g_km` now respects the ordering of the arm variable factor levels in the resulting Kaplan-Meier curve.
* Added a new argument `armval` to `h_tbl_median_surv` and `h_grob_median_surv` to allow use of arm value as strata name in `g_km` when treatment arm variable only has one level.
* New argument `no_fillin_visits` added to `h_adsl_adlb_merge_using_worst_flag` to specify the visits that are excluded from post-baseline worst toxicity grade output. Improved `h_adsl_adlb_merge_using_worst_flag` to include variables shared between `adsl` and `adlb`, along with `PARAM`, `PARAMCD`, `ATOXGR`, `BTOXGR` and optionally `AVISIT`, `AVISITN` when `by_visit = TRUE`. Previously, output only contains `USUBJID`, `ARMCD`, `PARAMCD`, `ATOXGR`, and `BTOXGR`.
* Added a new argument `groups_lists` to `extract_survival_subgroups`, `extract_rsp_subgroups` and associated helper functions which allows to group factor levels of subgroup variables into manually defined groups, enhancing the flexibility of the resulting forest graphs.
* Cox regression via `fit_coxreg_univar` and `fit_coxreg_multivar` is now also possible without treatment arm. In the univariate case this means that separate univariate models for the provided covariates are fitted and the corresponding effect estimates can later be tabulated.
* Modified `stat_median_ci` function so that when empty var with empty name is passed, no `row names contain missing values` error would show.
* New utility functions to work with factors: `fct_collapse_only` collapses levels of a factor and only keeps those in the order provided, and `fct_explicit_na_if` inserts explicit missings in a factor based on a condition.

# tern 0.7.2
* Fixed internal test errors regarding column labels.

# tern 0.7.1
* New argument `position_surv_med` added to `g_km` to move position of the annotation table with median survival times.  
* Fixed bug in `g_km` related to the arguments `pch` and `size`. Previously, these arguments were not passed on to helper function `h_ggkm` and so were effectively being ignored.
* Updated xticks and max_time arguments in `g_km` for greater functionality. max_time added as an argument in `h_xticks` to allow this. 
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
* Fitting and tabulating MMRMs with `fit_mmrm` and `as.rtable` and `summarize_lsmeans`, see `?tabulate_mmrm` for details.
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
* Removed `grade_levels` argument from `t_events_term_grade_id` functions. If a different ordering of the rows is needed, this must be done through postprocessing by reordering the leaves of the table tree. Eventually, a helper function will be provided.
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
* Added helper function `h_default_forest_header` which gives a reasonable default for `forest_header` argument of `g_forest` if full tables are used, such that the user does not need to do this manually.
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

* Fixed colors in Kaplan-Meyer-Plot https://github.roche.com/NEST/tern/issues/66.
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
