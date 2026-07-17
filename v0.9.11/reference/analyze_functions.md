# Analyze functions

These functions are wrappers of
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
which apply corresponding `tern` statistics functions to add an analysis
to a given table layout:

- [`analyze_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md)

- [`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)

- [`compare_vars()`](https://insightsengineering.github.io/tern/reference/compare_variables.md)

- [`count_abnormal()`](https://insightsengineering.github.io/tern/reference/abnormal.md)

- [`count_abnormal_by_baseline()`](https://insightsengineering.github.io/tern/reference/abnormal_by_baseline.md)

- [`count_abnormal_by_marked()`](https://insightsengineering.github.io/tern/reference/abnormal_by_marked.md)

- [`count_abnormal_by_worst_grade()`](https://insightsengineering.github.io/tern/reference/abnormal_by_worst_grade.md)

- [`count_cumulative()`](https://insightsengineering.github.io/tern/reference/count_cumulative.md)

- [`count_missed_doses()`](https://insightsengineering.github.io/tern/reference/count_missed_doses.md)

- [`count_occurrences()`](https://insightsengineering.github.io/tern/reference/count_occurrences.md)

- [`count_occurrences_by_grade()`](https://insightsengineering.github.io/tern/reference/count_occurrences_by_grade.md)

- [`count_patients_events_in_cols()`](https://insightsengineering.github.io/tern/reference/count_patients_events_in_cols.md)

- [`count_patients_with_event()`](https://insightsengineering.github.io/tern/reference/count_patients_with_event.md)

- [`count_patients_with_flags()`](https://insightsengineering.github.io/tern/reference/count_patients_with_flags.md)

- [`count_values()`](https://insightsengineering.github.io/tern/reference/count_values.md)

- [`coxph_pairwise()`](https://insightsengineering.github.io/tern/reference/survival_coxph_pairwise.md)

- [`estimate_incidence_rate()`](https://insightsengineering.github.io/tern/reference/incidence_rate.md)

- [`estimate_multinomial_rsp()`](https://insightsengineering.github.io/tern/reference/estimate_multinomial_rsp.md)

- [`estimate_odds_ratio()`](https://insightsengineering.github.io/tern/reference/odds_ratio.md)

- [`estimate_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md)

- [`estimate_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff.md)

- [`summarize_ancova()`](https://insightsengineering.github.io/tern/reference/summarize_ancova.md)

- [`summarize_colvars()`](https://insightsengineering.github.io/tern/reference/summarize_colvars.md):
  even if this function uses
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html),
  it applies the analysis methods as different rows for one or more
  variables that are split into different columns. In comparison,
  [analyze_colvars_functions](https://insightsengineering.github.io/tern/reference/analyze_colvars_functions.md)
  leverage `analyze_colvars` to have the context split in rows and the
  analysis methods in columns.

- [`summarize_change()`](https://insightsengineering.github.io/tern/reference/summarize_change.md)

- [`surv_time()`](https://insightsengineering.github.io/tern/reference/survival_time.md)

- [`surv_timepoint()`](https://insightsengineering.github.io/tern/reference/survival_timepoint.md)

- [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md)

## See also

- [analyze_colvars_functions](https://insightsengineering.github.io/tern/reference/analyze_colvars_functions.md)
  for functions that are wrappers for
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html).

- [summarize_functions](https://insightsengineering.github.io/tern/reference/summarize_functions.md)
  for functions which are wrappers for
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).
