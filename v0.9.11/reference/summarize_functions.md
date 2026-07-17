# Summarize functions

These functions are wrappers for
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html),
applying corresponding `tern` content functions to add summary rows to a
given table layout:

## Details

- [`add_rowcounts()`](https://insightsengineering.github.io/tern/reference/add_rowcounts.md)

- [`estimate_multinomial_response()`](https://insightsengineering.github.io/tern/reference/estimate_multinomial_rsp.md)
  (with
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html))

- [`logistic_summary_by_flag()`](https://insightsengineering.github.io/tern/reference/logistic_summary_by_flag.md)

- [`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md)

- [`summarize_occurrences()`](https://insightsengineering.github.io/tern/reference/count_occurrences.md)

- [`summarize_occurrences_by_grade()`](https://insightsengineering.github.io/tern/reference/count_occurrences_by_grade.md)

- [`summarize_patients_events_in_cols()`](https://insightsengineering.github.io/tern/reference/count_patients_events_in_cols.md)

- [`summarize_patients_exposure_in_cols()`](https://insightsengineering.github.io/tern/reference/summarize_patients_exposure_in_cols.md)

Additionally, the
[`summarize_coxreg()`](https://insightsengineering.github.io/tern/reference/cox_regression.md)
function utilizes
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
(in combination with several other `rtables` functions like
[`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html))
to output a Cox regression summary table.

## See also

- [analyze_functions](https://insightsengineering.github.io/tern/reference/analyze_functions.md)
  for functions which are wrappers for
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

- [analyze_colvars_functions](https://insightsengineering.github.io/tern/reference/analyze_colvars_functions.md)
  for functions that are wrappers for
  [`rtables::analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html).
