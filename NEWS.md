
# tern 0.5.0.3 

## New TLGs

* `t_summary` and methods for `data.frame`, `numeric`, `logical`, `character`,
`factor`, and `Date` objects
* `t_events_per_term_id`, `t_events_per_term_grade_id`: Adverse Events &
Concomitant Treatment Tables
* `t_max_grade_per_id`, `t_count_unique`, `t_events_summary` elementary tables
used for the Adverse Events & Concomitant Treatment Tables
* `g_waterfall`: Horizontal Waterfall Plot

## New Helper Functions

* `decorate_grob`, `decorate_grob_set`, `decorate_grob_factory`, `splitTextGrob`
* `stack_grobs`, `arrange_grobs`, `draw_grob`

## TLG changes

* `t_tte` now shows two rows with ranges for event and censored times,
respectively.
* `g_km` works with one arm `survfit` objects
* in forest plot functions, added formatting to display extreme values to
">999.9".
* `t_summarise_variables` uses now `n` instead of `N` as a denominator for
calculating percentages for factors by default.
* `t_rsp` now works when all response values are `TRUE` or `FALSE`

## Deprecated Functions

* `t_summarize_variables` is deprecated as `t_summary` is more powerful
* `t_summarize_by_visit` will be replaced with `t_summary_by` in an upcoming release.

# tern 0.5.0

First version where analysis functions names and arguments have been harmonized. 