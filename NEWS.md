
# tern 0.5.1

## New TLGs

* `t_ae_ctc`: Adverse Events Table by Highest NCI CTCAE Grade
* `t_max_grade_per_id`: Tabulate maximum grade per id by col_by
* `g_waterfall`: Horizontal Waterfall Plot

## TLG changes

* `t_tte` now shows two rows with ranges for event and censored times,
respectively.

## Bug Fixes

* `t_summarise_variables` uses now `n` instead of `N` as a denominator for
calculating percentages for factors.

## Package Internal Changes

* added function `compound_table`

* in forest plot functions, added formatting to display extreme values to
">999.9".

* added `t_ae_ctc` adverse events table by highest NCI CTCAE grade function.

# tern 0.5.0

First version where analysis functions names and arguments have been harmonized. 