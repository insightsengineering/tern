# tern 0.6.4.9000

# tern 0.6.4

* Refactored after renaming functions in `utils.nest`
* Added `event_type` argument to `t_events_per_term_grade_id`
* Added one/two-arm t-test functions
* improvements in `t_summary_by`
* internal code refactoring

# tern 0.6.3

* Added `node` S4 class to create trees:
    - for all related tree functions see the reference under trees 

* Removed functions:
    - Moved all the label functions to `rtables`
    - deleted `keys` and `keys<-` functions

* New helper functions:
    - exported `tabulate_pairwise`
    - `get_N`, `col_N_add_total`, `check_id`
    - `na_as_level`
    - `as_factor_keep_attributes`
    - `r_by`

* New TLGs:
    - disposition elementary table `t_el_disposition`
    - `t_el_forest_tte`, `t_el_forest_rsp`

* Changed Arguments:
    - all compound tables
        - added `table_tree` argument which returns a `node` object
    - `t_summary.numeric`
        - added `f_numeric` to choose which statistics should be calculted
    - `t_summary.factor`
        - `denominator` now also allows for `omit` if the percentages should be omitted.
    - `t_summary_by`
        - renamed `by` to `row_by`
    - `t_forest_rsp`, `t_forest_tte`
        - changed functionality of `group_data` using `row_by_list`
        - removed `na_omit_group`
    - `t_count_unique`
        - removed `indent` argument, use the `indent` funtion in `rtables` instead

# tern 0.6.2

* use chached data from `random.cdisc.data` to speed up testing
* added `t_summary.Date` method
* added `save_join`

# tern 0.6.1

* Fixed colors in Kaplan-Meyer-Plot https://github.roche.com/NEST/tern/issues/66
* Refactor of all functions to pass `test.nest` tests:
    * changed `width_row.names` argument of `g_forest` function into `width_row_names`
    * changed `censor.show` argument of `g_km` function into `censor_show`
    * changed `col.legend.title` argument of `g_waterfall` function into `col_legend_title`
    * changed `na.rm` argument of `t_count_unique` function into `na_rm`
    * changed `row.name` argument of `t_count_unique` function into `row_name`
    * changed `na.omit.group` argument of `t_forest_rsp` function into `na_omit_group`
    * changed `na.omit.group` argument of `t_forest_tte` function into `na_omit_group`
    * changed `row.name.TRUE` and `row.name.FALSE` arguments of `t_summary.logical` into `row_name_true` and `row_name_false` respectively
    * rename `splotTextGrob` into `split_text_grob`
    * fix examples
    * refactor of internal functions code

# tern 0.6.0

* Removed functions `addTable`, `t_summarize_by_visit`, `t_summarize_variables`.
* Added `t_summary_by` function.
* Refactor of `g_km` function, renamed `kmGrob` into `kmCurveGrob`.
* Refactor `t_events_*` family of functions.
* Updated examples.

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
