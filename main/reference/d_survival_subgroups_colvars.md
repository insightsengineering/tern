# Labels for column variables in survival duration by subgroup table

**\[stable\]**

Internal function to check variables included in
[`tabulate_survival_subgroups()`](https://insightsengineering.github.io/tern/reference/survival_duration_subgroups.md)
and create column labels.

## Usage

``` r
d_survival_subgroups_colvars(vars, conf_level, method, time_unit = NULL)
```

## Arguments

- vars:

  (`character`)  
  the names of statistics to be reported among:

  - `n_tot_events`: Total number of events per group.

  - `n_events`: Number of events per group.

  - `n_tot`: Total number of observations per group.

  - `n`: Number of observations per group.

  - `median`: Median survival time.

  - `hr`: Hazard ratio.

  - `ci`: Confidence interval of hazard ratio.

  - `pval`: p-value of the effect. Note, one of the statistics `n_tot`
    and `n_tot_events`, as well as both `hr` and `ci` are required.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string`)  
  p-value method for testing hazard ratio = 1.

- time_unit:

  (`string`)  
  label with unit of median survival time. Default `NULL` skips
  displaying unit.

## Value

A `list` of variables and their labels to tabulate.

## Note

At least one of `n_tot` and `n_tot_events` must be provided in `vars`.
