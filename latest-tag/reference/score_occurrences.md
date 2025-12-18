# Occurrence table sorting

**\[stable\]**

Functions to score occurrence table subtables and rows which can be used
in the sorting of occurrence tables.

## Usage

``` r
score_occurrences(table_row)

score_occurrences_cols(...)

score_occurrences_subtable(...)

score_occurrences_cont_cols(...)
```

## Arguments

- table_row:

  (`TableRow`)  
  an analysis row in a occurrence table.

- ...:

  arguments for row or column access, see
  [`rtables_access`](https://insightsengineering.github.io/tern/reference/rtables_access.md):
  either `col_names` (`character`) including the names of the columns
  which should be used, or alternatively `col_indices` (`integer`)
  giving the indices directly instead.

## Value

- `score_occurrences()` returns the sum of counts across all columns of
  a table row.

&nbsp;

- `score_occurrences_cols()` returns a function that sums counts across
  all specified columns of a table row.

&nbsp;

- `score_occurrences_subtable()` returns a function that sums counts in
  each subtable across all specified columns.

&nbsp;

- `score_occurrences_cont_cols()` returns a function that sums counts in
  the first content row in specified columns.

## Functions

- `score_occurrences()`: Scoring function which sums the counts across
  all columns. It will fail if anything else but counts are used.

- `score_occurrences_cols()`: Scoring functions can be produced by this
  constructor to only include specific columns in the scoring. See
  [`h_row_counts()`](https://insightsengineering.github.io/tern/reference/rtables_access.md)
  for further information.

- `score_occurrences_subtable()`: Scoring functions produced by this
  constructor can be used on subtables: They sum up all specified column
  counts in the subtable. This is useful when there is no available
  content row summing up these counts.

- `score_occurrences_cont_cols()`: Produces a score function for sorting
  table by summing the first content row in specified columns. Note that
  this is extending
  [`rtables::cont_n_onecol()`](https://insightsengineering.github.io/rtables/latest-tag/reference/score_funs.html)
  and
  [`rtables::cont_n_allcols()`](https://insightsengineering.github.io/rtables/latest-tag/reference/score_funs.html).

## See also

[`h_row_first_values()`](https://insightsengineering.github.io/tern/reference/rtables_access.md)

[`h_row_counts()`](https://insightsengineering.github.io/tern/reference/rtables_access.md)

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique"),
    .labels = c("Total number of patients with at least one event")
  ) %>%
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      "Total number of patients with at least one event",
      "Total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD")

tbl <- build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl) %>%
  prune_table()

tbl_sorted <- tbl %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

tbl_sorted
#>                                                      A: Drug X    B: Placebo   C: Combination
#>                                                        (N=69)       (N=73)         (N=58)    
#> —————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one event     59 (85.5%)   57 (78.1%)     48 (82.8%)  
#> cl D.1                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     20 (34.5%)  
#>   Total number of events                                 39           35             28      
#>     dcd D.1.1.4.2                                    16 (23.2%)   13 (17.8%)     16 (27.6%)  
#>     dcd D.1.1.1.1                                    17 (24.6%)   18 (24.7%)     7 (12.1%)   
#> cl C.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   13 (17.8%)     18 (31.0%)  
#>   Total number of events                                 17           15             19      
#>     dcd C.1.1.1.3                                    15 (21.7%)   13 (17.8%)     18 (31.0%)  
#> cl C.2                                                                                       
#>   Total number of patients with at least one event   20 (29.0%)   14 (19.2%)     10 (17.2%)  
#>   Total number of events                                 21           15             14      
#>     dcd C.2.1.2.1                                    20 (29.0%)   14 (19.2%)     10 (17.2%)  
#> cl B.2                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     24 (41.4%)  
#>   Total number of events                                 40           38             33      
#>     dcd B.2.2.3.1                                    17 (24.6%)   15 (20.5%)     16 (27.6%)  
#>     dcd B.2.1.2.1                                    17 (24.6%)   16 (21.9%)     13 (22.4%)  
#> cl B.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   19 (26.0%)     15 (25.9%)  
#>   Total number of events                                 19           19             18      
#>     dcd B.1.1.1.1                                    15 (21.7%)   19 (26.0%)     15 (25.9%)  
#> cl D.2                                                                                       
#>   Total number of patients with at least one event   21 (30.4%)   20 (27.4%)     12 (20.7%)  
#>   Total number of events                                 27           22             15      
#>     dcd D.2.1.5.3                                    21 (30.4%)   20 (27.4%)     12 (20.7%)  
#> cl A.1                                                                                       
#>   Total number of patients with at least one event   31 (44.9%)   24 (32.9%)     27 (46.6%)  
#>   Total number of events                                 39           33             35      
#>     dcd A.1.1.1.1                                    17 (24.6%)   17 (23.3%)     14 (24.1%)  
#>     dcd A.1.1.1.2                                    17 (24.6%)   14 (19.2%)     17 (29.3%)  

score_cols_a_and_b <- score_occurrences_cols(col_names = c("A: Drug X", "B: Placebo"))

# Note that this here just sorts the AEDECOD inside the AEBODSYS. The AEBODSYS are not sorted.
# That would require a second pass of `sort_at_path`.
tbl_sorted <- tbl %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_cols_a_and_b)

tbl_sorted
#>                                                      A: Drug X    B: Placebo   C: Combination
#>                                                        (N=69)       (N=73)         (N=58)    
#> —————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one event     59 (85.5%)   57 (78.1%)     48 (82.8%)  
#> cl D.1                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     20 (34.5%)  
#>   Total number of events                                 39           35             28      
#>     dcd D.1.1.1.1                                    17 (24.6%)   18 (24.7%)     7 (12.1%)   
#>     dcd D.1.1.4.2                                    16 (23.2%)   13 (17.8%)     16 (27.6%)  
#> cl C.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   13 (17.8%)     18 (31.0%)  
#>   Total number of events                                 17           15             19      
#>     dcd C.1.1.1.3                                    15 (21.7%)   13 (17.8%)     18 (31.0%)  
#> cl C.2                                                                                       
#>   Total number of patients with at least one event   20 (29.0%)   14 (19.2%)     10 (17.2%)  
#>   Total number of events                                 21           15             14      
#>     dcd C.2.1.2.1                                    20 (29.0%)   14 (19.2%)     10 (17.2%)  
#> cl B.2                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     24 (41.4%)  
#>   Total number of events                                 40           38             33      
#>     dcd B.2.1.2.1                                    17 (24.6%)   16 (21.9%)     13 (22.4%)  
#>     dcd B.2.2.3.1                                    17 (24.6%)   15 (20.5%)     16 (27.6%)  
#> cl B.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   19 (26.0%)     15 (25.9%)  
#>   Total number of events                                 19           19             18      
#>     dcd B.1.1.1.1                                    15 (21.7%)   19 (26.0%)     15 (25.9%)  
#> cl D.2                                                                                       
#>   Total number of patients with at least one event   21 (30.4%)   20 (27.4%)     12 (20.7%)  
#>   Total number of events                                 27           22             15      
#>     dcd D.2.1.5.3                                    21 (30.4%)   20 (27.4%)     12 (20.7%)  
#> cl A.1                                                                                       
#>   Total number of patients with at least one event   31 (44.9%)   24 (32.9%)     27 (46.6%)  
#>   Total number of events                                 39           33             35      
#>     dcd A.1.1.1.1                                    17 (24.6%)   17 (23.3%)     14 (24.1%)  
#>     dcd A.1.1.1.2                                    17 (24.6%)   14 (19.2%)     17 (29.3%)  

score_subtable_all <- score_occurrences_subtable(col_names = names(tbl))

# Note that this code just sorts the AEBODSYS, not the AEDECOD within AEBODSYS. That
# would require a second pass of `sort_at_path`.
tbl_sorted <- tbl %>%
  sort_at_path(path = c("AEBODSYS"), scorefun = score_subtable_all, decreasing = FALSE)

tbl_sorted
#>                                                      A: Drug X    B: Placebo   C: Combination
#>                                                        (N=69)       (N=73)         (N=58)    
#> —————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one event     59 (85.5%)   57 (78.1%)     48 (82.8%)  
#> cl C.2                                                                                       
#>   Total number of patients with at least one event   20 (29.0%)   14 (19.2%)     10 (17.2%)  
#>   Total number of events                                 21           15             14      
#>     dcd C.2.1.2.1                                    20 (29.0%)   14 (19.2%)     10 (17.2%)  
#> cl C.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   13 (17.8%)     18 (31.0%)  
#>   Total number of events                                 17           15             19      
#>     dcd C.1.1.1.3                                    15 (21.7%)   13 (17.8%)     18 (31.0%)  
#> cl B.1                                                                                       
#>   Total number of patients with at least one event   15 (21.7%)   19 (26.0%)     15 (25.9%)  
#>   Total number of events                                 19           19             18      
#>     dcd B.1.1.1.1                                    15 (21.7%)   19 (26.0%)     15 (25.9%)  
#> cl D.2                                                                                       
#>   Total number of patients with at least one event   21 (30.4%)   20 (27.4%)     12 (20.7%)  
#>   Total number of events                                 27           22             15      
#>     dcd D.2.1.5.3                                    21 (30.4%)   20 (27.4%)     12 (20.7%)  
#> cl D.1                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     20 (34.5%)  
#>   Total number of events                                 39           35             28      
#>     dcd D.1.1.1.1                                    17 (24.6%)   18 (24.7%)     7 (12.1%)   
#>     dcd D.1.1.4.2                                    16 (23.2%)   13 (17.8%)     16 (27.6%)  
#> cl B.2                                                                                       
#>   Total number of patients with at least one event   29 (42.0%)   27 (37.0%)     24 (41.4%)  
#>   Total number of events                                 40           38             33      
#>     dcd B.2.1.2.1                                    17 (24.6%)   16 (21.9%)     13 (22.4%)  
#>     dcd B.2.2.3.1                                    17 (24.6%)   15 (20.5%)     16 (27.6%)  
#> cl A.1                                                                                       
#>   Total number of patients with at least one event   31 (44.9%)   24 (32.9%)     27 (46.6%)  
#>   Total number of events                                 39           33             35      
#>     dcd A.1.1.1.1                                    17 (24.6%)   17 (23.3%)     14 (24.1%)  
#>     dcd A.1.1.1.2                                    17 (24.6%)   14 (19.2%)     17 (29.3%)  
```
