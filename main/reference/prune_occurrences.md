# Occurrence table pruning

**\[stable\]**

Family of constructor and condition functions to flexibly prune
occurrence tables. The condition functions always return whether the row
result is higher than the threshold. Since they are of class
[`CombinationFunction()`](https://insightsengineering.github.io/tern/reference/combination_function.md)
they can be logically combined with other condition functions.

## Usage

``` r
keep_rows(row_condition)

keep_content_rows(content_row_condition)

has_count_in_cols(atleast, ...)

has_count_in_any_col(atleast, ...)

has_fraction_in_cols(atleast, ...)

has_fraction_in_any_col(atleast, ...)

has_fractions_difference(atleast, ...)

has_counts_difference(atleast, ...)
```

## Arguments

- row_condition:

  (`CombinationFunction`)  
  condition function which works on individual analysis rows and flags
  whether these should be kept in the pruned table.

- content_row_condition:

  (`CombinationFunction`)  
  condition function which works on individual first content rows of
  leaf tables and flags whether these leaf tables should be kept in the
  pruned table.

- atleast:

  (`numeric(1)`)  
  threshold which should be met in order to keep the row.

- ...:

  arguments for row or column access, see
  [`rtables_access`](https://insightsengineering.github.io/tern/reference/rtables_access.md):
  either `col_names` (`character`) including the names of the columns
  which should be used, or alternatively `col_indices` (`integer`)
  giving the indices directly instead.

## Value

- `keep_rows()` returns a pruning function that can be used with
  [`rtables::prune_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/prune_table.html)
  to prune an `rtables` table.

&nbsp;

- `keep_content_rows()` returns a pruning function that checks the
  condition on the first content row of leaf tables in the table.

&nbsp;

- `has_count_in_cols()` returns a condition function that sums the
  counts in the specified column.

&nbsp;

- `has_count_in_any_col()` returns a condition function that compares
  the counts in the specified columns with the threshold.

&nbsp;

- `has_fraction_in_cols()` returns a condition function that sums the
  counts in the specified column, and computes the fraction by dividing
  by the total column counts.

&nbsp;

- `has_fraction_in_any_col()` returns a condition function that looks at
  the fractions in the specified columns and checks whether any of them
  fulfill the threshold.

&nbsp;

- `has_fractions_difference()` returns a condition function that
  extracts the fractions of each specified column, and computes the
  difference of the minimum and maximum.

&nbsp;

- `has_counts_difference()` returns a condition function that extracts
  the counts of each specified column, and computes the difference of
  the minimum and maximum.

## Functions

- `keep_rows()`: Constructor for creating pruning functions based on a
  row condition function. This removes all analysis rows (`TableRow`)
  that should be pruned, i.e., don't fulfill the row condition. It
  removes the sub-tree if there are no children left.

- `keep_content_rows()`: Constructor for creating pruning functions
  based on a condition for the (first) content row in leaf tables. This
  removes all leaf tables where the first content row does not fulfill
  the condition. It does not check individual rows. It then proceeds
  recursively by removing the sub tree if there are no children left.

- `has_count_in_cols()`: Constructor for creating condition functions on
  total counts in the specified columns.

- `has_count_in_any_col()`: Constructor for creating condition functions
  on any of the counts in the specified columns satisfying a threshold.

- `has_fraction_in_cols()`: Constructor for creating condition functions
  on total fraction in the specified columns.

- `has_fraction_in_any_col()`: Constructor for creating condition
  functions on any fraction in the specified columns.

- `has_fractions_difference()`: Constructor for creating condition
  function that checks the difference between the fractions reported in
  each specified column.

- `has_counts_difference()`: Constructor for creating condition function
  that checks the difference between the counts reported in each
  specified column.

## Note

Since most table specifications are worded positively, we name our
constructor and condition functions positively, too. However, note that
the result of `keep_rows()` says what should be pruned, to conform with
the
[`rtables::prune_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/prune_table.html)
interface.

## Examples

``` r
# \donttest{
tab <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze_vars("COUNTRY", .stats = "count_fraction") %>%
  build_table(DM)
# }

# \donttest{
# `keep_rows`
is_non_empty <- !CombinationFunction(all_zero_or_na)
prune_table(tab, keep_rows(is_non_empty))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>     GBR                      1 (4.2%)        0              0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }

# `keep_content_rows`
# \donttest{
more_than_twenty <- has_count_in_cols(atleast = 20L, col_names = names(tab))
prune_table(tab, keep_content_rows(more_than_twenty))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>     CHE                         0            0              0       
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>     GBR                      1 (4.2%)        0              0       
#>     CAN                         0            0              0       
#>     CHE                         0            0              0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#>     GBR                         0            0              0       
#>     CAN                         0            0              0       
#>     CHE                         0            0              0       
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     RUS                         0            0              0       
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>     CAN                         0            0              0       
#>     CHE                         0            0              0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     NGA                         0            0              0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>     CHE                         0            0              0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#>     JPN                         0            0              0       
#>     GBR                         0            0              0       
#>     CAN                         0            0              0       
#>     CHE                         0            0              0       
# }

# \donttest{
more_than_one <- has_count_in_cols(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(more_than_one))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>     GBR                      1 (4.2%)        0              0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }

# \donttest{
# `has_count_in_any_col`
any_more_than_one <- has_count_in_any_col(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(any_more_than_one))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>     GBR                      1 (4.2%)        0              0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }

# \donttest{
# `has_fraction_in_cols`
more_than_five_percent <- has_fraction_in_cols(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(more_than_five_percent))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#> BLACK OR AFRICAN AMERICAN                                           
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
# }

# \donttest{
# `has_fraction_in_any_col`
any_atleast_five_percent <- has_fraction_in_any_col(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(any_atleast_five_percent))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }

# \donttest{
# `has_fractions_difference`
more_than_five_percent_diff <- has_fractions_difference(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(more_than_five_percent_diff))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     PAK                      1 (8.3%)     1 (9.1%)      1 (14.3%)   
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }

# \donttest{
more_than_one_diff <- has_counts_difference(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(more_than_one_diff))
#>                             A: Drug X    B: Placebo   C: Combination
#> ————————————————————————————————————————————————————————————————————
#> ASIAN                                                               
#>   A                         27 (22.3%)   20 (18.9%)     31 (24.0%)  
#>     CHN                     14 (51.9%)    9 (45%)       12 (38.7%)  
#>     USA                      2 (7.4%)      1 (5%)       8 (25.8%)   
#>     BRA                      1 (3.7%)     4 (20%)        1 (3.2%)   
#>     PAK                     3 (11.1%)     2 (10%)        2 (6.5%)   
#>     NGA                     3 (11.1%)      1 (5%)        3 (9.7%)   
#>     RUS                      2 (7.4%)      1 (5%)        1 (3.2%)   
#>     JPN                         0          1 (5%)        2 (6.5%)   
#>     GBR                         0          1 (5%)        1 (3.2%)   
#>     CAN                      2 (7.4%)        0           1 (3.2%)   
#>   B                         24 (19.8%)   29 (27.4%)     22 (17.1%)  
#>     CHN                      12 (50%)    13 (44.8%)      11 (50%)   
#>     USA                      2 (8.3%)    5 (17.2%)       1 (4.5%)   
#>     BRA                     4 (16.7%)    3 (10.3%)       1 (4.5%)   
#>     PAK                      2 (8.3%)     2 (6.9%)      4 (18.2%)   
#>     NGA                      2 (8.3%)     1 (3.4%)      3 (13.6%)   
#>     RUS                      1 (4.2%)     1 (3.4%)       2 (9.1%)   
#>     JPN                         0        4 (13.8%)          0       
#>     GBR                      1 (4.2%)        0              0       
#>   C                         28 (23.1%)   19 (17.9%)     31 (24.0%)  
#>     CHN                     13 (46.4%)   10 (52.6%)     16 (51.6%)  
#>     USA                     3 (10.7%)    3 (15.8%)      4 (12.9%)   
#>     BRA                      1 (3.6%)     1 (5.3%)      4 (12.9%)   
#>     PAK                      1 (3.6%)     1 (5.3%)       3 (9.7%)   
#>     NGA                     4 (14.3%)     1 (5.3%)       2 (6.5%)   
#>     RUS                     4 (14.3%)    2 (10.5%)       1 (3.2%)   
#>     JPN                      2 (7.1%)     1 (5.3%)       1 (3.2%)   
#> BLACK OR AFRICAN AMERICAN                                           
#>   A                          6 (5.0%)     7 (6.6%)       8 (6.2%)   
#>     CHN                      3 (50%)     3 (42.9%)      5 (62.5%)   
#>     USA                     2 (33.3%)    1 (14.3%)      1 (12.5%)   
#>     BRA                         0        1 (14.3%)          0       
#>     PAK                         0            0          1 (12.5%)   
#>     NGA                         0            0          1 (12.5%)   
#>     JPN                     1 (16.7%)        0              0       
#>     GBR                         0        2 (28.6%)          0       
#>   B                         10 (8.3%)     6 (5.7%)      12 (9.3%)   
#>     CHN                      3 (30%)     2 (33.3%)      8 (66.7%)   
#>     USA                      1 (10%)         0           1 (8.3%)   
#>     BRA                      2 (20%)     1 (16.7%)       1 (8.3%)   
#>     PAK                         0        1 (16.7%)          0       
#>     RUS                         0        1 (16.7%)          0       
#>     JPN                      2 (20%)         0           1 (8.3%)   
#>     GBR                      1 (10%)         0           1 (8.3%)   
#>     CAN                      1 (10%)     1 (16.7%)          0       
#>   C                         12 (9.9%)    11 (10.4%)      7 (5.4%)   
#>     CHN                     8 (66.7%)    5 (45.5%)      5 (71.4%)   
#>     USA                      1 (8.3%)    2 (18.2%)      1 (14.3%)   
#>     BRA                      1 (8.3%)    2 (18.2%)          0       
#>     NGA                         0         1 (9.1%)          0       
#>     RUS                      1 (8.3%)        0              0       
#> WHITE                                                               
#>   A                          3 (2.5%)     6 (5.7%)       6 (4.7%)   
#>     CHN                     2 (66.7%)    2 (33.3%)       3 (50%)    
#>     USA                     1 (33.3%)    1 (16.7%)          0       
#>     PAK                         0        1 (16.7%)      1 (16.7%)   
#>     NGA                         0        1 (16.7%)          0       
#>     RUS                         0            0          1 (16.7%)   
#>     JPN                         0        1 (16.7%)          0       
#>     CAN                         0            0          1 (16.7%)   
#>   B                          7 (5.8%)     5 (4.7%)       4 (3.1%)   
#>     CHN                     4 (57.1%)     1 (20%)        3 (75%)    
#>     USA                         0         1 (20%)           0       
#>     BRA                         0         1 (20%)           0       
#>     PAK                     1 (14.3%)        0              0       
#>     NGA                     1 (14.3%)        0              0       
#>     RUS                     1 (14.3%)        0           1 (25%)    
#>     JPN                         0         1 (20%)           0       
#>     CAN                         0         1 (20%)           0       
#>   C                          4 (3.3%)     3 (2.8%)       8 (6.2%)   
#>     CHN                      3 (75%)      3 (100%)       6 (75%)    
#>     USA                      1 (25%)         0          1 (12.5%)   
#>     JPN                         0            0          1 (12.5%)   
# }
```
