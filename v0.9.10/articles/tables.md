# Tabulation

## `tern` Tabulation

The `tern` R package provides functions to create common analyses from
clinical trials in `R`. The core functionality for tabulation is built
on the more general purpose `rtables` package. New users should first
begin by reading the [“Introduction to
tern”](https://insightsengineering.github.io/tern/latest-tag/articles/tern.html)
and [“Introduction to
`rtables`”](https://insightsengineering.github.io/rtables/latest-release/articles/rtables.html)
vignettes.

The packages used in this vignette are:

``` r
library(rtables)
library(tern)
library(dplyr)
```

The datasets used in this vignette are:

``` r
adsl <- ex_adsl
adae <- ex_adae
adrs <- ex_adrs
```

## `tern` Analyze Functions

Analyze functions are used in combination with the `rtables` layout
functions, in the pipeline which creates the `rtables` table. They apply
some statistical logic to the layout of the `rtables` table. The table
layout is materialized with the
[`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)
function and the data.

The `tern` analyze functions are wrappers around
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
function, they offer various methods useful from the perspective of
clinical trials and other statistical projects.

Examples of the `tern` analyze functions are `count_occurrences`,
`summarize_ancova` or `analyze_vars`. As there is no one prefix to
identify all `tern` analyze functions it is recommended to use the [the
tern website functions
reference](https://insightsengineering.github.io/tern/latest-tag/reference/index.html).

### Internals of `tern` Analyze Functions

**Please skip this subsection if you are not interested in the internals
of `tern` analyze functions.**

Internally `tern` analyze functions like `summarize_ancova` are mainly
built in the 4 elements chain:

    h_ancova() -> tern:::s_ancova() -> tern:::a_ancova() -> summarize_ancova()

The descriptions for each function type:

- analysis helper functions `h_*`. These functions are useful to help
  define the analysis.
- statistics function `s_*`. Statistics functions should do the
  computation of the numbers that are tabulated later. In order to
  separate computation from formatting, they should not take care of
  `rcell` type formatting themselves.
- formatted analysis functions `a_*`. These apply formatting to results
  from their corresponding statistics functions. They are used as `afun`
  in
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).
- analyze functions `rtables::analyze(..., afun = tern::a_*)`. Analyze
  functions are used in combination with the `rtables` layout functions,
  in the pipeline which creates the table. They are the last element of
  the chain.

We will use the native
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
function with the `tern` formatted analysis functions as a `afun`
parameter.

    l <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        split_rows_by(var = "AVISIT") %>%
        analyze(vars = "AVAL", afun = a_summary)

    build_table(l, df = adrs)

## Tabulation Examples

We are going to create 3 different tables using `tern` analyze functions
and the `rtables` interface.

| Table | `tern` analyze functions |
|----|:---|
| **Demographic Table** | [`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md) and [`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md) |
| **Adverse event Table** | [`count_occurrences()`](https://insightsengineering.github.io/tern/reference/count_occurrences.md) |
| **Response Table** | [`estimate_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md), [`estimate_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff.md) and [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md) |

### Demographic Table

Demographic tables provide a summary of the characteristics of patients
enrolled in a clinical trial. Typically the table columns represent
treatment arms and variables summarized in the table are demographic
properties such as age, sex, race, etc.

In the example below the only function from `tern` is
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
and the remaining layout functions are from `rtables`.

``` r
# Select variables to include in table.
vars <- c("AGE", "SEX")
var_labels <- c("Age (yr)", "Sex")

basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
#>                       A: Drug X    B: Placebo    C: Combination   All Patients
#>                        (N=134)       (N=134)        (N=132)         (N=400)   
#> ——————————————————————————————————————————————————————————————————————————————
#> Age (yr)                                                                      
#>   n                      134           134            132             400     
#>   Mean (SD)          33.8 (6.6)    35.4 (7.9)      35.4 (7.7)      34.9 (7.4) 
#>   Median                33.0          35.0            35.0            34.0    
#>   Min - Max          21.0 - 50.0   21.0 - 62.0    20.0 - 69.0     20.0 - 69.0 
#> Sex                                                                           
#>   n                      134           134            132             400     
#>   F                   79 (59%)     77 (57.5%)       66 (50%)      222 (55.5%) 
#>   M                  51 (38.1%)     55 (41%)       60 (45.5%)     166 (41.5%) 
#>   U                   3 (2.2%)      2 (1.5%)         4 (3%)         9 (2.2%)  
#>   UNDIFFERENTIATED    1 (0.7%)          0           2 (1.5%)        3 (0.8%)
```

To change the display order of categorical variables in a table use
factor variables and explicitly set the order of the levels. This is the
case for the display order in columns and rows. Note that the `forcats`
package has many useful functions to help with these types of data
processing steps (not used below).

``` r
# Reorder the levels in the ARM variable.
adsl$ARM <- factor(adsl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))

# Reorder the levels in the SEX variable.
adsl$SEX <- factor(adsl$SEX, levels = c("M", "F", "U", "UNDIFFERENTIATED"))

basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
#>                      B: Placebo     A: Drug X    C: Combination   All Patients
#>                        (N=134)       (N=134)        (N=132)         (N=400)   
#> ——————————————————————————————————————————————————————————————————————————————
#> Age (yr)                                                                      
#>   n                      134           134            132             400     
#>   Mean (SD)          35.4 (7.9)    33.8 (6.6)      35.4 (7.7)      34.9 (7.4) 
#>   Median                35.0          33.0            35.0            34.0    
#>   Min - Max          21.0 - 62.0   21.0 - 50.0    20.0 - 69.0     20.0 - 69.0 
#> Sex                                                                           
#>   n                      134           134            132             400     
#>   M                   55 (41%)     51 (38.1%)      60 (45.5%)     166 (41.5%) 
#>   F                  77 (57.5%)     79 (59%)        66 (50%)      222 (55.5%) 
#>   U                   2 (1.5%)      3 (2.2%)         4 (3%)         9 (2.2%)  
#>   UNDIFFERENTIATED        0         1 (0.7%)        2 (1.5%)        3 (0.8%)
```

The `tern` package includes many functions similar to
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).
These functions are called layout creating functions and are used in
combination with other `rtables` layout functions just like in the
examples above. Layout creating functions are wrapping calls to
`rtables`
[`analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html),
[`analyze_colvars()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze_colvars.html)
and
[`summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
and provide options for easy formatting and analysis modifications.

To customize the display for the demographics table, we can do so via
the arguments in
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).
Most layout creating functions in `tern` include the standard arguments
`.stats`, `.formats`, `.labels` and `.indent_mods` which control which
statistics are displayed and how the numbers are formatted. Refer to the
package help with
[`help("analyze_vars")`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
or
[`?analyze_vars`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
to see the full set of options.

For this example we will change the default summary for numeric
variables to include the number of records, and the mean and standard
deviation (in a single statistic, i.e. within a single cell). For
categorical variables we modify the summary to include the number of
records and the counts of categories. We also modify the display format
for the mean and standard deviation to print two decimal places instead
of just one.

``` r
# Select statistics and modify default formats.
basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels,
    .stats = c("n", "mean_sd", "count"),
    .formats = c(mean_sd = "xx.xx (xx.xx)")
  ) %>%
  build_table(adsl)
#>                       B: Placebo     A: Drug X     C: Combination   All Patients
#>                        (N=134)        (N=134)         (N=132)         (N=400)   
#> ————————————————————————————————————————————————————————————————————————————————
#> Age (yr)                                                                        
#>   n                      134            134             132             400     
#>   Mean (SD)          35.43 (7.90)   33.77 (6.55)    35.43 (7.72)    34.88 (7.44)
#> Sex                                                                             
#>   n                      134            134             132             400     
#>   M                       55             51              60             166     
#>   F                       77             79              66             222     
#>   U                       2              3               4               9      
#>   UNDIFFERENTIATED        0              1               2               3
```

One feature of a `layout` is that it can be used with different datasets
to create different summaries. For example, here we can easily create
the same summary of demographics for the Brazil and China subgroups,
respectively:

``` r
lyt <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  )

build_table(lyt, df = adsl %>% dplyr::filter(COUNTRY == "BRA"))
#>                      B: Placebo     A: Drug X    C: Combination   All Patients
#>                         (N=7)        (N=13)          (N=10)          (N=30)   
#> ——————————————————————————————————————————————————————————————————————————————
#> Age (yr)                                                                      
#>   n                       7            13              10              30     
#>   Mean (SD)          32.0 (6.1)    36.7 (6.4)     38.3 (10.6)      36.1 (8.1) 
#>   Median                32.0          37.0            35.0            35.5    
#>   Min - Max          25.0 - 42.0   24.0 - 47.0    25.0 - 64.0     24.0 - 64.0 
#> Sex                                                                           
#>   n                       7            13              10              30     
#>   M                   4 (57.1%)     8 (61.5%)       5 (50%)        17 (56.7%) 
#>   F                   3 (42.9%)     5 (38.5%)       5 (50%)        13 (43.3%) 
#>   U                       0             0              0               0      
#>   UNDIFFERENTIATED        0             0              0               0

build_table(lyt, df = adsl %>% dplyr::filter(COUNTRY == "CHN"))
#>                      B: Placebo     A: Drug X    C: Combination   All Patients
#>                        (N=81)        (N=74)          (N=64)         (N=219)   
#> ——————————————————————————————————————————————————————————————————————————————
#> Age (yr)                                                                      
#>   n                      81            74              64             219     
#>   Mean (SD)          35.7 (7.3)    33.0 (6.4)      35.2 (6.4)      34.6 (6.8) 
#>   Median                36.0          32.0            35.0            34.0    
#>   Min - Max          21.0 - 58.0   23.0 - 48.0    21.0 - 49.0     21.0 - 58.0 
#> Sex                                                                           
#>   n                      81            74              64             219     
#>   M                  35 (43.2%)    27 (36.5%)      30 (46.9%)       92 (42%)  
#>   F                  45 (55.6%)    44 (59.5%)      29 (45.3%)     118 (53.9%) 
#>   U                   1 (1.2%)      2 (2.7%)        3 (4.7%)        6 (2.7%)  
#>   UNDIFFERENTIATED        0         1 (1.4%)        2 (3.1%)        3 (1.4%)
```

### Adverse Event Table

The standard table of adverse events is a summary by system organ class
and preferred term. For frequency counts by preferred term, if there are
multiple occurrences of the same AE in an individual we count them only
once.

To create this table we will need to use a combination of several layout
creating functions in a tabulation pipeline.

We start by creating the high-level summary. The layout creating
function in `tern` that can do this is
[`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md):

``` r
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )
#>                                                  A: Drug X    B: Placebo    C: Combination   All Patients
#>                                                   (N=134)       (N=134)        (N=132)         (N=400)   
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one AE   122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
#> Overall total number of events                      609           622            703             1934
```

Note that for this table, the denominator used for percentages and shown
in the header of the table `(N = xx)` is defined based on the
subject-level dataset `adsl`. This is done by using the `alt_df_counts`
argument in
[`build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html),
which provides an alternative data set for deriving the counts in the
header. This is often required when we work with data sets that include
multiple records per patient as `df`, such as `adae` here.

#### Statistics Functions

Before building out the rest of the AE table it is helpful to introduce
some more `tern` package design conventions. Each layout creating
function in `tern` is a wrapper for a Statistics function. Statistics
functions are the ones that do the actual computation of numbers in a
table. These functions always return named lists whose elements are the
statistics available to include in a layout via the `.stats` argument at
the layout creating function level.

Statistics functions follow a naming convention to always begin with
`s_*` and for ease of use are documented on the same page as their
layout creating function counterpart. It is helpful to review a
Statistic function to understand the logic used to calculate the numbers
in a table and see what options may be available to modify the analysis.

For example, the Statistics function calculating the numbers in
[`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md)
is
[`s_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md).
The results of this Statistics function is a list with the elements
`unique`, `nonunique` and `unique_count`:

``` r
s_num_patients(x = adae$USUBJID, labelstr = "", .N_col = nrow(adae))
#> $unique
#> [1] 365.000000   0.188728
#> attr(,"label")
#> [1] ""
#> 
#> $nonunique
#> [1] 1934
#> attr(,"label")
#> [1] ""
#> 
#> $unique_count
#> [1] 365
#> attr(,"label")
#> [1] "(n)"
```

From these results you can see that the `unique` and `nonunique`
statistics are those displayed in the “All Patients” column in the
initial AE table output above. Also you can see that these are raw
numbers and are not formatted in any way. All formatting functionality
is handled at the layout creating function level with the `.formats`
argument.

Now that we know what types of statistics can be derived by
[`s_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md),
we can try modifying the default layout returned by
[`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md).
Instead of reporting the `unique` and `nonqunie` statistics, we specify
that the analysis should include only the `unique_count` statistic. The
result will show only the counts of unique patients. Note we make this
update in both the `.stats` and `.labels` argument of
[`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md).

``` r
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique_count",
    .labels = c(unique_count = "Total number of patients with at least one AE")
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )
#>                                                 A: Drug X   B: Placebo   C: Combination   All Patients
#>                                                  (N=134)     (N=134)        (N=132)         (N=400)   
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one AE      122         123            120             365
```

Let’s now continue building on the layout for the adverse event table.

After we have the top-level summary, we can repeat the same summary at
each system organ class level. To do this we split the analysis data
with
[`split_rows_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
before calling again
[`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md).

``` r
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    indent_mod = -1L,
    split_fun = drop_split_levels
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )
#>                                                    A: Drug X    B: Placebo    C: Combination   All Patients
#>                                                     (N=134)       (N=134)        (N=132)         (N=400)   
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one AE     122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
#> Overall total number of events                        609           622            703             1934    
#> cl A.1                                                                                                     
#>   Total number of patients with at least one AE   78 (58.2%)    75 (56.0%)      89 (67.4%)     242 (60.5%) 
#>   Overall total number of events                      132           130            160             422     
#> cl B.1                                                                                                     
#>   Total number of patients with at least one AE   47 (35.1%)    49 (36.6%)      43 (32.6%)     139 (34.8%) 
#>   Overall total number of events                      56            60              62             178     
#> cl B.2                                                                                                     
#>   Total number of patients with at least one AE   79 (59.0%)    74 (55.2%)      85 (64.4%)     238 (59.5%) 
#>   Overall total number of events                      129           138            143             410     
#> cl C.1                                                                                                     
#>   Total number of patients with at least one AE   43 (32.1%)    46 (34.3%)      43 (32.6%)     132 (33.0%) 
#>   Overall total number of events                      55            63              64             182     
#> cl C.2                                                                                                     
#>   Total number of patients with at least one AE   35 (26.1%)    48 (35.8%)      55 (41.7%)     138 (34.5%) 
#>   Overall total number of events                      48            53              65             166     
#> cl D.1                                                                                                     
#>   Total number of patients with at least one AE   79 (59.0%)    67 (50.0%)      80 (60.6%)     226 (56.5%) 
#>   Overall total number of events                      127           106            135             368     
#> cl D.2                                                                                                     
#>   Total number of patients with at least one AE   47 (35.1%)    58 (43.3%)      57 (43.2%)     162 (40.5%) 
#>   Overall total number of events                      62            72              74             208
```

The table looks almost ready. For the final step, we need a layout
creating function that can produce a count table of event frequencies.
The layout creating function for this is
[`count_occurrences()`](https://insightsengineering.github.io/tern/reference/count_occurrences.md).
Let’s first try using this function in a simpler layout without row
splits:

``` r
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences(vars = "AEDECOD") %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )
#>                 A: Drug X    B: Placebo   C: Combination   All Patients
#>                  (N=134)      (N=134)        (N=132)         (N=400)   
#> ———————————————————————————————————————————————————————————————————————
#> dcd A.1.1.1.1   50 (37.3%)   45 (33.6%)     63 (47.7%)     158 (39.5%) 
#> dcd A.1.1.1.2   48 (35.8%)   48 (35.8%)     50 (37.9%)     146 (36.5%) 
#> dcd B.1.1.1.1   47 (35.1%)   49 (36.6%)     43 (32.6%)     139 (34.8%) 
#> dcd B.2.1.2.1   49 (36.6%)   44 (32.8%)     52 (39.4%)     145 (36.2%) 
#> dcd B.2.2.3.1   48 (35.8%)   54 (40.3%)     51 (38.6%)     153 (38.2%) 
#> dcd C.1.1.1.3   43 (32.1%)   46 (34.3%)     43 (32.6%)     132 (33.0%) 
#> dcd C.2.1.2.1   35 (26.1%)   48 (35.8%)     55 (41.7%)     138 (34.5%) 
#> dcd D.1.1.1.1   50 (37.3%)   42 (31.3%)     51 (38.6%)     143 (35.8%) 
#> dcd D.1.1.4.2   48 (35.8%)   42 (31.3%)     50 (37.9%)     140 (35.0%) 
#> dcd D.2.1.5.3   47 (35.1%)   58 (43.3%)     57 (43.2%)     162 (40.5%)
```

Putting everything together, the final AE table looks like this:

``` r
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    indent_mod = -1L,
    split_fun = drop_split_levels
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD") %>%
  build_table(
    df = adae,
    alt_counts_df = adsl
  )
#>                                                    A: Drug X    B: Placebo    C: Combination   All Patients
#>                                                     (N=134)       (N=134)        (N=132)         (N=400)   
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one AE     122 (91.0%)   123 (91.8%)    120 (90.9%)     365 (91.2%) 
#> Overall total number of events                        609           622            703             1934    
#> cl A.1                                                                                                     
#>   Total number of patients with at least one AE   78 (58.2%)    75 (56.0%)      89 (67.4%)     242 (60.5%) 
#>   Overall total number of events                      132           130            160             422     
#>     dcd A.1.1.1.1                                 50 (37.3%)    45 (33.6%)      63 (47.7%)     158 (39.5%) 
#>     dcd A.1.1.1.2                                 48 (35.8%)    48 (35.8%)      50 (37.9%)     146 (36.5%) 
#> cl B.1                                                                                                     
#>   Total number of patients with at least one AE   47 (35.1%)    49 (36.6%)      43 (32.6%)     139 (34.8%) 
#>   Overall total number of events                      56            60              62             178     
#>     dcd B.1.1.1.1                                 47 (35.1%)    49 (36.6%)      43 (32.6%)     139 (34.8%) 
#> cl B.2                                                                                                     
#>   Total number of patients with at least one AE   79 (59.0%)    74 (55.2%)      85 (64.4%)     238 (59.5%) 
#>   Overall total number of events                      129           138            143             410     
#>     dcd B.2.1.2.1                                 49 (36.6%)    44 (32.8%)      52 (39.4%)     145 (36.2%) 
#>     dcd B.2.2.3.1                                 48 (35.8%)    54 (40.3%)      51 (38.6%)     153 (38.2%) 
#> cl C.1                                                                                                     
#>   Total number of patients with at least one AE   43 (32.1%)    46 (34.3%)      43 (32.6%)     132 (33.0%) 
#>   Overall total number of events                      55            63              64             182     
#>     dcd C.1.1.1.3                                 43 (32.1%)    46 (34.3%)      43 (32.6%)     132 (33.0%) 
#> cl C.2                                                                                                     
#>   Total number of patients with at least one AE   35 (26.1%)    48 (35.8%)      55 (41.7%)     138 (34.5%) 
#>   Overall total number of events                      48            53              65             166     
#>     dcd C.2.1.2.1                                 35 (26.1%)    48 (35.8%)      55 (41.7%)     138 (34.5%) 
#> cl D.1                                                                                                     
#>   Total number of patients with at least one AE   79 (59.0%)    67 (50.0%)      80 (60.6%)     226 (56.5%) 
#>   Overall total number of events                      127           106            135             368     
#>     dcd D.1.1.1.1                                 50 (37.3%)    42 (31.3%)      51 (38.6%)     143 (35.8%) 
#>     dcd D.1.1.4.2                                 48 (35.8%)    42 (31.3%)      50 (37.9%)     140 (35.0%) 
#> cl D.2                                                                                                     
#>   Total number of patients with at least one AE   47 (35.1%)    58 (43.3%)      57 (43.2%)     162 (40.5%) 
#>   Overall total number of events                      62            72              74             208     
#>     dcd D.2.1.5.3                                 47 (35.1%)    58 (43.3%)      57 (43.2%)     162 (40.5%)
```

### Response Table

A typical response table for a binary clinical trial endpoint may be
composed of several different analyses:

- Proportion of responders in each treatment group
- Difference between proportion of responders in comparison groups
  vs. control group
- Chi-Square test for difference in response rates between comparison
  groups vs. control group

We can build a table layout like this by following the same approach we
used for the AE table: each table section will be produced using a
different layout creating function from `tern`.

First we start with some data preparation steps to set up the analysis
dataset. We select the endpoint to analyze from `PARAMCD` and define the
logical variable `is_rsp` which indicates whether a patient is
classified as a responder or not.

``` r
# Preprocessing to select an analysis endpoint.
anl <- adrs %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR"))
```

To create a summary of the proportion of responders in each treatment
group, use the
[`estimate_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md)
layout creating function:

``` r
basic_table() %>%
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    table_names = "est_prop"
  ) %>%
  build_table(anl)
#>                                   A: Drug X      B: Placebo    C: Combination
#>                                    (N=134)        (N=134)         (N=132)    
#> —————————————————————————————————————————————————————————————————————————————
#> Responders                       114 (85.1%)     90 (67.2%)     120 (90.9%)  
#> 95% CI (Wald, with correction)   (78.7, 91.5)   (58.8, 75.5)    (85.6, 96.2)
```

To specify which arm in the table should be used as the reference, use
the argument `ref_group` from
[`split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html).
Below we change the reference arm to “B: Placebo” and so this arm is
displayed as the first column:

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp"
  ) %>%
  build_table(anl)
#>                                   A: Drug X      B: Placebo    C: Combination
#>                                    (N=134)        (N=134)         (N=132)    
#> —————————————————————————————————————————————————————————————————————————————
#> Responders                       114 (85.1%)     90 (67.2%)     120 (90.9%)  
#> 95% CI (Wald, with correction)   (78.7, 91.5)   (58.8, 75.5)    (85.6, 96.2)
```

To further customize the analysis, we can use the `method` and
`conf_level` arguments to modify the type of confidence interval that is
calculated:

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    method = "clopper-pearson",
    conf_level = 0.9
  ) %>%
  build_table(anl)
#>                             A: Drug X      B: Placebo    C: Combination
#>                              (N=134)        (N=134)         (N=132)    
#> ———————————————————————————————————————————————————————————————————————
#> Responders                 114 (85.1%)     90 (67.2%)     120 (90.9%)  
#> 90% CI (Clopper-Pearson)   (79.1, 89.9)   (59.9, 73.9)    (85.7, 94.7)
```

The next table section needed should summarize the difference in
response rates between the reference arm each comparison arm. Use
[`estimate_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff.md)
layout creating function for this:

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis"
  ) %>%
  build_table(anl)
#>                                       A: Drug X    B: Placebo   C: Combination
#>                                        (N=134)      (N=134)        (N=132)    
#> ——————————————————————————————————————————————————————————————————————————————
#> Unstratified Analysis                                                         
#>   Difference in Response rate (%)       17.9                         23.7     
#>     95% CI (Wald, with correction)   (7.2, 28.6)                 (13.7, 33.8)
```

The final section needed to complete the table includes a statistical
test for the difference in response rates. Use the
[`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md)
layout creating function for this:

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  test_proportion_diff(vars = "is_rsp") %>%
  build_table(anl)
#>                                A: Drug X   B: Placebo   C: Combination
#>                                 (N=134)     (N=134)        (N=132)    
#> ——————————————————————————————————————————————————————————————————————
#>   p-value (Chi-Squared Test)    0.0006                     <0.0001
```

To customize the output, we use the `method` argument to select a
Chi-Squared test with Schouten correction.

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  test_proportion_diff(
    vars = "is_rsp",
    method = "schouten"
  ) %>%
  build_table(anl)
#>                                                         A: Drug X   B: Placebo   C: Combination
#>                                                          (N=134)     (N=134)        (N=132)    
#> ———————————————————————————————————————————————————————————————————————————————————————————————
#>   p-value (Chi-Squared Test with Schouten Correction)    0.0008                     <0.0001
```

Now we can put all the table sections together in one layout pipeline.
Note there is one more small change needed. Since the primary analysis
variable in all table sections is the same (`is_rsp`), we need to give
each sub-table a unique name. This is done by adding the `table_names`
argument and providing unique names through that:

``` r
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    method = "clopper-pearson",
    conf_level = 0.9,
    table_names = "est_prop"
  ) %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis",
    table_names = "est_prop_diff"
  ) %>%
  test_proportion_diff(
    vars = "is_rsp",
    method = "schouten",
    table_names = "test_prop_diff"
  ) %>%
  build_table(anl)
#>                                                          A: Drug X      B: Placebo    C: Combination
#>                                                           (N=134)        (N=134)         (N=132)    
#> ————————————————————————————————————————————————————————————————————————————————————————————————————
#> Responders                                              114 (85.1%)     90 (67.2%)     120 (90.9%)  
#> 90% CI (Clopper-Pearson)                                (79.1, 89.9)   (59.9, 73.9)    (85.7, 94.7) 
#> Unstratified Analysis                                                                               
#>   Difference in Response rate (%)                           17.9                           23.7     
#>     95% CI (Wald, with correction)                      (7.2, 28.6)                    (13.7, 33.8) 
#>   p-value (Chi-Squared Test with Schouten Correction)      0.0008                        <0.0001
```

## Summary

Tabulation with `tern` builds on top of the the layout tabulation
framework from `rtables`. Complex tables are built step by step in a
pipeline by combining layout creating functions that perform a specific
type of analysis.

The `tern` analyze functions introduced in this vignette are:

- [`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
- [`summarize_num_patients()`](https://insightsengineering.github.io/tern/reference/summarize_num_patients.md)
- [`count_occurrences()`](https://insightsengineering.github.io/tern/reference/count_occurrences.md)
- [`estimate_proportion()`](https://insightsengineering.github.io/tern/reference/estimate_proportion.md)
- [`estimate_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff.md)
- [`test_proportion_diff()`](https://insightsengineering.github.io/tern/reference/prop_diff_test.md)

Layout creating functions build a formatted `layout` by controlling
features such as labels, numerical display formats and indentation.
These functions are wrappers for the Statistics functions which
calculate the raw summaries of each analysis. You can easily spot
Statistics functions in the documentation because they always begin with
the prefix `s_`. It can be helpful to inspect and run Statistics
functions to understand ways an analysis can be customized.
