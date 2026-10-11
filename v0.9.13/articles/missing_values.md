# Missing Values in Tern

The packages used in this vignette are:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`rtables`](https://github.com/pharmaverse/rtables)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`formatters`](https://pharmaverse.github.io/formatters/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tern`](https://pharmaverse.github.io/tern/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`

## Variable Class Conversion

`rtables` requires that split variables to be factors. When you try and
split a variable that isn’t, a warning message will appear. Here we
purposefully convert the SEX variable to character to demonstrate what
happens when we try splitting the rows by this variable. To fix this,
`df_explict_na` will convert this to a factor resulting in the table
being generated.

\
`adsl`` ``<-`` ``tern_ex_adsl`\
`adsl``$``SEX`` ``<-`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``adsl``$``SEX``)`\
\
`vars`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"SEX"``, ``"RACE"``, ``"BMRKR1"``)`\
`var_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Age (yr)"``,`\
`  ``"Sex"``,`\
`  ``"Race"``,`\
`  ``"Continous Level Biomarker 1"`\
`)`\
\
`result`` ``<-`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``show_colcounts ``=`` ``TRUE``)`` ``|>`\
`  `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``var ``=`` ``"ARM"``)`` ``|>`\
`  `[`add_overall_col`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_col.html)`(``"All Patients"``)`` ``|>`\
`  `[`analyze_vars`](https://pharmaverse.github.io/tern/reference/analyze_variables.md)`(`\
`    vars ``=`` ``vars``,`\
`    var_labels ``=`` ``var_labels`\
`  ``)`` ``|>`\
`  `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``adsl``)`\
`result`\
`#>                                                A: Drug X    B: Placebo    C: Combination   All Patients`\
`#>                                                 (N=69)        (N=73)          (N=58)         (N=200)   `\
`#> ———————————————————————————————————————————————————————————————————————————————————————————————————————`\
`#> Age (yr)                                                                                               `\
`#>   n                                               69            73              58             200     `\
`#>   Mean (SD)                                   34.1 (6.8)    35.8 (7.1)      36.1 (7.4)      35.3 (7.1) `\
`#>   Median                                         32.8          35.4            36.2            34.8    `\
`#>   Min - Max                                   22.4 - 48.0   23.3 - 57.5    23.0 - 58.3     22.4 - 58.3 `\
`#> Sex                                                                                                    `\
`#>   n                                               69            73              58             200     `\
`#>   F                                           38 (55.1%)    40 (54.8%)      32 (55.2%)      110 (55%)  `\
`#>   M                                           31 (44.9%)    33 (45.2%)      26 (44.8%)       90 (45%)  `\
`#> Race                                                                                                   `\
`#>   n                                               69            73              58             200     `\
`#>   ASIAN                                       38 (55.1%)    43 (58.9%)       29 (50%)       110 (55%)  `\
`#>   BLACK OR AFRICAN AMERICAN                   15 (21.7%)    13 (17.8%)      12 (20.7%)       40 (20%)  `\
`#>   WHITE                                       11 (15.9%)    12 (16.4%)       11 (19%)        34 (17%)  `\
`#>   AMERICAN INDIAN OR ALASKA NATIVE             4 (5.8%)      3 (4.1%)       6 (10.3%)       13 (6.5%)  `\
`#>   MULTIPLE                                     1 (1.4%)      1 (1.4%)           0             2 (1%)   `\
`#>   NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER        0         1 (1.4%)           0            1 (0.5%)  `\
`#>   OTHER                                            0             0              0               0      `\
`#>   UNKNOWN                                          0             0              0               0      `\
`#> Continous Level Biomarker 1                                                                            `\
`#>   n                                               69            73              58             200     `\
`#>   Mean (SD)                                    6.3 (3.6)     6.7 (3.5)      6.2 (3.3)       6.4 (3.5)  `\
`#>   Median                                          5.4           6.3            5.4             5.6     `\
`#>   Min - Max                                   0.4 - 17.8    1.0 - 18.5      2.4 - 19.1      0.4 - 19.1`

## Including Missing Values in `rtables`

Here we purposefully convert all `M` values to `NA` in the `SEX`
variable. After running `df_explicit_na` the `NA` values are encoded as
`<Missing>` but they are not included in the table. As well, the missing
values are not included in the `n` count and they are not included in
the denominator value for calculating the percent values.

\
`adsl`` ``<-`` ``tern_ex_adsl`\
`adsl``$``SEX``[``adsl``$``SEX`` ``==`` ``"M"``]`` ``<-`` ``NA`\
`adsl`` ``<-`` `[`df_explicit_na`](https://pharmaverse.github.io/tern/reference/df_explicit_na.md)`(``adsl``)`\
\
`vars`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"SEX"``)`\
`var_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Age (yr)"``,`\
`  ``"Sex"`\
`)`\
\
`result`` ``<-`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``show_colcounts ``=`` ``TRUE``)`` ``|>`\
`  `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``var ``=`` ``"ARM"``)`` ``|>`\
`  `[`add_overall_col`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_col.html)`(``"All Patients"``)`` ``|>`\
`  `[`analyze_vars`](https://pharmaverse.github.io/tern/reference/analyze_variables.md)`(`\
`    vars ``=`` ``vars``,`\
`    var_labels ``=`` ``var_labels`\
`  ``)`` ``|>`\
`  `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``adsl``)`\
`result`\
`#>                A: Drug X    B: Placebo    C: Combination   All Patients`\
`#>                 (N=69)        (N=73)          (N=58)         (N=200)   `\
`#> ———————————————————————————————————————————————————————————————————————`\
`#> Age (yr)                                                               `\
`#>   n               69            73              58             200     `\
`#>   Mean (SD)   34.1 (6.8)    35.8 (7.1)      36.1 (7.4)      35.3 (7.1) `\
`#>   Median         32.8          35.4            36.2            34.8    `\
`#>   Min - Max   22.4 - 48.0   23.3 - 57.5    23.0 - 58.3     22.4 - 58.3 `\
`#> Sex                                                                    `\
`#>   n               38            40              32             110     `\
`#>   F            38 (100%)     40 (100%)      32 (100%)       110 (100%) `\
`#>   M                0             0              0               0`

If you want the `Na` values to be displayed in the table and included in
the `n` count and as the denominator for calculating percent values, use
the `na_level` argument.

\
`adsl`` ``<-`` ``tern_ex_adsl`\
`adsl``$``SEX``[``adsl``$``SEX`` ``==`` ``"M"``]`` ``<-`` ``NA`\
`adsl`` ``<-`` `[`df_explicit_na`](https://pharmaverse.github.io/tern/reference/df_explicit_na.md)`(``adsl``, na_level ``=`` ``"Missing Values"``)`\
\
`result`` ``<-`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``show_colcounts ``=`` ``TRUE``)`` ``|>`\
`  `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``var ``=`` ``"ARM"``)`` ``|>`\
`  `[`add_overall_col`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_col.html)`(``"All Patients"``)`` ``|>`\
`  `[`analyze_vars`](https://pharmaverse.github.io/tern/reference/analyze_variables.md)`(`\
`    vars ``=`` ``vars``,`\
`    var_labels ``=`` ``var_labels`\
`  ``)`` ``|>`\
`  `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``adsl``)`\
`result`\
`#>                     A: Drug X    B: Placebo    C: Combination   All Patients`\
`#>                      (N=69)        (N=73)          (N=58)         (N=200)   `\
`#> ————————————————————————————————————————————————————————————————————————————`\
`#> Age (yr)                                                                    `\
`#>   n                    69            73              58             200     `\
`#>   Mean (SD)        34.1 (6.8)    35.8 (7.1)      36.1 (7.4)      35.3 (7.1) `\
`#>   Median              32.8          35.4            36.2            34.8    `\
`#>   Min - Max        22.4 - 48.0   23.3 - 57.5    23.0 - 58.3     22.4 - 58.3 `\
`#> Sex                                                                         `\
`#>   n                    69            73              58             200     `\
`#>   F                38 (55.1%)    40 (54.8%)      32 (55.2%)      110 (55%)  `\
`#>   M                     0             0              0               0      `\
`#>   Missing Values   31 (44.9%)    33 (45.2%)      26 (44.8%)       90 (45%)`

## Missing Values in Numeric Variables

Numeric variables that have missing values are not altered. This means
that any `NA` value in a numeric variable will not be included in the
summary statistics, nor will they be included in the denominator value
for calculating the percent values. Here we make any value less than 30
missing in the `AGE` variable and only the valued greater than 30 are
included in the table below.

\
`adsl`` ``<-`` ``tern_ex_adsl`\
`adsl``$``AGE``[``adsl``$``AGE`` ``<`` ``30``]`` ``<-`` ``NA`\
`adsl`` ``<-`` `[`df_explicit_na`](https://pharmaverse.github.io/tern/reference/df_explicit_na.md)`(``adsl``)`\
\
`vars`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"SEX"``)`\
`var_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Age (yr)"``,`\
`  ``"Sex"`\
`)`\
\
`result`` ``<-`` `[`basic_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)`(``show_colcounts ``=`` ``TRUE``)`` ``|>`\
`  `[`split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)`(``var ``=`` ``"ARM"``)`` ``|>`\
`  `[`add_overall_col`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_col.html)`(``"All Patients"``)`` ``|>`\
`  `[`analyze_vars`](https://pharmaverse.github.io/tern/reference/analyze_variables.md)`(`\
`    vars ``=`` ``vars``,`\
`    var_labels ``=`` ``var_labels`\
`  ``)`` ``|>`\
`  `[`build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)`(``adsl``)`\
`result`\
`#>                A: Drug X    B: Placebo    C: Combination   All Patients`\
`#>                 (N=69)        (N=73)          (N=58)         (N=200)   `\
`#> ———————————————————————————————————————————————————————————————————————`\
`#> Age (yr)                                                               `\
`#>   n               46            56              44             146     `\
`#>   Mean (SD)   37.8 (5.2)    38.3 (6.3)      39.1 (5.9)      38.3 (5.8) `\
`#>   Median         37.2          37.3            37.5            37.5    `\
`#>   Min - Max   30.3 - 48.0   30.0 - 57.5    30.5 - 58.3     30.0 - 58.3 `\
`#> Sex                                                                    `\
`#>   n               69            73              58             200     `\
`#>   F           38 (55.1%)    40 (54.8%)      32 (55.2%)      110 (55%)  `\
`#>   M           31 (44.9%)    33 (45.2%)      26 (44.8%)       90 (45%)`
