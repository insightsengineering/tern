# h_proportion_df functions as expected with valid input and default arguments

    Code
      res
    Output
        arm n n_rsp prop
      1   B 2     1 0.50
      2   A 4     1 0.25

# h_proportion_df functions as expected when 0 responses in one group

    Code
      res
    Output
        arm n n_rsp prop
      1   A 2     1  0.5
      2   B 2     0  0.0

# h_proportion_df functions when 0 obs in one arm

    Code
      res
    Output
        arm n n_rsp      prop
      1   B 0    NA        NA
      2   A 6     2 0.3333333

# h_proportion_subgroups_df functions as expected with valid input and default arguments

    Code
      res
    Output
                arm  n n_rsp      prop     subgroup     var    var_label row_type
      1  B: Placebo  9     7 0.7777778 All Patients     ALL All Patients  content
      2   A: Drug X 11     8 0.7272727 All Patients     ALL All Patients  content
      3  B: Placebo  5     4 0.8000000            F     SEX          SEX analysis
      4   A: Drug X  6     6 1.0000000            F     SEX          SEX analysis
      5  B: Placebo  4     3 0.7500000            M     SEX          SEX analysis
      6   A: Drug X  5     2 0.4000000            M     SEX          SEX analysis
      7  B: Placebo  5     4 0.8000000           S1 STRATA2      STRATA2 analysis
      8   A: Drug X  5     4 0.8000000           S1 STRATA2      STRATA2 analysis
      9  B: Placebo  4     3 0.7500000           S2 STRATA2      STRATA2 analysis
      10  A: Drug X  6     4 0.6666667           S2 STRATA2      STRATA2 analysis

# h_proportion_subgroups_df functions as expected when subgroups is NULL.

    Code
      res
    Output
               arm  n n_rsp      prop     subgroup var    var_label row_type
      1 B: Placebo  9     7 0.7777778 All Patients ALL All Patients  content
      2  A: Drug X 11     8 0.7272727 All Patients ALL All Patients  content

# h_proportion_subgroups_df works as expected with groups_lists

    Code
      res
    Output
      [1] "low"             "low"             "low/medium"      "low/medium"     
      [5] "low/medium/high" "low/medium/high"

# h_odds_ratio_df functions as expected with valid input and default arguments

    Code
      res
    Output
        arm n_tot or        lcl      ucl conf_level
      1         4  1 0.01984252 50.39681       0.95

# h_odds_ratio_df functions as expected with valid input and non-default arguments

    Code
      res
    Output
        arm n_tot       or       lcl    ucl conf_level      pval
      1       100 2.135417 0.9478486 4.8109        0.9 0.1200954
                        pval_label
      1 p-value (Chi-Squared Test)

# h_odds_ratio_df functions as expected with strata

    Code
      res
    Output
        arm n_tot       or       lcl      ucl conf_level      pval
      1       100 2.072353 0.9091078 4.724022        0.9 0.1411528
                                    pval_label
      1 p-value (Cochran-Mantel-Haenszel Test)

# h_odds_ratio_df functions when 0 obs in one arm

    Code
      res
    Output
        arm n_tot or lcl ucl conf_level pval pval_label
      1         6 NA  NA  NA       0.95   NA         NA

# h_odds_ratio_subgroups_df functions as expected with valid input and default arguments

    Code
      res
    Output
        arm n_tot         or       lcl        ucl conf_level     subgroup     var
      1       100  2.1354167 0.8112648   5.620858       0.95 All Patients     ALL
      2        54 12.6315770 1.4840381 107.515254       0.95            F     SEX
      3        46  0.7142857 0.1889048   2.700853       0.95            M     SEX
      4        51  2.1388889 0.5395114   8.479609       0.95           S1 STRATA2
      5        49  2.1111111 0.5405387   8.245089       0.95           S2 STRATA2
           var_label row_type
      1 All Patients  content
      2          SEX analysis
      3          SEX analysis
      4      STRATA2 analysis
      5      STRATA2 analysis

# h_odds_ratio_subgroups_df functions as expected when subgroups is NULL.

    Code
      res
    Output
        arm n_tot       or       lcl      ucl conf_level     subgroup var
      1       100 2.135417 0.8112648 5.620858       0.95 All Patients ALL
           var_label row_type
      1 All Patients  content

# h_odds_ratio_subgroups_df functions as expected with strata

    Code
      res
    Output
        arm n_tot         or       lcl       ucl conf_level        pval
      1       100  2.0600522 0.7897670  5.373503       0.95 0.135169890
      2        54 11.2344101 1.3407992 94.131894       0.95 0.007819801
      3        46  0.6700931 0.1796710  2.499150       0.95 0.549837487
      4        51  2.2252093 0.5419286  9.136916       0.95 0.260432159
      5        49  1.9373524 0.4946238  7.588260       0.95 0.337843302
                                    pval_label     subgroup     var    var_label
      1 p-value (Cochran-Mantel-Haenszel Test) All Patients     ALL All Patients
      2 p-value (Cochran-Mantel-Haenszel Test)            F     SEX          SEX
      3 p-value (Cochran-Mantel-Haenszel Test)            M     SEX          SEX
      4 p-value (Cochran-Mantel-Haenszel Test)           S1 STRATA2      STRATA2
      5 p-value (Cochran-Mantel-Haenszel Test)           S2 STRATA2      STRATA2
        row_type
      1  content
      2 analysis
      3 analysis
      4 analysis
      5 analysis

# h_odds_ratio_subgroups_df works as expected with groups_lists

    Code
      res
    Output
      [1] "low"             "low/medium"      "low/medium/high"

