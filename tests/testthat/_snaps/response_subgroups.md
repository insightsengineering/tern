# extract_rsp_subgroups functions as expected with valid input and default arguments

    Code
      res
    Output
      $prop
                arm  n n_rsp      prop     subgroup     var               var_label
      1  B: Placebo 51    36 0.7058824 All Patients     ALL            All Patients
      2   A: Drug X 49    41 0.8367347 All Patients     ALL            All Patients
      3  B: Placebo 29    19 0.6551724            F     SEX                     Sex
      4   A: Drug X 25    24 0.9600000            F     SEX                     Sex
      5  B: Placebo 22    17 0.7727273            M     SEX                     Sex
      6   A: Drug X 24    17 0.7083333            M     SEX                     Sex
      7  B: Placebo 25    18 0.7200000           S1 STRATA2 Stratification Factor 2
      8   A: Drug X 26    22 0.8461538           S1 STRATA2 Stratification Factor 2
      9  B: Placebo 26    18 0.6923077           S2 STRATA2 Stratification Factor 2
      10  A: Drug X 23    19 0.8260870           S2 STRATA2 Stratification Factor 2
         row_type
      1   content
      2   content
      3  analysis
      4  analysis
      5  analysis
      6  analysis
      7  analysis
      8  analysis
      9  analysis
      10 analysis
      
      $or
        arm n_tot         or       lcl        ucl conf_level     subgroup     var
      1       100  2.1354167 0.8112648   5.620858       0.95 All Patients     ALL
      2        54 12.6315770 1.4840381 107.515254       0.95            F     SEX
      3        46  0.7142857 0.1889048   2.700853       0.95            M     SEX
      4        51  2.1388889 0.5395114   8.479609       0.95           S1 STRATA2
      5        49  2.1111111 0.5405387   8.245089       0.95           S2 STRATA2
                      var_label row_type
      1            All Patients  content
      2                     Sex analysis
      3                     Sex analysis
      4 Stratification Factor 2 analysis
      5 Stratification Factor 2 analysis
      

# extract_rsp_subgroups functions as expected with NULL subgroups

    Code
      res
    Output
      $prop
               arm  n n_rsp      prop     subgroup var    var_label row_type
      1 B: Placebo 51    36 0.7058824 All Patients ALL All Patients  content
      2  A: Drug X 49    41 0.8367347 All Patients ALL All Patients  content
      
      $or
        arm n_tot       or       lcl      ucl conf_level     subgroup var
      1       100 2.135417 0.8112648 5.620858       0.95 All Patients ALL
           var_label row_type
      1 All Patients  content
      

# extract_rsp_subgroups works as expected with groups_lists

    Code
      res
    Output
      [1] "low"             "low"             "low/medium"      "low/medium"     
      [5] "low/medium/high" "low/medium/high"

---

    Code
      res
    Output
      [1] "low"             "low/medium"      "low/medium/high"

# extract_rsp_subgroups functions as expected with strata

    Code
      res
    Output
      $prop
                arm  n n_rsp      prop subgroup     var               var_label
      1  B: Placebo 51    36 0.7058824      ALL     ALL                     ALL
      2   A: Drug X 49    41 0.8367347      ALL     ALL                     ALL
      3  B: Placebo 29    19 0.6551724        F     SEX                     Sex
      4   A: Drug X 25    24 0.9600000        F     SEX                     Sex
      5  B: Placebo 22    17 0.7727273        M     SEX                     Sex
      6   A: Drug X 24    17 0.7083333        M     SEX                     Sex
      7  B: Placebo 25    18 0.7200000       S1 STRATA2 Stratification Factor 2
      8   A: Drug X 26    22 0.8461538       S1 STRATA2 Stratification Factor 2
      9  B: Placebo 26    18 0.6923077       S2 STRATA2 Stratification Factor 2
      10  A: Drug X 23    19 0.8260870       S2 STRATA2 Stratification Factor 2
         row_type
      1   content
      2   content
      3  analysis
      4  analysis
      5  analysis
      6  analysis
      7  analysis
      8  analysis
      9  analysis
      10 analysis
      
      $or
        arm n_tot         or       lcl       ucl conf_level        pval
      1       100  2.0600522 0.9213863  4.605902        0.9 0.135169890
      2        54 11.2344101 1.8870687 66.882551        0.9 0.007819801
      3        46  0.6700931 0.2220166  2.022483        0.9 0.549837487
      4        51  2.2252093 0.6800882  7.280756        0.9 0.260432159
      5        49  1.9373524 0.6160317  6.092761        0.9 0.337843302
                                    pval_label subgroup     var
      1 p-value (Cochran-Mantel-Haenszel Test)      ALL     ALL
      2 p-value (Cochran-Mantel-Haenszel Test)        F     SEX
      3 p-value (Cochran-Mantel-Haenszel Test)        M     SEX
      4 p-value (Cochran-Mantel-Haenszel Test)       S1 STRATA2
      5 p-value (Cochran-Mantel-Haenszel Test)       S2 STRATA2
                      var_label row_type
      1                     ALL  content
      2                     Sex analysis
      3                     Sex analysis
      4 Stratification Factor 2 analysis
      5 Stratification Factor 2 analysis
      

# a_response_subgroups functions as expected with valid input

    Code
      res
    Output
          prop    pval  
      ——————————————————
      M   0.12   <0.0001
      F   0.57   0.9838 

# tabulate_rsp_subgroups functions as expected with valid input

    Code
      res
    Output
                                             B: Placebo           A: Drug X                                                              
      Baseline Risk Factors     Total n   n    Response (%)   n    Response (%)   Odds Ratio      95% CI       p-value (Chi-Squared Test)
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                142     73      68.5%       69      85.5%          2.71      (1.18, 6.24)              0.0164          
      Sex                                                                                                                                
        F                         78      40      62.5%       38      94.7%         10.80      (2.27, 51.45)             0.0006          
        M                         64      33      75.8%       31      74.2%          0.92      (0.30, 2.85)              0.8852          
      Stratification Factor 2                                                                                                            
        S1                        73      34      70.6%       39      87.2%          2.83      (0.86, 9.35)              0.0801          
        S2                        69      39      66.7%       30      83.3%          2.50      (0.78, 8.04)              0.1181          

# tabulate_rsp_subgroups correctly calculates column indices

    Code
      res
    Output
      $col_x
      [1] 6
      
      $col_ci
      [1] 7
      
      $col_symbol_size
      [1] 1
      

---

    Code
      res
    Output
      $col_x
      [1] 1
      
      $col_ci
      [1] 3
      
      $col_symbol_size
      [1] 2
      

# tabulate_rsp_subgroups functions as expected with valid input extreme values in OR table

    Code
      res
    Output
                                               REF                COMP                                      
      Baseline Risk Factors   Total n   n    Response (%)   n    Response (%)   Odds Ratio       95% CI     
      ——————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients              62      40      82.5%       22       9.1%          0.02       (<0.01, 0.11) 
      var1                                                                                                  
        subgroup1               50      30      100.0%      20       0.0%         <0.01      (0.00, >999.99)
        subgroup2               12      10      30.0%       2       100.0%       >999.99     (0.00, >999.99)

# tabulate_rsp_subgroups functions as expected with NULL subgroups

    Code
      res
    Output
                                           B: Placebo           A: Drug X                                                             
      Baseline Risk Factors   Total n   n    Response (%)   n    Response (%)   Odds Ratio      95% CI      p-value (Chi-Squared Test)
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients              142     73      68.5%       69      85.5%          2.71      (1.18, 6.24)             0.0164          

# tabulate_rsp_subgroups functions as expected when 0 obs in one arm

    Code
      res
    Output
                                                                 B: Placebo           A: Drug X                                                                
      Baseline Risk Factors                         Total n   n    Response (%)   n    Response (%)   Odds Ratio       95% CI        p-value (Chi-Squared Test)
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                                    142     73      68.5%       69      85.5%          2.71       (1.18, 6.24)               0.0164          
      Race                                                                                                                                                     
        ASIAN                                         81      43      69.8%       38      92.1%          5.06       (1.31, 19.44)              0.0117          
        BLACK OR AFRICAN AMERICAN                     28      13      69.2%       15      73.3%          1.22       (0.24, 6.31)               0.8106          
        WHITE                                         23      12      75.0%       11      72.7%          0.89       (0.14, 5.72)               0.9013          
        AMERICAN INDIAN OR ALASKA NATIVE               7      3        0.0%       4       100.0%       >999.99     (0.00, >999.99)             0.0082          
        MULTIPLE                                       2      1       100.0%      1       100.0%         1.00      (0.00, >999.99)             1.0000          
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER      1      1       100.0%      0         NA            NA             NA                      NA            

# d_rsp_subgroups_colvars functions as expected with valid input

    Code
      res
    Output
      $vars
      [1] "n"     "n_rsp" "prop"  "n_tot" "or"    "lcl"   "pval" 
      
      $labels
                                 n                        n_rsp 
                               "n"                 "Responders" 
                              prop                        n_tot 
                    "Response (%)"                    "Total n" 
                                or                           ci 
                      "Odds Ratio"                     "90% CI" 
                              pval 
      "p-value (Chi-Squared Test)" 
      

# tabulate_rsp_subgroups na_str argument works as expected

    Code
      res
    Output
                                             B: Placebo           A: Drug X                                                              
      Baseline Risk Factors     Total n   n    Response (%)   n    Response (%)   Odds Ratio      95% CI       p-value (Chi-Squared Test)
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                142     73      68.5%       69      85.5%          2.71      (1.18, 6.24)              0.0164          
      Sex                                                                                                                                
        F                         78      40      62.5%       38      94.7%       <No data>    (2.27, 51.45)             0.0006          
        M                         64      33      75.8%       31      74.2%       <No data>    (0.30, 2.85)              0.8852          
      Stratification Factor 2                                                                                                            
        S1                        73      34      70.6%       39      87.2%       <No data>    (0.86, 9.35)              0.0801          
        S2                        69      39      66.7%       30      83.3%       <No data>    (0.78, 8.04)              0.1181          

