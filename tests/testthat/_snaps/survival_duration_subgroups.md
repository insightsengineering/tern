# extract_survival_subgroups functions as expected with valid input and default arguments

    Code
      res
    Output
      $survtime
                arm  n n_events    median     subgroup    var
      1  B: Placebo 73       57  727.8043 All Patients    ALL
      2   A: Drug X 69       44  974.6402 All Patients    ALL
      3  B: Placebo 40       31  599.1772            F    SEX
      4   A: Drug X 38       24 1016.2982            F    SEX
      5  B: Placebo 33       26  888.4916            M    SEX
      6   A: Drug X 31       20  974.6402            M    SEX
      7  B: Placebo 24       21  735.4722          LOW BMRKR2
      8   A: Drug X 26       15  974.6402          LOW BMRKR2
      9  B: Placebo 23       14  731.8352       MEDIUM BMRKR2
      10  A: Drug X 26       17  964.2197       MEDIUM BMRKR2
      11 B: Placebo 26       22  654.8245         HIGH BMRKR2
      12  A: Drug X 17       12 1016.2982         HIGH BMRKR2
                            var_label row_type
      1                  All Patients  content
      2                  All Patients  content
      3                           Sex analysis
      4                           Sex analysis
      5                           Sex analysis
      6                           Sex analysis
      7  Continuous Level Biomarker 2 analysis
      8  Continuous Level Biomarker 2 analysis
      9  Continuous Level Biomarker 2 analysis
      10 Continuous Level Biomarker 2 analysis
      11 Continuous Level Biomarker 2 analysis
      12 Continuous Level Biomarker 2 analysis
      
      $hr
        arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
      1       142          101 0.7108557 0.4779138 1.0573368       0.95 0.09049511
      2        78           55 0.5595391 0.3246658 0.9643271       0.95 0.03411759
      3        64           46 0.9102874 0.5032732 1.6464678       0.95 0.75582028
      4        50           36 0.7617717 0.3854349 1.5055617       0.95 0.43236030
      5        49           31 0.7651261 0.3641277 1.6077269       0.95 0.47860004
      6        43           34 0.6662356 0.3257413 1.3626456       0.95 0.26285846
                pval_label     subgroup    var                    var_label row_type
      1 p-value (log-rank) All Patients    ALL                 All Patients  content
      2 p-value (log-rank)            F    SEX                          Sex analysis
      3 p-value (log-rank)            M    SEX                          Sex analysis
      4 p-value (log-rank)          LOW BMRKR2 Continuous Level Biomarker 2 analysis
      5 p-value (log-rank)       MEDIUM BMRKR2 Continuous Level Biomarker 2 analysis
      6 p-value (log-rank)         HIGH BMRKR2 Continuous Level Biomarker 2 analysis
      

# extract_survival_subgroups works as expected with groups_lists

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

# extract_survival_subgroups functions as expected with NULL subgroups

    Code
      res
    Output
      $survtime
               arm  n n_events   median     subgroup var    var_label row_type
      1 B: Placebo 73       57 727.8043 All Patients ALL All Patients  content
      2  A: Drug X 69       44 974.6402 All Patients ALL All Patients  content
      
      $hr
        arm n_tot n_tot_events        hr       lcl      ucl conf_level       pval
      1       142          101 0.7108557 0.4779138 1.057337       0.95 0.09049511
                pval_label     subgroup var    var_label row_type
      1 p-value (log-rank) All Patients ALL All Patients  content
      

# a_survival_subgroups functions as expected with valid input

    Code
      res
    Output
           hr     pval  
      ——————————————————
      M   0.12   <0.0001
      F   0.57   1.3023 

# tabulate_survival_subgroups functions as expected with valid input

    Code
      res
    Output
                                                          B: Placebo               A: Drug X                                     
      Baseline Risk Factors          Total Events   Events   Median (DAYS)   Events   Median (DAYS)   Hazard Ratio   95% Wald CI 
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                       101          57         727.8         44         974.6           0.71       (0.48, 1.06)
      Sex                                                                                                                        
        F                                 55          31         599.2         24        1016.3           0.56       (0.32, 0.96)
        M                                 46          26         888.5         20         974.6           0.91       (0.50, 1.65)
      Continuous Level Biomarker 2                                                                                               
        LOW                               36          21         735.5         15         974.6           0.76       (0.39, 1.51)
        MEDIUM                            31          14         731.8         17         964.2           0.77       (0.36, 1.61)
        HIGH                              34          22         654.8         12        1016.3           0.67       (0.33, 1.36)

# tabulate_survival_subgroups functions as expected with NULL subgroups

    Code
      res
    Output
                                                   B: Placebo               A: Drug X                                     
      Baseline Risk Factors   Total Events   Events   Median (DAYS)   Events   Median (DAYS)   Hazard Ratio   95% Wald CI 
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                101          57         727.8         44         974.6           0.71       (0.48, 1.06)

# tabulate_survival_subgroups functions as expected with extreme values in subgroups

    Code
      res
    Output
                                                   B: Placebo               A: Drug X                                     
      Baseline Risk Factors   Total Events   Events   Median (DAYS)   Events   Median (DAYS)   Hazard Ratio   95% Wald CI 
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                 18          10         859.0         8          954.2           0.60       (0.23, 1.59)
      Geographic Region 1                                                                                                 
        Asia                       9           4         1244.7         5          796.9           0.97       (0.26, 3.64)
        North America              2           2         1066.9         NA          NA              NA             NA     
        South America              7           4          859.0         3         1759.9           0.39       (0.07, 2.17)

# tabulate_survival_subgroups functions as expected when one arm has 0 records

    Code
      res
    Output
                                                                           B: Placebo                     A: Drug X                                                               
      Baseline Risk Factors                         Total Events   n    Events   Median (DAYS)   n    Events   Median (DAYS)   Hazard Ratio     95% Wald CI     p-value (log-rank)
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                                      101        73     57         727.8       69     44         974.6           0.71        (0.48, 1.06)           0.0905      
      Race                                                                                                                                                                        
        ASIAN                                            51        43     31         731.8       38     20        1500.8           0.52        (0.29, 0.92)           0.0235      
        BLACK OR AFRICAN AMERICAN                        24        13     11         888.5       15     13         685.2           0.84        (0.35, 1.99)           0.6842      
        WHITE                                            20        12     11         589.4       11     9          796.9           1.15        (0.45, 2.92)           0.7719      
        AMERICAN INDIAN OR ALASKA NATIVE                 3         3      2         2009.3       4      1          373.5         >999.99      (0.00, >999.99)         0.2207      
        MULTIPLE                                         2         1      1          202.4       1      1          660.7          <0.01       (0.00, >999.99)         0.3173      
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER        1         1      1          370.4       0      NA          NA              NA              NA                  NA        

# tabulate_survival_subgroups works correctly with both `n_tot` and `n_tot_events` in `vars`

    Code
      res
    Output
      $col_x
      [1] 1
      
      $col_ci
      [1] 2
      
      $col_symbol_size
      [1] 3
      

---

    Code
      res
    Output
      $col_x
      [1] 9
      
      $col_ci
      [1] 10
      
      $col_symbol_size
      [1] 1
      

---

    Code
      res
    Output
       [1] "Baseline Risk Factors" "Total Events"          "Total n"              
       [4] "Median (DAYS)"         "Events"                "n"                    
       [7] "Median (DAYS)"         "Events"                "n"                    
      [10] "Hazard Ratio"          "95% Wald CI"           "p-value (log-rank)"   

# d_survival_subgroups_colvars functions as expected with valid input

    Code
      res
    Output
      $vars
      [1] "n"            "n_events"     "median"       "n_tot_events" "hr"          
      [6] "lcl"          "pval"        
      
      $labels
                         n             n_events               median 
                       "n"             "Events"    "Median (Months)" 
              n_tot_events                   hr                   ci 
            "Total Events"       "Hazard Ratio"        "90% Wald CI" 
                      pval 
      "p-value (log-rank)" 
      

# tabulate_survival_subgroups na_str argument works as expected

    Code
      res
    Output
                                                          B: Placebo               A: Drug X                                     
      Baseline Risk Factors          Total Events   Events   Median (DAYS)   Events   Median (DAYS)   Hazard Ratio   95% Wald CI 
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                       101          57         727.8         44         974.6           0.71       (0.48, 1.06)
      Sex                                                                                                                        
        F                                 55          31         599.2         24        1016.3        <No data>     (0.32, 0.96)
        M                                 46          26         888.5         20         974.6        <No data>     (0.50, 1.65)
      Continuous Level Biomarker 2                                                                                               
        LOW                               36          21         735.5         15         974.6        <No data>     (0.39, 1.51)
        MEDIUM                            31          14         731.8         17         964.2        <No data>     (0.36, 1.61)
        HIGH                              34          22         654.8         12        1016.3           0.67       (0.33, 1.36)

