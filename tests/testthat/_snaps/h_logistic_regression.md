# h_get_interaction_vars works as expected

    Code
      res
    Output
      [1] "ARMCD" "RACE" 

# h_interaction_coef_name works as expected

    Code
      res
    Output
      [1] "ARMCDARM B:RACEWHITE"

# h_or_cat_interaction works as expected

    Code
      res
    Output
      $or
      ARMCDARM B 
       0.2413265 
      
      $ci
           lcl      ucl 
      0.022360 2.604584 
      

# h_or_cont_interaction works as expected with median increment

    Code
      res
    Output
      NULL

---

    Code
      res
    Output
      [1] "or" "ci"

---

    Code
      res
    Output
      [1] "or" "ci"

---

    Code
      res
    Output
      [1] "or" "ci"

---

    Code
      res
    Output
      $or
      [1] 1.309155
      
      $ci
           lcl      ucl 
      1.002180 1.710157 
      

# h_or_cont_interaction works as expected with custom increments

    Code
      res
    Output
      [1] "25" "30" "40"

---

    Code
      res
    Output
      [1] "25" "30" "40"

# h_simple_term_labels works correctly with factor input

    Code
      res
    Output
      [1] "B, n = 2" "C, n = 1"

# h_interaction_term_labels works correctly with factor input

    Code
      res
    Output
      [1] "B * 2, n = 1" "C * 3, n = 0"

# h_interaction_term_labels works correctly when any term can be fulfilled

    Code
      res
    Output
      [1] "B or 2, n = 3"

# h_glm_simple_term_extract works for factor and numeric variables

    Code
      res
    Output
        variable variable_label                      term
      1     RACE           Race                     ASIAN
      2     RACE           Race BLACK OR AFRICAN AMERICAN
      3     RACE           Race                     WHITE
                               term_label interaction interaction_label reference
      1          Reference ASIAN, n = 110                                        
      2 BLACK OR AFRICAN AMERICAN, n = 40                                        
      3                     WHITE, n = 34                                        
        reference_label   estimate std_error df    pvalue is_variable_summary
      1                                       2 0.7967307                TRUE
      2                   17.92262  4001.705  1 0.9964265               FALSE
      3                 -0.6562766 0.9735107  1 0.5002262               FALSE
        is_term_summary
      1           FALSE
      2            TRUE
      3            TRUE

---

    Code
      res
    Output
        variable variable_label term term_label interaction interaction_label
      1      AGE            Age  AGE        Age                              
        reference reference_label  estimate  std_error df     pvalue
      1                           0.1698216 0.09524116  1 0.07457501
        is_variable_summary is_term_summary
      1               FALSE            TRUE

# h_glm_simple_term_extract can extract continuous variable results from clogit objects

    Code
      res
    Output
                  variable       variable_label                 term 
                     "AGE"                "AGE"                "AGE" 
                term_label          interaction    interaction_label 
                     "AGE"                   ""                   "" 
                 reference      reference_label             estimate 
                        ""                   ""  "0.018435221512945" 
                 std_error                   df               pvalue 
      "0.0242935223263706"                  "1"  "0.447940270007941" 
       is_variable_summary      is_term_summary 
                   "FALSE"               "TRUE" 

# h_glm_interaction_extract works for categorical interaction

    Code
      res
    Output
          variable                         variable_label
      1 ARMCD:RACE Interaction of Planned Arm Code * Race
      2 ARMCD:RACE Interaction of Planned Arm Code * Race
      3 ARMCD:RACE Interaction of Planned Arm Code * Race
      4 ARMCD:RACE Interaction of Planned Arm Code * Race
      5 ARMCD:RACE Interaction of Planned Arm Code * Race
                                     term                                term_label
      1                      ARMCD * RACE         Reference ARM A or ASIAN, n = 136
      2 ARM B * BLACK OR AFRICAN AMERICAN ARM B * BLACK OR AFRICAN AMERICAN, n = 13
      3 ARM C * BLACK OR AFRICAN AMERICAN ARM C * BLACK OR AFRICAN AMERICAN, n = 12
      4                     ARM B * WHITE                     ARM B * WHITE, n = 12
      5                     ARM C * WHITE                     ARM C * WHITE, n = 11
        interaction interaction_label reference reference_label  estimate std_error
      1                                                                            
      2                                                          1.018378  10415.13
      3                                                         -17.92633  11842.72
      4                                                         -18.10507  8366.027
      5                                                         -17.10294  12735.87
        df    pvalue is_variable_summary is_term_summary
      1  4         1                TRUE           FALSE
      2  1  0.999922               FALSE            TRUE
      3  1 0.9987922               FALSE            TRUE
      4  1 0.9982733               FALSE            TRUE
      5  1 0.9989285               FALSE            TRUE

# h_glm_interaction_extract works for continuous interaction

    Code
      res
    Output
         variable                        variable_label  term              term_label
      1 ARMCD:AGE Interaction of Planned Arm Code * Age ARM A Reference ARM A, n = 64
      2 ARMCD:AGE Interaction of Planned Arm Code * Age ARM B           ARM B, n = 68
      3 ARMCD:AGE Interaction of Planned Arm Code * Age ARM C           ARM C, n = 52
        interaction interaction_label reference reference_label   estimate std_error
      1                                                                             
      2                                                          0.3081205 0.2062392
      3                                                         0.02948826  548.5923
        df    pvalue is_variable_summary is_term_summary
      1  2 0.3275837                TRUE           FALSE
      2  1 0.1351767               FALSE            TRUE
      3  1 0.9999571               FALSE            TRUE

# h_logistic_simple_terms works

    Code
      res
    Output
        variable variable_label term           term_label interaction
      1      AGE            Age  AGE                  Age            
      2      SEX            Sex    F Reference F, n = 100            
      3      SEX            Sex    M            M, n = 84            
        interaction_label reference reference_label  estimate  std_error df
      1                                             0.1577267 0.09400483  1
      2                                                                    
      3                                             0.6337791  0.9367545  1
            pvalue is_variable_summary is_term_summary odds_ratio       lcl      ucl
      1 0.09337495               FALSE            TRUE   1.170846 0.9738292 1.407722
      2                           TRUE           FALSE                              
      3  0.4986794               FALSE            TRUE    1.88472 0.3005245  11.8199
                           ci
      1  0.9738292, 1.4077220
      2                      
      3 0.3005245, 11.8198956

---

    Code
      res
    Output
        variable variable_label term           term_label interaction
      1      SEX            Sex    F Reference F, n = 100            
      2      SEX            Sex    M            M, n = 84            
        interaction_label reference reference_label  estimate std_error df    pvalue
      1                                                                             
      2                                             0.6337791 0.9367545  1 0.4986794
        is_variable_summary is_term_summary odds_ratio       lcl      ucl
      1                TRUE           FALSE                              
      2               FALSE            TRUE    1.88472 0.1687832 21.04575
                           ci
      1                      
      2 0.1687832, 21.0457529

# h_logistic_simple_terms can extract continuous variable results from clogit objects

    Code
      res
    Output
                  variable       variable_label                 term 
                     "AGE"                "AGE"                "AGE" 
                term_label          interaction    interaction_label 
                     "AGE"                   ""                   "" 
                 reference      reference_label             estimate 
                        ""                   ""  "0.018435221512945" 
                 std_error                   df               pvalue 
      "0.0242935223263706"                  "1"  "0.447940270007941" 
       is_variable_summary      is_term_summary           odds_ratio 
                   "FALSE"               "TRUE"   "1.01860619926387" 
                       lcl                  ucl                  ci1 
       "0.971242395138309"   "1.06827975629198"  "0.971242395138309" 
                       ci2 
        "1.06827975629198" 

# h_glm_inter_term_extract works as expected with categorical interaction

    Code
      res
    Output
             variable   variable_label  term              term_label interaction
      1         ARMCD Planned Arm Code ARM A Reference ARM A, n = 64            
      2         ARMCD Planned Arm Code ARM B           ARM B, n = 68            
      ARM B1    ARMCD Planned Arm Code ARM B           ARM B, n = 68        RACE
      ARM B2    ARMCD Planned Arm Code ARM B           ARM B, n = 68        RACE
      ARM B3    ARMCD Planned Arm Code ARM B           ARM B, n = 68        RACE
      3         ARMCD Planned Arm Code ARM C           ARM C, n = 52            
      ARM C1    ARMCD Planned Arm Code ARM C           ARM C, n = 52        RACE
      ARM C2    ARMCD Planned Arm Code ARM C           ARM C, n = 52        RACE
      ARM C3    ARMCD Planned Arm Code ARM C           ARM C, n = 52        RACE
             interaction_label                 reference           reference_label
      1                                                                           
      2                                                                           
      ARM B1              Race                     ASIAN                     ASIAN
      ARM B2              Race BLACK OR AFRICAN AMERICAN BLACK OR AFRICAN AMERICAN
      ARM B3              Race                     WHITE                     WHITE
      3                                                                           
      ARM C1              Race                     ASIAN                     ASIAN
      ARM C2              Race BLACK OR AFRICAN AMERICAN BLACK OR AFRICAN AMERICAN
      ARM C3              Race                     WHITE                     WHITE
              estimate std_error   odds_ratio     lcl      ucl df    pvalue
      1                                    NA      NA       NA  2 0.5036187
      2      -1.421604  1.213735           NA      NA       NA  1 0.2414927
      ARM B1        NA        NA 2.413265e-01 0.02236 2.604584 NA        NA
      ARM B2        NA        NA 6.681606e-01 0.00000      Inf NA        NA
      ARM B3        NA        NA 3.308830e-09 0.00000      Inf NA        NA
      3       17.53699  5222.122           NA      NA       NA  1 0.9973205
      ARM C1        NA        NA 4.132557e+07 0.00000      Inf NA        NA
      ARM C2        NA        NA 6.775023e-01 0.00000      Inf NA        NA
      ARM C3        NA        NA 1.543498e+00 0.00000      Inf NA        NA
             is_variable_summary is_term_summary is_reference_summary
      1                     TRUE           FALSE                FALSE
      2                    FALSE            TRUE                FALSE
      ARM B1               FALSE           FALSE                 TRUE
      ARM B2               FALSE           FALSE                 TRUE
      ARM B3               FALSE           FALSE                 TRUE
      3                    FALSE            TRUE                FALSE
      ARM C1               FALSE           FALSE                 TRUE
      ARM C2               FALSE           FALSE                 TRUE
      ARM C3               FALSE           FALSE                 TRUE

# h_logistic_inter_terms works as expected

    Code
      res
    Output
       [1] "AGE"       "ARMCD"     "ARMCD"     "ARMCD"     "ARMCD"     "ARMCD"    
       [7] "ARMCD"     "ARMCD"     "SEX"       "SEX"       "SEX"       "SEX"      
      [13] "SEX"       "ARMCD:SEX" "ARMCD:SEX" "ARMCD:SEX"

---

    Code
      res
    Output
       [1] "Age"                           "Reference ARM A, n = 64"      
       [3] "ARM B, n = 68"                 "ARM B, n = 68"                
       [5] "ARM B, n = 68"                 "ARM C, n = 52"                
       [7] "ARM C, n = 52"                 "ARM C, n = 52"                
       [9] "Reference F, n = 100"          "M, n = 84"                    
      [11] "M, n = 84"                     "M, n = 84"                    
      [13] "M, n = 84"                     "Reference ARM A or F, n = 129"
      [15] "ARM B * M, n = 31"             "ARM C * M, n = 24"            

---

    Code
      res
    Output
       [1] "F"     "M"     "ARM A" "ARM B" "ARM B" "ARM C" "ARM C" "AGE"   "AGE"  
      [10] "AGE"   "AGE"   "ARM A" "ARM B" "ARM C"

---

    Code
      res
    Output
       [1] "Reference F, n = 100"    "M, n = 84"              
       [3] "Reference ARM A, n = 64" "ARM B, n = 68"          
       [5] "ARM B, n = 68"           "ARM C, n = 52"          
       [7] "ARM C, n = 52"           "Age"                    
       [9] "Age"                     "Age"                    
      [11] "Age"                     "Reference ARM A, n = 64"
      [13] "ARM B, n = 68"           "ARM C, n = 52"          

