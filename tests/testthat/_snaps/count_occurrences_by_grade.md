# h_append_grade_groups works with valid input

    Code
      res
    Output
      $`Any Grade`
      [1] 150
      
      $`Grade 1-2`
      [1] 30
      
      $`1`
      [1] 10
      
      $`2`
      [1] 20
      
      $`Grade 3-4`
      [1] 70
      
      $`3`
      [1] 30
      
      $`4`
      [1] 40
      
      $`5`
      [1] 50
      

# h_append_grade_groups works with valid input with revers order and one-element grade groups

    Code
      res
    Output
      $`Any Grade`
      [1] 150
      
      $`Grade A`
      [1] 50
      
      $`Grade B`
      [1] 70
      
      $`4`
      [1] 40
      
      $`3`
      [1] 30
      
      $`2`
      [1] 20
      
      $`1`
      [1] 10
      

# s_count_occurrences_by_grade works with valid input and default arguments for grade

    Code
      res
    Output
      $count_fraction
      $count_fraction$`1`
      [1] 2.0 0.2
      
      $count_fraction$`2`
      [1] 2.0 0.2
      
      $count_fraction$`3`
      [1] 2.0 0.2
      
      $count_fraction$`4`
      [1] 0 0
      
      $count_fraction$`5`
      [1] 0 0
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`1`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`2`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`3`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`5`
      [1] 0 0
      
      

---

    Code
      res
    Output
      $count_fraction
      $count_fraction$`1`
      [1] 0 0
      
      $count_fraction$`2`
      [1] 0 0
      
      $count_fraction$`3`
      [1] 0 0
      
      $count_fraction$`4`
      [1] 0 0
      
      $count_fraction$`5`
      [1] 0 0
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`1`
      [1] 0 0
      
      $count_fraction_fixed_dp$`2`
      [1] 0 0
      
      $count_fraction_fixed_dp$`3`
      [1] 0 0
      
      $count_fraction_fixed_dp$`4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`5`
      [1] 0 0
      
      

# s_count_occurrences_by_grade sorts grade levels so that 'missing' level appears last

    Code
      res
    Output
      $count_fraction
      $count_fraction$`1`
      [1] 2.0 0.2
      
      $count_fraction$`2`
      [1] 2.0 0.2
      
      $count_fraction$`3`
      [1] 1.0 0.1
      
      $count_fraction$`4`
      [1] 0 0
      
      $count_fraction$`5`
      [1] 0 0
      
      $count_fraction$Missing
      [1] 1.0 0.1
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`1`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`2`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`3`
      [1] 1.0 0.1
      
      $count_fraction_fixed_dp$`4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`5`
      [1] 0 0
      
      $count_fraction_fixed_dp$Missing
      [1] 1.0 0.1
      
      

# s_count_occurrences_by_grade works with valid input for grade grouping

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Any Grade`
      [1] 6.0 0.6
      
      $count_fraction$`Grade 1-2`
      [1] 4.0 0.4
      
      $count_fraction$`1`
      [1] 2.0 0.2
      
      $count_fraction$`2`
      [1] 2.0 0.2
      
      $count_fraction$`Grade 3-4`
      [1] 2.0 0.2
      
      $count_fraction$`3`
      [1] 2.0 0.2
      
      $count_fraction$`4`
      [1] 0 0
      
      $count_fraction$`5`
      [1] 0 0
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Any Grade`
      [1] 6.0 0.6
      
      $count_fraction_fixed_dp$`Grade 1-2`
      [1] 4.0 0.4
      
      $count_fraction_fixed_dp$`1`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`2`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`Grade 3-4`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`3`
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$`4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`5`
      [1] 0 0
      
      

---

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Any Grade`
      [1] 0 0
      
      $count_fraction$`Grade 1-2`
      [1] 0 0
      
      $count_fraction$`1`
      [1] 0 0
      
      $count_fraction$`2`
      [1] 0 0
      
      $count_fraction$`Grade 3-4`
      [1] 0 0
      
      $count_fraction$`3`
      [1] 0 0
      
      $count_fraction$`4`
      [1] 0 0
      
      $count_fraction$`5`
      [1] 0 0
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Any Grade`
      [1] 0 0
      
      $count_fraction_fixed_dp$`Grade 1-2`
      [1] 0 0
      
      $count_fraction_fixed_dp$`1`
      [1] 0 0
      
      $count_fraction_fixed_dp$`2`
      [1] 0 0
      
      $count_fraction_fixed_dp$`Grade 3-4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`3`
      [1] 0 0
      
      $count_fraction_fixed_dp$`4`
      [1] 0 0
      
      $count_fraction_fixed_dp$`5`
      [1] 0 0
      
      

---

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Any Grade`
      [1] 6.0 0.6
      
      $count_fraction$`Grade 1-2`
      [1] 4.0 0.4
      
      $count_fraction$`Grade 3-4`
      [1] 2.0 0.2
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Any Grade`
      [1] 6.0 0.6
      
      $count_fraction_fixed_dp$`Grade 1-2`
      [1] 4.0 0.4
      
      $count_fraction_fixed_dp$`Grade 3-4`
      [1] 2.0 0.2
      
      

# s_count_occurrences_by_grade works with valid input for intensity and custom arguments

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Any Intensity`
      [1] 6.0 0.6
      
      $count_fraction$MILD
      [1] 2.0 0.2
      
      $count_fraction$MODERATE
      [1] 2.0 0.2
      
      $count_fraction$SEVERE
      [1] 2.0 0.2
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Any Intensity`
      [1] 6.0 0.6
      
      $count_fraction_fixed_dp$MILD
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$MODERATE
      [1] 2.0 0.2
      
      $count_fraction_fixed_dp$SEVERE
      [1] 2.0 0.2
      
      

# a_count_occurrences_by_grade works with healthy input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
         row_name formatted_cell indent_mod row_label
      1         1        2 (20%)          0         1
      2         2        2 (20%)          0         2
      3         3        2 (20%)          0         3
      4         4              0          0         4
      5         5              0          0         5
      6         1      2 (20.0%)          0         1
      7         2      2 (20.0%)          0         2
      8         3      2 (20.0%)          0         3
      9         4              0          0         4
      10        5              0          0         5

# a_count_occurrences_by_grade works with custom input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
          row_name formatted_cell indent_mod  row_label
      1   Level: 1        2 (20%)          1   Level: 1
      2      LVL 2        2 (20%)          2      LVL 2
      3 Count of 3        2 (20%)          0 Count of 3
      4  Missing 4         0 (0%)          3  Missing 4
      5          5         0 (0%)          0          5

# count_occurrences_by_grade works with default arguments for intensity

    Code
      res
    Output
                     A           B    
                   (N=3)       (N=3)  
      ————————————————————————————————
      MILD           0       2 (66.7%)
      MODERATE   1 (33.3%)   1 (33.3%)
      SEVERE     2 (66.7%)       0    

---

    Code
      res
    Output
                     A           B         D  
                   (N=3)       (N=3)     (N=0)
      ————————————————————————————————————————
      MILD           0       2 (66.7%)     0  
      MODERATE   1 (33.3%)   1 (33.3%)     0  
      SEVERE     2 (66.7%)       0         0  

# count_occurrences_by_grade label works when more than one variables are analyzed

    Code
      res
    Output
                           A           B    
                         (N=3)       (N=3)  
      ——————————————————————————————————————
      AESEV                                 
        MILD               0       2 (66.7%)
        MODERATE       1 (33.3%)   1 (33.3%)
        SEVERE         2 (66.7%)       0    
      Toxicity Grade                        
        1                  0       2 (66.7%)
        2              1 (33.3%)   1 (33.3%)
        3              2 (66.7%)       0    
        4                  0           0    
        5                  0           0    

# count_occurrences_by_grade works with custom arguments for grade

    Code
      res
    Output
                      A           B    
                    (N=3)       (N=3)  
      —————————————————————————————————
      -Any-       3 (100%)    3 (100%) 
      Grade 1-2   1 (33.3%)   3 (100%) 
      1               0       2 (66.7%)
      2           1 (33.3%)   1 (33.3%)
      Grade 3-5   2 (66.7%)       0    
      3           2 (66.7%)       0    
      4               0           0    
      5               0           0    

---

    Code
      res
    Output
                      A          B    
                    (N=3)      (N=3)  
      ————————————————————————————————
      -Any-       3 (100%)    3 (100%)
      Grade 1-2   1 (33.3%)   3 (100%)
      Grade 3-5   2 (66.7%)      0    

# summarize_occurrences_by_grade works with default arguments for intensity

    Code
      res
    Output
                      all obs   
                       (N=9)    
      ——————————————————————————
      A                         
        MILD       0.00 (0.00%) 
        MODERATE   1.00 (11.11%)
        SEVERE     2.00 (22.22%)
      B                         
        MILD       2.00 (22.22%)
        MODERATE   1.00 (11.11%)
        SEVERE     0.00 (0.00%) 

---

    Code
      res
    Output
                         A               B              D      
                      (N=10)          (N=10)          (N=0)    
      —————————————————————————————————————————————————————————
      LOW                                                      
        MILD       0.00 (0.00%)    1.00 (10.00%)   0.00 (0.00%)
        MODERATE   0.00 (0.00%)    0.00 (0.00%)    0.00 (0.00%)
        SEVERE     2.00 (20.00%)   0.00 (0.00%)    0.00 (0.00%)
      HIGH                                                     
        MILD       0.00 (0.00%)    1.00 (10.00%)   0.00 (0.00%)
        MODERATE   1.00 (10.00%)   1.00 (10.00%)   0.00 (0.00%)
        SEVERE     0.00 (0.00%)    0.00 (0.00%)    0.00 (0.00%)

# summarize_occurrences_by_grade works with custom arguments for grade

    Code
      res
    Output
                     all obs 
                     (N=10)  
      ———————————————————————
      A                      
        -Any-       3 (30.0%)
        Grade 1-2   1 (10.0%)
        1               0    
        2           1 (10.0%)
        Grade 3-5   2 (20.0%)
        3           2 (20.0%)
        4               0    
        5               0    
      B                      
        -Any-       3 (30.0%)
        Grade 1-2   3 (30.0%)
        1           2 (20.0%)
        2           1 (10.0%)
        Grade 3-5       0    
        3               0    
        4               0    
        5               0    

# count_occurrences_by_grade works with trim_levels_in_group split function

    Code
      res
    Output
                      ARM A        ARM B  
                      (N=15)      (N=15)  
      ————————————————————————————————————
      SOC1                                
        -Any-       15 (100%)    15 (100%)
        Grade 1-2   15 (100%)    15 (100%)
        1           10 (66.7%)       0    
        2           5 (33.3%)    15 (100%)

# count_occurrences_by_grade works as expected with risk difference column

    Code
      res
    Output
                 A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                  (N=202)      (N=177)        (N=162)                 (N=379)           
      ——————————————————————————————————————————————————————————————————————————————————
      MILD        6 (3.0%)     4 (2.3%)       2 (1.2%)            0.7 (-2.5 - 3.9)      
      MODERATE   19 (9.4%)    15 (8.5%)      14 (8.6%)            0.9 (-4.8 - 6.7)      
      SEVERE     34 (16.8%)   38 (21.5%)     32 (19.8%)          -4.6 (-12.6 - 3.3)     

---

    Code
      res
    Output
                   A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                    (N=202)      (N=177)        (N=162)                 (N=379)           
      ————————————————————————————————————————————————————————————————————————————————————
        -Any-      25 (12.4%)   34 (19.2%)     28 (17.3%)          -6.8 (-14.2 - 0.5)     
        MILD        1 (0.5%)     2 (1.1%)          0               -0.6 (-2.5 - 1.2)      
        MODERATE    7 (3.5%)     9 (5.1%)       6 (3.7%)           -1.6 (-5.7 - 2.5)      
        SEVERE     17 (8.4%)    23 (13.0%)     22 (13.6%)          -4.6 (-10.8 - 1.7)     

# count_occurrences_by_grade works with denom argument specified

    Code
      res
    Output
                      A           B    
      —————————————————————————————————
      LOW          2 (100%)   1 (100%) 
        MILD          0       1 (100%) 
        MODERATE      0           0    
        SEVERE     2 (100%)       0    
      HIGH         1 (100%)   2 (100%) 
        MILD          0       1 (50.0%)
        MODERATE   1 (100%)   1 (50.0%)
        SEVERE        0           0    

# summarize_occurrences works as expected with risk difference column

    Code
      res
    Output
                   A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                    (N=202)      (N=177)        (N=162)                 (N=379)           
      ————————————————————————————————————————————————————————————————————————————————————
      F                                                                                   
        MILD        3 (1.5%)     4 (2.3%)       1 (0.6%)           -0.8 (-3.5 - 2.0)      
        MODERATE    8 (4.0%)     6 (3.4%)       6 (3.7%)            0.6 (-3.2 - 4.4)      
        SEVERE     22 (10.9%)   21 (11.9%)     20 (12.3%)          -1.0 (-7.4 - 5.4)      
      M                                                                                   
        MILD        3 (1.5%)        0           1 (0.6%)            1.5 (-0.2 - 3.2)      
        MODERATE   11 (5.4%)     9 (5.1%)       8 (4.9%)            0.4 (-4.1 - 4.9)      
        SEVERE     12 (5.9%)    17 (9.6%)      12 (7.4%)           -3.7 (-9.1 - 1.8)      

---

    Code
      res
    Output
                     A: Drug X   B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                      (N=202)     (N=177)        (N=162)                 (N=379)           
      —————————————————————————————————————————————————————————————————————————————————————
      F                                                                                    
          -Any-      20 (9.9%)   19 (10.7%)     19 (11.7%)          -0.8 (-7.0 - 5.3)      
          MILD       2 (1.0%)     2 (1.1%)          0               -0.1 (-2.2 - 1.9)      
          MODERATE   4 (2.0%)     3 (1.7%)       3 (1.9%)            0.3 (-2.4 - 3.0)      
          SEVERE     14 (6.9%)   14 (7.9%)      16 (9.9%)           -1.0 (-6.3 - 4.3)      
      M                                                                                    
          -Any-      14 (6.9%)   21 (11.9%)     17 (10.5%)          -4.9 (-10.8 - 1.0)     
          MILD       1 (0.5%)        0           1 (0.6%)            0.5 (-0.5 - 1.5)      
          MODERATE   4 (2.0%)     7 (4.0%)       5 (3.1%)           -2.0 (-5.4 - 1.5)      
          SEVERE     9 (4.5%)    14 (7.9%)      11 (6.8%)           -3.5 (-8.3 - 1.4)      

