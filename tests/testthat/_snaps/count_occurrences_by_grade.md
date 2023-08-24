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
      
      

# s_count_occurrences_by_grade sorts grade levels so that 'missing' level appears last

    Code
      res
    Output
      $count_fraction
      $count_fraction$`1`
      [1] 0 0
      
      $count_fraction$`2`
      [1] 2.0 0.2
      
      $count_fraction$`3`
      [1] 2.0 0.2
      
      $count_fraction$`4`
      [1] 2.0 0.2
      
      $count_fraction$`5`
      [1] 0 0
      
      $count_fraction$Missing
      [1] 0 0
      
      

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
      MILD           0       2 (66.7%)    NA  
      MODERATE   1 (33.3%)   1 (33.3%)    NA  
      SEVERE     2 (66.7%)       0        NA  

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
                        A                B       
                      (N=3)            (N=3)     
      ———————————————————————————————————————————
      -Any-       3.00 (100.00%)   3.00 (100.00%)
      Grade 1-2   1.00 (33.33%)    3.00 (100.00%)
      1            0.00 (0.00%)    2.00 (66.67%) 
      2           1.00 (33.33%)    1.00 (33.33%) 
      Grade 3-5   2.00 (66.67%)     0.00 (0.00%) 
      3           2.00 (66.67%)     0.00 (0.00%) 
      4            0.00 (0.00%)     0.00 (0.00%) 
      5            0.00 (0.00%)     0.00 (0.00%) 

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
                         A               B             D     
                      (N=10)          (N=10)         (N=0)   
      ———————————————————————————————————————————————————————
      LOW                                                    
        MILD       0.00 (0.00%)    1.00 (10.00%)   0.00 (NA%)
        MODERATE   0.00 (0.00%)    0.00 (0.00%)    0.00 (NA%)
        SEVERE     2.00 (20.00%)   0.00 (0.00%)    0.00 (NA%)
      HIGH                                                   
        MILD       0.00 (0.00%)    1.00 (10.00%)   0.00 (NA%)
        MODERATE   1.00 (10.00%)   1.00 (10.00%)   0.00 (NA%)
        SEVERE     0.00 (0.00%)    0.00 (0.00%)    0.00 (NA%)

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

