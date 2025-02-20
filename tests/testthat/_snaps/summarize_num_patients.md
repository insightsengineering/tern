# s_num_patients works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients works as expected with empty input

    Code
      res
    Output
      $unique
      [1] 0 0
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 0
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 0
      attr(,"label")
      [1] "(n)"
      

# s_num_patients works as expected with unique_count_suffix = FALSE

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] ""
      

# s_num_patients_content works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# summarize_num_patients works as expected with healthy input

    Code
      res
    Output
                                                       A           B    
                                                     (N=5)       (N=4)  
      ——————————————————————————————————————————————————————————————————
      Number of patients with at least one event   3 (60.0%)   3 (75.0%)
      Number of events                                 4           4    
      (n)                                              3           3    

---

    Code
      res
    Output
                                                       A           B    
                                                     (N=5)       (N=4)  
      ——————————————————————————————————————————————————————————————————
      Number of patients with at least one event   3 (60.0%)   3 (75.0%)

---

    Code
      res
    Output
                           A       B  
                         (N=5)   (N=4)
      ————————————————————————————————
      Number of events     4       4  

---

    Code
      res
    Output
              A       B  
            (N=5)   (N=4)
      ———————————————————
      (n)     3       3  

# s_num_patients count_by works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients count_by with missing works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients count_by with missing case 2 works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 3
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients_content with count_by works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 3
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients_content with count_by case 2 works as expected with healthy input

    Code
      res
    Output
      $unique
      [1] 3.0 0.6
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 3
      attr(,"label")
      [1] "(n)"
      

# s_num_patients_content with count_by trivial cases, identical to without count_by

    Code
      res
    Output
      $unique
      [1] 4.0 0.8
      attr(,"label")
      [1] ""
      
      $nonunique
      [1] 4
      attr(,"label")
      [1] ""
      
      $unique_count
      [1] 4
      attr(,"label")
      [1] "(n)"
      

# summarize_num_patients with count_by works as expected with healthy input

    Code
      res
    Output
                                                       A           B    
                                                     (N=5)       (N=4)  
      ——————————————————————————————————————————————————————————————————
      Number of patients with at least one event   3 (60.0%)   3 (75.0%)
      Number of events                                 3           3    
      (n)                                              3           3    

---

    Code
      res
    Output
                                                       A           B    
                                                     (N=5)       (N=4)  
      ——————————————————————————————————————————————————————————————————
      Number of patients with at least one event   3 (60.0%)   3 (75.0%)

---

    Code
      res
    Output
                           A       B  
                         (N=5)   (N=4)
      ————————————————————————————————
      Number of events     3       3  

---

    Code
      res
    Output
              A       B  
            (N=5)   (N=4)
      ———————————————————
      (n)     3       3  

# summarize_num_patients with count_by different combinations works as expected with healthy input

    Code
      res
    Output
                                                       A           B    
                                                     (N=5)       (N=4)  
      ——————————————————————————————————————————————————————————————————
      Number of patients with at least one event   3 (60.0%)   3 (75.0%)
      Number of events                                 4           3    
      (n)                                              3           3    

# analyze_num_patients works well for pagination

    Code
      res
    Output
                                                         A           B          A+B   
                                                       (N=5)       (N=4)       (N=9)  
      ————————————————————————————————————————————————————————————————————————————————
      Number of patients with at least one event     3 (60.0%)   3 (75.0%)   6 (66.7%)
      Number of events                                   4           4           8    
      e 1.1                                                                           
        Number of patients with at least one event   2 (40.0%)   2 (50.0%)   4 (44.4%)
        Number of events                                 3           3           6    
        11                                           1 (20.0%)   1 (25.0%)   2 (22.2%)
        19                                               0       1 (25.0%)   1 (11.1%)
        10                                           1 (20.0%)       0       1 (11.1%)
        17                                           1 (20.0%)       0       1 (11.1%)
      f 1.1                                                                           
        Number of patients with at least one event   1 (20.0%)   1 (25.0%)   2 (22.2%)
        Number of events                                 1           1           2    
        17                                               0       1 (25.0%)   1 (11.1%)
        15                                           1 (20.0%)       0       1 (11.1%)

