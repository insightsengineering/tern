# s_count_abnormal_by_marked works as expected

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Single, not last`
      [1] 0 0
      
      $count_fraction$`Last or replicated`
      [1] 5.00000000 0.07246377
      
      $count_fraction$`Any Abnormality`
      [1] 5.00000000 0.07246377
      
      

---

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Single, not last`
      [1] 0 0
      
      $count_fraction$`Last or replicated`
      [1] 5.00000000 0.07246377
      
      $count_fraction$`Any Abnormality`
      [1] 5.00000000 0.07246377
      
      

# count_abnormal_by_marked works as expected

    Code
      res
    Output
                                ARM A      ARM B      ARM C  
      ———————————————————————————————————————————————————————
      CRP (n)                     69         73         58   
        Low                                                  
          Single, not last        0       1 (1.4%)      0    
          Last or replicated   5 (7.2%)   2 (2.7%)   5 (8.6%)
          Any Abnormality      5 (7.2%)   3 (4.1%)   5 (8.6%)
        High                                                 
          Single, not last        0          0       1 (1.7%)
          Last or replicated   5 (7.2%)   7 (9.6%)   4 (6.9%)
          Any Abnormality      5 (7.2%)   7 (9.6%)   5 (8.6%)

