# s_count_abnormal_by_marked works as expected

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Single, not last`
      [1] 0 0
      
      $count_fraction$`Last or replicated`
      [1] 3.00000000 0.04347826
      
      $count_fraction$`Any Abnormality`
      [1] 3.00000000 0.04347826
      
      

---

    Code
      res
    Output
      $count_fraction
      $count_fraction$`Single, not last`
      [1] 1.00000000 0.01449275
      
      $count_fraction$`Last or replicated`
      [1] 5.00000000 0.07246377
      
      $count_fraction$`Any Abnormality`
      [1] 6.00000000 0.08695652
      
      

# count_abnormal_by_marked works as expected

    Code
      res
    Output
                                ARM A      ARM B      ARM C  
      ———————————————————————————————————————————————————————
      CRP (n)                     69         73         58   
        Low                                                  
          Single, not last        0       1 (1.4%)      0    
          Last or replicated   3 (4.3%)   2 (2.7%)   3 (5.2%)
          Any Abnormality      3 (4.3%)   3 (4.1%)   3 (5.2%)
        High                                                 
          Single, not last     1 (1.4%)      0          0    
          Last or replicated   5 (7.2%)   5 (6.8%)   4 (6.9%)
          Any Abnormality      6 (8.7%)   5 (6.8%)   4 (6.9%)

